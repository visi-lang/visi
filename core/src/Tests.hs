import Char
import List
import Test.QuickCheck
import Text.Printf

{- ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Visi.io.
 *
 * The Initial Developer of the Original Code is
 * David Pollak.
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** -}

import Visi.Util
-- import Visi.Runtime
import Visi.Expression
import Visi.Parse
import Visi.Executor
import Control.Monad.Error

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Text.Parsec.Error

main :: IO ()
main = 
    do
        l1 <- testOMatic syntaxTests
        let allL = l1
        mapM_ (snd) allL
        let errs = foldr (+) 0 $ map fst allL
        putStrLn $ "Ran " ++ (show $ length allL) ++ " tests, " ++ (show errs) ++ " errors"
 
-- testOMatic a b => [(a, a -> b, b)] :: IO ()
testOMatic lst = 
    do 
      l2 <- mapM runTest lst
      return l2
      {-
      let errs = foldr (+) 0 $ map fst l2
      putStrLn $ "Ran " ++ (show $ length lst) ++ " tests, " ++ (show errs) ++ " errors"
      -- putStrLn "Dude"
        -}
runTest (param, func) = 
        do
            let res = func param
            return $ case res of
                        (Just msg)-> (1, putStrLn $ "Failed " ++ (show param) ++ " error " ++ msg)
                        _ -> (0, return ())
                            
-- syntaxTests :: ([(String, String -> Either VisiError a, Either VisiError a -> Either String ())])
syntaxTests = 
    [
      ("a = 1 // simple assignment\n", psuccess 1 . checkparse)
      ,("f a = a + 1 // function definition", psuccess 1 . checkparse)
      ,("f 33 = 44 // constant in parameter position", pfailure . checkparse)
      ,("f a = a {- a multiline example -}\n\
       \f b = b", psuccess 2 . checkparse)
      ,("f a = if a then 3 else 4 {-if/then/else-}", psuccess 1 . checkparse)
      ,("f a b c = f (1 + 2) 3 q w // multiple parameters to a function", psuccess 1 . checkparse)
      ,("add41 v = v + 41", psuccess 1 . checkparse)
      ,("\"Answer\" = add41 1", psuccess 1 . checkparse)
      ,("and = p1 && p2", psuccess 1 . checkparse)
      ,("\"Greeting\" = \"Hello, World!\" // Sink a constant String", psuccess 1 . checkparse)
      ,("\"And\" = p1 && p2\n\
         \?p1\n\
         \?p2", psuccess 3 . checkparse)
      ,("\"Age\" = 2011 - birthYear\n\
         \?birthYear // birthYear infered as Number", psuccess 2 . checkparse)
      ,("{- A big multi-line expression -}\n\
         \total = subtotal + tax\n\
         \tax = taxable * taxRate\n\
         \subtotal = taxable + nonTaxable\n\n\n\
         \\"Total\" = total // sink the total\n\
         \\"Tax\" = tax // sink the tax\n\
         \?taxRate // source the tax rate\n\
         \?taxable\n\
         \?nonTaxable", psuccess 8 . checkparse)
      ,("{- and indented line should fail -}\n\
         \total = subtotal + tax\n\
         \tax = taxable * taxRate\n\
         \subtotal = taxable + nonTaxable\n\n\n\
         \   \"Total\" = total // sink the total\n\
         \\"Tax\" = tax // sink the tax\n\
         \?taxRate // source the tax rate\n\
         \?taxable\n\
         \?nonTaxable", pfailure . checkparse)
         
      ,("f = 3", testTypes [("f", TPrim PrimDouble)] . checktype)
      ,("f = 3\n\
        \f2 n = f + n", testTypes [("f", TPrim PrimDouble)
                                  ,("f2", TFun (TPrim PrimDouble) (TPrim PrimDouble))] . checktype)
      ,("f = 3\n\
        \f2 = \"Hello\"", testTypes [("f", TPrim PrimDouble)
                                     ,("f2", TPrim PrimStr)] . checktype)

      ,("f n = n + 1", testTypes [("f", TFun (TPrim PrimDouble) (TPrim PrimDouble))] . checktype)
      ,("f n = n & \"hi\"", testTypes [("f", TFun (TPrim PrimStr) (TPrim PrimStr))] . checktype)
      ,("q n = n", testTypes [("q", TPrim PrimDouble)] . checktype)
      ,("f n = if true then n else n + 1", testTypes [("f", TFun (TPrim PrimDouble) (TPrim PrimDouble))] . checktype)
      ,("f n = if true then n else f (n + 1)", testTypes [("f", TFun (TPrim PrimDouble) (TPrim PrimDouble))] . checktype)
     
      ,("f n = if true then n else (n + 1)\n\
        \f2 n = if true then n else (n & \"foo\")", testTypes [("f", TFun (TPrim PrimDouble) (TPrim PrimDouble))
                                                              ,("f2", TFun (TPrim PrimStr) (TPrim PrimStr))] . checktype)
     ,("f n = n & \"hi\"\n\
        \q n = n", testTypes [("f", TFun (TPrim PrimStr) (TPrim PrimStr))
                             ,("q", TPrim PrimDouble)] . checktype)
                             
     ,("{- and indented line should fail -}\n\
         \total = subtotal + tax\n\
         \tax = taxable * taxRate\n\
         \subtotal = taxable + nonTaxable\n\n\n\
         \\"Total\" = total // sink the total\n\
         \\"Tax\" = tax // sink the tax\n\
         \?taxRate // source the tax rate\n\
         \?taxable\n\
         \?nonTaxable", testTypes [("tax", TPrim PrimDouble)
                                  ,("taxable", TPrim PrimDouble)] . checktype)                             
    ]

-- | test that the string parses and there are cnt expressions
psuccess cnt p = case p of 
              (Right ar) | (length ar) == cnt -> Nothing
              (Right ar) -> Just $ "Expected " ++ show cnt ++ " but got " ++ (show $ length ar) ++ " expressions"
              (Left msg) -> Just $ show msg

pfailure p = case p of 
              (Left _) -> Nothing
              (Right _) -> Just "Should have failed"

-- checkparse :: (Error e) => String -> e
checkparse str = parseLines str

-- testTypes :: [(String, Type)] -> ThrowsError (Map.Map String Type) -> Either String ()
testTypes listOStuff res = 
    case res of 
        (Left err) -> Just $ show err
        (Right typeMap) -> 
          let testIt (funcName, expType) = 
                case Map.lookup funcName typeMap of
                  (Just t) | t == expType -> Nothing
                  (Just t) -> Just $ "Type Mismatch for "++ funcName ++" expected " ++ show expType ++ " got " ++ show t
                  _ -> Just $ "Not function "++ funcName ++ " defined"
                in
          let res = List.map testIt listOStuff in
          let collapseLeft x Nothing = x
              collapseLeft Nothing x = x
              collecteLeft (Just msg) (Just m2) = Just $ msg ++ ", " ++ m2 in
          List.foldl' collapseLeft Nothing res

checktype str =
    do
        exps <- parseLines str
        let allExp = builtInExp ++ exps
        let grp = mkGroup allExp
        let typeVars = collectVars grp
        (atv, t) <- vtrace ("Analyzing "++str) analyze Map.empty Set.empty typeVars grp
        {-
        (atv, t) <- collectSubs Map.empty typeVars grp
        -}
        (atv', lets) <- vtrace ("Checking:\n" ++ str ++"\n atv " ++ show atv) resolveLets grp atv
        let typeMap = Map.fromList lets
        return typeMap