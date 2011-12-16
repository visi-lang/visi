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
import Visi.Typer
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
                        (Just msg)-> (1, putStrLn $ "Failed " ++ param ++ " error " ++ msg)
                        _ -> (0, return ())
                            
-- syntaxTests :: ([(String, String -> Either VisiError a, Either VisiError a -> Either String ())])
syntaxTests = 
    [
      ("f n = \n\
       \  v = 33\n\
       \  n + v\n", psuccess 1 . checkparse)

      ,("f n = \n\
       \  v = 33\n\
       \  n + v\n", testTypes [("f", testDoubleFunc)] . checktype)

      ,("f n = \n\
       \  v = 33\n\
       \  n + v\n\
       \res = f 3\n", testResults [("res", DoubleValue 36)] . checkResults)       


      ,("f n = \n\
       \  fact n = if n == 0 then 1 else n * fact(n - 1)\n\
       \  fact n\n\
       \res = f 8\n", testResults [("res", DoubleValue 40320.0)] . checkResults)

      ,("f n = {- Test partially applied functions -}\n\
       \  fact n = if n == 0 then 1 else n * fact(n - 1)\n\
       \  app n fact\n\
       \app v f = f v\n\
       \res = f 8\n", testResults [("res", DoubleValue 40320.0)] . checkResults)


      ,("fact n = n & \"hello\" {-  propper scoping this fact is not the inner fact -}\n\
        \f n = {- Test partially applied functions -}\n\
        \  fact n = if n == 0 then 1 else n * fact(n - 1)\n\
        \  app n fact\n\
        \app v f = f v\n\
        \res = f 8\n", testResults [("res", DoubleValue 40320.0)] . checkResults)

      ,("fact n = n & \"hello\" // propper scoping this fact is not the inner fact \n\
        \f n = {- test that the function closes over local scope -}\n\
        \  fact n = if n == 0 then 1 else n * fact(n - 1)\n\
        \  moo = fact n {- This is the local reference to 'n' -}\n\
        \  moo\n\
        \app v f = f v\n\
        \res = f 7\n", testResults [("res", DoubleValue 5040.0)] . checkResults)

      ,("f b = {- test that the function closes over local scope -}\n\
        \  timesb n m = n * b * m\n\
        \  timesb\n\
        \app v f = f v\n\
        \q = f 8\n\
        \res = app 9 (app 8 q) - ((f 8) 8 9)\n", testResults [("res", DoubleValue 576)] . checkResults)

      ,("a = 1 // simple assignment\n", psuccess 1 . checkparse)

      ,("f n = \n\
        \  33\n", psuccess 1 . checkparse)

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

      ,("f = 3 & \"hi\"", failsTyper . checktype)
      ,("f = 3\n\
        \d = f & \"hi\"", failsTyper . checktype)
      ,("f = 3", testTypes [("f", testPrimDouble)] . checktype)
      ,("f = 3\n\
        \f2 n = f + n", testTypes [("f", testPrimDouble)
                                  ,("f2", testDoubleFunc)] . checktype)
      ,("f = 3\n\
        \f2 = \"Hello\"", testTypes [("f", testPrimDouble)
                                     ,("f2", testPrimStr)] . checktype)

      ,("f n = n + 1", testTypes [("f", testDoubleFunc)] . checktype)
      ,("f n = n & \"hi\"", testTypes [("f", testStrFunc)] . checktype)
      ,("q n = n", testTypes [("q", testGenFunc)] . checktype)

      ,("q n = n\n\
        \f n = if true then n else (q n)", testTypes [("q", testGenFunc)
                                                      ,("f", testGenFunc)] . checktype)

      ,("f n = if true then n else (n + 1)", testTypes [("f", testDoubleFunc)] . checktype)

      ,("f n = if true then n else (f (n + 1))", testTypes [("f", testDoubleFunc)] . checktype)
     
      ,("f n = if true then n else (n + 1)\n\
        \f2 n = if true then n else (n & \"foo\")", testTypes [("f", testDoubleFunc)
                                                              ,("f2", testStrFunc)] . checktype)
     ,("f n = n & \"hi\"\n\
        \q n = n", testTypes [("f", testStrFunc)
                             ,("q", testGenFunc)] . checktype)

     ,("a n = b n\n\
       \b n = c n\n\
       \c n = a n", testTypes [("a", testGenXFunc)
                              ,("b", testGenXFunc)
                              ,("c", testGenXFunc)] . checktype)

     ,("plus41 n = n + 41\n\
       \p41 = plus41\n\
       \f = p41 1\n", testTypes [("f", testPrimDouble)] . checktype)

     ,("a n = n == 1", testTypes [("a", (testT $ tFun (TPrim PrimDouble) (TPrim PrimBool)))] . checktype)

     ,("a n = n == 1\n\
       \b = a true", failsTyper . checktype)

     ,("a n = \"foo\" == 1", failsTyper . checktype)


     ,("a n = b n\n\
       \b n = c n\n\
       \c n = a n\n\
       \d n = a n", testTypes [("a", testGenXFunc)
                              ,("b", testGenXFunc)
                              ,("c", testGenXFunc)
                              ,("d", testGenXFunc)] . checktype)

     ,("fact n = if n == 0 then 1 else n * fact n - 1",
                  testTypes [("fact", testDoubleFunc)] . checktype)

     ,("fact n = if n == 0 then 1 else n * fact n - 1\n\
       \res = fact 10",
                  testResults [("res", DoubleValue 3628800)] . checkResults)

     ,("fact n = if n == 0 then 1 else n * fact n - 1\n\
       \res = fact 10\n\
       \good = goodorbad true\n\
       \bad = goodorbad false\n\
       \goodorbad v = if v then \"good\" else \"bad\"",
                  testResults [("res", DoubleValue 3628800)
                              ,("good", StrValue "good")
                              ,("bad", StrValue "bad")] . checkResults)

     ,("res = \"10\"",
                  testResults [("res", StrValue "10")] . checkResults)


     ,("total = subtotal + tax\n\
       \tax = taxable * taxRate\n\
       \subtotal = taxable + nonTaxable\n\n\n\
       \\"Total\" = total // sink the total\n\
       \\"Tax\" = tax // sink the tax\n\
       \?taxRate // source the tax rate\n\
       \?taxable\n\
       \?nonTaxable", testTypes [("tax", testPrimDouble)
                                ,("taxable", testPrimDouble)] . checktype)

    ]

testGenFunc _ (TOper funcOperName [(TVar t1), (TVar t2)]) | t1 == t2 = Nothing
testGenFunc n t = Just $ "In: "++n++" Expecting a generic function, but got " ++ show t

testGenXFunc _ (TOper funcOperName [(TVar t1), (TVar t2)]) = Nothing
testGenXFunc n t = Just $ "In: "++n++" Expecting a generic function, but got " ++ show t

testT t1 n t2 =
  if t1 == t2 then Nothing else Just $ "In: " ++ n ++ " Expecting: " ++ show t1 ++ " but got " ++ show t2

testPrimDouble = testT $ TPrim PrimDouble

testPrimStr = testT $ TPrim PrimStr

testDoubleFunc = testT $ tFun (TPrim PrimDouble) (TPrim PrimDouble)

testStrFunc = testT $ tFun (TPrim PrimStr) (TPrim PrimStr)

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

failsTyper (Left (TypeError _)) = Nothing
failsTyper (Right types) = Just $ "Expected a type error, but got " ++ show types

-- testTypes :: [(String, String -> Type -> Maybe String)] -> ThrowsError (Map.Map String Type) -> Either String ()
testTypes listOStuff res = 
    case res of 
        (Left err) -> Just $ show err
        (Right typeMap) -> 
          let testIt (funcName, expType) = 
                case Map.lookup funcName typeMap of
                  (Just t) -> expType funcName t
                  _ -> Just $ "Not function "++ funcName ++ " defined"
                in
          let res = List.map testIt listOStuff in
          List.foldr collapseLeft Nothing res

collapseLeft x Nothing = x
collapseLeft Nothing x = x
collapseLeft (Just msg) (Just m2) = Just $ msg ++ ", " ++ m2

checktype str =
    do
        exps <- parseLines str
        let allExp = builtInExp ++ exps
        let grp = mkGroup allExp
        lets <- collectTypes grp
        let typeMap = Map.fromList lets
        return typeMap

testResults list res =
  case res of
    Left err -> Just $ show err
    Right grp ->
      let exprs = buildLetScope grp in
      let testIt (funcName, expVal) = case eval Map.empty exprs $ Var $ FuncName funcName of
                                          v | expVal == v -> Nothing
                                          v -> Just $ "Funcname " ++ funcName ++ " yielded " ++ show v ++ " expected " ++ show expVal
      in
      let res = List.map testIt list in
      List.foldr collapseLeft Nothing res



checkResults str =
  do
    exps <- parseLines str
    let allExp = builtInExp ++ exps
    let grp = mkGroup allExp
    lets <- collectTypes grp
    return grp