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
 * Portions created by the Initial Developer are Copyright (C) 2011-2012
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
import Visi.Model
import Control.Monad.Error
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Text.Parsec.Error
import System.Directory
import System.Environment
import System.IO
import Visi.Markdown


main :: IO ()
main = 
    do
        argv <- getArgs
        case argv of
          "--markdown":file -> do
            contents <- readFile $ unwords file
            putStrLn $ markdown contents
          name:_ -> do
                      path <- canonicalizePath $ head argv
                      revisedTests <- mapM (loadSamples path) syntaxTests
                      l1 <- testOMatic revisedTests -- paths
                      let allL = l1
                      mapM_ snd allL
                      let errs = sum $ map fst allL
                      putStrLn $ "Ran " ++ show (length allL) ++ " tests, " ++ show errs ++ " errors"
          _ -> putStrLn "run_test --markdown file | test_dir"
 
loadSamples path (param, func) =
  if ".md" `List.isSuffixOf` param then
    do
      let whole = path ++ "/" ++ param
      contents <- readFile whole
      return (contents, func)
  else
    return (param, func)

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

     ("choose b x y = if b then x else y\n\
       \x = choose true 1 2\n", testTypes [("choose", testT $ tFun (TPrim PrimBool) $ tFun (TVar $ T.pack "synthetic45") $ tFun (TVar  $ T.pack "synthetic45")  (TVar  $ T.pack "synthetic45"))] . checktype)

{-
     ,("choose b x y = if b then x else y\n\
       \choose2 b x y = if b then x else y\n\
       \n = choose2 true true true\n\
       \x = choose true 1 2\n\
       \y = choose false \"hi\" \"dude\"\n", testTypes [("choose", testT $ tFun (TPrim PrimBool) $ tFun (TVar $ T.pack "synthetic45") $ tFun (TVar  $ T.pack "synthetic45")  (TVar  $ T.pack "synthetic45"))] . checktype)

      ,("test_40_struct_parsing.md", psuccess 1 . checkparse)

      ,("test_41_struct_and_more.md", psuccess 6 . checkparse)

      ,("test_05_parse_with_local_var.md", psuccess 1 . checkparse)

      ,("test_05_parse_with_local_var.md", testTypes [("f", testDoubleFunc)] . checktype)

      ,("test_21_call_a_function.md", testResults [("res", DoubleValue 36)] . checkResults)       

      ,("test_22_local_function.md", testResults [("res", DoubleValue 40320.0)] . checkResults)

      ,("test_23_partially_applied_local_func.md", 
          testResults [("res", DoubleValue 40320.0)] . checkResults)


      ,("test_24_proper_scoping.md", testResults [("res", DoubleValue 40320.0)] . checkResults)

      ,("test_25_more_scoping.md", testResults [("res", DoubleValue 5040.0)] . checkResults)

      ,("test_16_complex_local_scope.md", testResults [("res", DoubleValue (-154))] . checkResults)

      ,("test_01_simple_assignment.md", psuccess 1 . checkparse)

      ,("test_02_significant_spaces.md", psuccess 1 . checkparse)

      ,("test_03_literate.md", psuccess 1 . checkparse)


      ,("test_04_multi_literal.md", psuccess 14 . checkparse)        

      ,("test_01_simple_function.md", psuccess 1 . checkparse)
      ,("f 33 = 44 // constant in parameter position", pfailure . checkparse)
      ,("test_02_two_function.md", psuccess 2 . checkparse)
      ,("test_04_if_then_else.md", psuccess 1 . checkparse)
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
      ,("test_15_complex_source_sink.md", psuccess 8 . checkparse)
      ,("/* and indented line should fail */\n\
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
      ,("test_01_simple_assignment.md", testTypes [("a", testPrimDouble)] . checktype)
      ,("test_04_multi_literal.md", testTypes [("res", testPrimDouble)
                                  ,("f", testDoubleFunc)] . checktype)
      ,("test_04_multi_literal.md", testTypes [("res", testPrimDouble)
                                     ,("f2", testPrimStr)] . checktype)

      ,("test_04_multi_literal.md", testTypes [("f3", testStrFunc)
                                              ,("f4", testStrFunc)
                                              ,("id", testGenFunc)
                                              ,("cond", testGenFunc)] . checktype)

      ,("test_04_multi_literal.md", testTypes [("numberFunc", testDoubleFunc)
                                              ,("selfRefNumber", testDoubleFunc)] . checktype)
     
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

     ,("a n = n == 1", testTypes [("a", testT $ tFun (TPrim PrimDouble) (TPrim PrimBool))] . checktype)

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

     ,("a n f = if f n then n else b n f\n\
       \b n f = c n f\n\
       \c n f = if f n then n else a n f\n", testTypes [("a", testGenBoolFunc)] . checktype)                              

     ,("fact n = if n == 0 then 1 else n * fact (n - 1)",
                  testTypes [("fact", testDoubleFunc)] . checktype)

     ,("fact n = if n == 0 then 1 else n * fact (n - 1)\n\
       \res = fact 10",
                  testResults [("res", DoubleValue 3628800)] . checkResults)

     ,("fact n = if n == 0 then 1 else n * fact (n - 1)\n\
       \res = fact 10\n\
       \good = goodorbad true\n\
       \bad = goodorbad false\n\
       \goodorbad v = if v then \"good\" else \"bad\"",
                  testResults [("res", DoubleValue 3628800)
                              ,("good", StrValue $ T.pack "good")
                              ,("bad", StrValue $ T.pack "bad")] . checkResults)

     ,("test_04_multi_literal.md",
                  testResults [("oddRes", BoolValue True)] . checkResults)

     ,("res = \"10\"",
                  testResults [("res", StrValue $ T.pack "10")] . checkResults)


     ,("res = fact 10\n\
       \fact n = if n == 0 then 1 else n * fact n",
                  testResults [("res", UndefinedValue)] . checkResults)


     ,("res n = n.age", psuccess 1 . checkparse)

     ,("res n = n.fizzbin\n\
       \frog = (res 44) + 1", testTypes [("frog", testPrimDouble)] . checktype)

     ,("res n = n.fizzbin\n\
       \frog = (res \"foo\") & \"1\"", testTypes [("frog", testPrimStr)] . checktype)

     ,("res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  x + y\n\
       \frog = (res 44) + 1", testTypes [("frog", testPrimDouble)] . checktype)

     ,("woof n =\n\
       \  x = n.fizzbin\n\
       \  show x\n\
       \res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  z = woof n\n\
       \  x + y + (len z)\n\
       \frog = (res 44) + 1", testTypes [("frog", testPrimDouble)] . checktype)

     ,("woof n =\n\
       \  x = n.fizzbin\n\
       \  show x\n\
       \woof2 n =\n\
       \  x = n.fizzbin\n\
       \  x\n\
       \res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  z = woof n\n\
       \  x + y + (len z)\n\
       \frog = (res 44) + 1\n\
       \dog = (woof2 \"44\") & (show (woof2 44))", testTypes [("frog", testPrimDouble), ("dog", testPrimStr)] . checktype)

     ,("woof n =\n\
       \  x = n.fizzbin\n\
       \  show x\n\
       \res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  z = woof n\n\
       \  x + y + (len z)\n\
       \frog = (res \"44\") + 1", failsTyper . checktype)

     ,("res n = n.fizzbin\n\
       \dog = (res 44) + 1\n\
       \frog = (res \"foo\") & \"1\"", testTypes [("frog", testPrimStr), ("dog", testPrimDouble)] . checktype)

     ,("res n = n.fizzbin\n\
       \dog = (res 44) + 1\n\
       \frog = (res \"foo\") + 1", failsTyper . checktype)


     ,("test_15_complex_source_sink.md", testTypes [("tax", testPrimDouble)
                                ,("taxable", testPrimDouble)] . checktype)
-}
    ]

testGenBoolFunc _ (TOper fon1 [TVar t1, 
                   TOper fon2 [TOper fon3 
                                     [TVar t2, TPrim PrimBool], TVar t3]]) | fon1 == funcOperName && 
                                                                             fon2 == funcOperName &&
                                                                             fon3 == funcOperName &&
                                                                             t1 == t2 &&
                                                                             t2 == t3 = Nothing
testGenBoolFunc n t = Just $ "In: "++n++" Expecting a generic plus bool function, but got " ++ show t


testGenFunc _ (TOper fon [TVar t1, TVar t2]) | fon == funcOperName && t1 == t2 = Nothing
testGenFunc n t = Just $ "In: "++n++" Expecting a generic function, but got " ++ show t

testGenXFunc _ (TOper fon [TVar t1, TVar t2]) | fon == funcOperName = Nothing
testGenXFunc n t = Just $ "In: "++n++" Expecting a generic function, but got " ++ show t

testT t1 n t2 =
  if t1 == t2 then Nothing else Just $ "In: " ++ n ++ " Expecting: " ++ show t1 ++ " but got " ++ show t2

testPrimDouble = testT $ TPrim PrimDouble

testPrimStr = testT $ TPrim PrimStr

testDoubleFunc = testT $ tFun (TPrim PrimDouble) (TPrim PrimDouble)

testStrFunc = testT $ tFun (TPrim PrimStr) (TPrim PrimStr)

-- | test that the string parses and there are cnt expressions
psuccess cnt p = case p of 
              Right ar | length ar == cnt -> Nothing
              Right ar -> Just $ "Expected " ++ show cnt ++ " but got " ++ show (length ar) ++ " expressions"
              Left msg -> Just $ show msg

pfailure p = case p of 
              (Left _) -> Nothing
              (Right _) -> Just "Should have failed"

-- checkparse :: (Error e) => String -> e
checkparse = parseLines

failsTyper (Left (TypeError _)) = Nothing
failsTyper (Right types) = Just $ "Expected a type error, but got " ++ show types

-- testTypes :: [(String, String -> Type -> Maybe String)] -> ThrowsError (Map.Map String Type) -> Either String ()
testTypes listOStuff res = 
    case res of 
        (Left err) -> Just $ show err
        (Right typeMap) -> 
          let testIt (funcName, expType) = 
                case Map.lookup (T.pack funcName) typeMap of
                  Just t -> expType funcName t
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
      let testIt (funcName, expVal) = case eval 0 Map.empty exprs $ Var NoSourceLoc $ FuncName $ T.pack funcName of
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