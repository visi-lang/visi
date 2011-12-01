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
import Visi.Runtime
import Visi.Expression
import Visi.Parse

import Test.HUnit

main :: IO ()
main = 
    do
        _ <- testOMatic myTests
        return ()
 
-- testOMatic a b => [(a, a -> b, b)] :: IO ()
testOMatic lst = 
    do 
      l2 <- mapM runTest lst
      mapM_ (snd) l2
      let errs = foldr (+) 0 $ map fst l2
      putStrLn $ "Ran " ++ (show $ length lst) ++ " tests, " ++ (show errs) ++ " errors"
      -- putStrLn "Dude"
    where runTest (param, func, test) = 
            do
                res <- func param
                let res' = test res
                return $ case res' of
                            (Left msg)-> (1, putStrLn $ "Failed " ++ (show param) ++ " error " ++ msg)
                            _ -> (0, return ())
            
myTests = 
    [
      ("a = 1 // simple assignment\n", checkparse, psuccess 1)
      ,("f a = a + 1 // function definition", checkparse, psuccess 1)
      ,("f 33 = 44 // constant in parameter position", checkparse, pfailure)
      ,("f a = a {- a multiline example -}\n\
       \f b = b", checkparse, psuccess 2)
      ,("f a = if a then 3 else 4 {-if/then/else-}", checkparse, psuccess 1)
      ,("f a b c = f (1 + 2) 3 q w // multiple parameters to a function", checkparse, psuccess 1)
      ,("add41 v = v + 41", checkparse, psuccess 1)
      ,("\"Answer\" = add41 1", checkparse, psuccess 1)
      ,("and = p1 && p2", checkparse, psuccess 1)
      ,("\"Greeting\" = \"Hello, World!\" // Sink a constant String", checkparse, psuccess 1)
      ,("\"And\" = p1 && p2\n\
         \?p1\n\
         \?p2", checkparse, psuccess 3)
      ,("\"Age\" = 2011 - birthYear\n\
         \?birthYear // birthYear infered as Number", checkparse, psuccess 2)
      ,("{- A big multi-line expression -}\n\
         \total = subtotal + tax\n\
         \tax = taxable * taxRate\n\
         \subtotal = taxable + nonTaxable\n\n\n\
         \\"Total\" = total // sink the total\n\
         \\"Tax\" = tax // sink the tax\n\
         \?taxRate // source the tax rate\n\
         \?taxable\n\
         \?nonTaxable", checkparse, psuccess 8)
      ,("{- and indented line should fail -}\n\
         \total = subtotal + tax\n\
         \tax = taxable * taxRate\n\
         \subtotal = taxable + nonTaxable\n\n\n\
         \   \"Total\" = total // sink the total\n\
         \\"Tax\" = tax // sink the tax\n\
         \?taxRate // source the tax rate\n\
         \?taxable\n\
         \?nonTaxable", checkparse, pfailure)
    ]

-- | test that the string parses and there are cnt expressions
psuccess cnt p = case p of 
              (Right ar) | (length ar) == cnt -> Right ()
              (Right ar) -> Left $ "Expected " ++ show cnt ++ " but got " ++ (show $ length ar) ++ " expressions"
              (Left msg) -> Left $ show msg

pfailure p = case p of 
              (Left _) -> Right ()
              (Right _) -> Left "Should have failed"

-- checkparse :: String -> IO TResult
checkparse str = 
    return $ parseLines str

        
        