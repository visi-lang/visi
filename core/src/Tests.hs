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
      ("a = 1 // simple assignment", checkparse, psuccess)
      ,("f a = a + 1 // function definition", checkparse, psuccess)
      ,("f 33 = 44 // constant in parameter position", checkparse, pfailure)
      ,("f a = a {- a multiline example -}\n\
       \f b = b", checkparse, psuccess)
      ,("f a = if a then 3 else 4 {-if/then/else-}", checkparse, psuccess)
      ,("f a b c = f (1 + 2) 3 q w // multiple parameters to a function", checkparse, pfailure)
    ]

psuccess p = case p of 
              (Right _) -> Right ()
              (Left msg) -> Left $ show msg

pfailure p = case p of 
              (Left _) -> Right ()
              (Right _) -> Left "Should have failed"

-- checkparse :: String -> IO TResult
checkparse str = 
    return $ parseLines str

        
        