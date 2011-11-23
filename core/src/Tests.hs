import Char
import List
import Test.QuickCheck
import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
-- reversing twice a finite list, is the same as identity
prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]
 
-- Dropping the "Haq! " string is the same as identity
prop_haq s = drop (length "Haq! ") (haqify s) == id s
    where haqify s = "Haq! " ++ s
 
tests  = [("reverse.reverse/id", quickCheck prop_reversereverse)
        ,("drop.haq/id",        quickCheck prop_haq)]