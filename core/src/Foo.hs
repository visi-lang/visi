import Visi.Util
import Visi.Runtime
import Visi.Expression

import Test.HUnit

main = do
		setSource "Hello" $ StrValue "Moo"
		putStrLn "Howdy"