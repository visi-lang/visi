import Visi.Util
import Visi.Runtime
import Visi.Expression
import Visi.Typer
import Visi.Executor
import Test.HUnit

main = do
		setSource "Hello" $ StrValue "Moo"
		putStrLn "Howdy"