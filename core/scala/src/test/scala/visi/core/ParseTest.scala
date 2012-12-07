package visi.core

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner



class ParseTestAsTest extends JUnit4(ParseTest)
object ParseTestRunner extends ConsoleRunner(ParseTest)

object ParseTest extends Specification {

  "Parser" should {
    "Find fenced code" in {
      Parse.hasFences(
        """
          |I like yaks
          |and mooses
          |```
          |This is some code
          |```
          |
          |This is not code
          |
          |```
          |This is more code
          |```
        """.stripMargin) must_== true
    }

    "Not Find fenced code" in {
      Parse.hasFences(
        """
          |I like yaks
          |and mooses
          |```
          |Yay... I'm not fenced
          |   ```
          |Meow
        """.stripMargin) must_== false
    }

    "parse constant" in {
      Parse.code("foo = 99\n").isDefined must_== true

    }


  }


}
