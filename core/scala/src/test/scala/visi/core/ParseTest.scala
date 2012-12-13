package visi.core

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import net.liftweb.common.Full


class ParseTestAsTest extends JUnit4(ParseTest)
object ParseTestRunner extends ConsoleRunner(ParseTest)

object ParseTest extends Specification {

  "Parser" should {
    "Find fenced code" in {
      VisiParse.hasFences(
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
      VisiParse.hasFences(
        """
          |I like yaks
          |and mooses
          |```
          |Yay... I'm not fenced
          |   ```
          |Meow
        """.stripMargin) must_== false
    }

    /*
    "parse constant" in {
      VisiParse.code("99\n").isDefined must_== true

    }*/

    "parse definition" in {
      VisiParse.code("dog = 99\n").isDefined must_== true

    }

    "parse a source" in {
      VisiParse.code("?foo   \n").isDefined must_== true
    }

    "parse a sink" in {
      VisiParse.code(
        """
          |"bar" = 55
        """.stripMargin).isDefined must_== true
          }

    "fail to parse an indented source" in {
      VisiParse.code("      ?foo   \n").isDefined must_== false
    }

    "fail to parse an indented sink" in {
      VisiParse.code(
        """
          | "bar" = 55
        """.stripMargin).isDefined must_== false
          }

    "parse source and sink and exp" in {
      VisiParse.code(
        """
          |?foo
          |
          |"bar" = 99
          |
          |dog = 123
        """.stripMargin).map(_.length) must_== Full(3)
    }



    "parse source and sink and exp in something that's somewhat complex" in {
      VisiParse.code(
        """
          |?foo
          |
          |"bar" = dog
          |
          |dog = len foo
        """.stripMargin).map(_.length) must_== Full(3)
    }

    "parse a function with parameters" in {
      VisiParse.code(
        """
          |foo bar = len bar
        """.stripMargin).map(_.length) must_== Full(1)
    }

    "Don't parse something with cruft at the end" in {
      VisiParse.code(
        """
          |foo bar = len bar
          |
          |I like mooses and yaks!!
        """.stripMargin).isDefined must_== false
    }

    "parse source and sink and exp in something that's somewhat complex inside Markdown" in {
      VisiParse.code(
        """
          |This is a markdown document. Let's see how markdowntastic it is
          |
          |Here's a source named foo:
          |```
          |?foo
          |
          |```
          |
          |And a sync named bar with some stuff in between:
          |
          |```
          |
          |"bar" = dog
          |
          |dog = len foo
          |```
          |
        """.stripMargin).map(_.length) must_== Full(3)
    }


  }


}
