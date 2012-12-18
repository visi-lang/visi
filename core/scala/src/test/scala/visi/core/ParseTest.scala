package visi.core.test

// import org.specs2._
import org.specs2.mutable._
import net.liftweb.common.{Box, Full}
import java.io.File
import net.liftweb.util.Helpers
import org.parboiled.buffers.InputBuffer

import visi.core.{VisiParse, Expression}

object ParseTest extends Specification {

  private def dog = "then"

  "Parser" should {

    "Find fenced code" in {
      val hf = VisiParse.hasFences(
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
        """.stripMargin)

      true must_== true
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

    "parse definition" in {
      val res: Box[List[Expression]] = VisiParse.code("dog = 99\n")

      res.isDefined must_== true

    }



  }
}

/*


/*

    "Multiline if then else" in {
      val rest: Box[List[Expression]] =
        VisiParse.code(
          """
            |foo n =
            |  if
            |    n
          """.stripMargin + "  " + dog + "\n" +
            """
              |    3
              |  else  4
            """.stripMargin, true)

      rest.map(_.length) must_== Full(1)

      //true must_== true
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
      val res = VisiParse.code("      ?foo   \n")

      res.isDefined must_== false
    }

    "fail to parse an indented sink" in {
      VisiParse.code(
        """
          |   "bar" = 55
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
      val res = VisiParse.code(
        """
          |foo bar = len bar
          |
          |Iee like mooses and yaks!!
        """.stripMargin)

      res.isDefined must_== false
    }

*/


    /*
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
        """.stripMargin, false).map(_.length) must_== Full(3)
    }
*/
    /*
    val dog = {
      val f = new File("/home/dpp/proj/visi.wiki/tests")
      val kids = f.listFiles().toList.filter(_.getName.endsWith(".md")).filterNot(_.getName.toLowerCase.startsWith("index"))
      val all = kids.map(f => new String(Helpers.readWholeFile(f), "UTF-8") -> f)
      all
    }

    "Can parse all test files" in {
      dog.map{
        case (str, f) =>
          println("Running "+f.getName)
          (VisiParse.code(str).isDefined, f.getName) must_== (true, f.getName)
      }
    }
    */
 */
