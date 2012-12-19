package visi.core

import org.specs._
import org.specs.runner.JUnit4
import org.specs.runner.ConsoleRunner
import net.liftweb.common.Full
import java.io.File
import net.liftweb.util.Helpers


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

    "parse a sink properly" in {
      VisiParse.code(
        """
          |"bar" = 55
        """.stripMargin) match {
        case Full(List(SinkExp(_,
        _, FuncName("bar"), TPrim(PrimDouble),ValueConst(_,DoubleValue(55.0),TPrim(PrimDouble))))) => true must_== true
        case x =>
          println(x)
          true must_== false
      }
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



    "Support a comment" in {
      VisiParse.code(
        """
          |/*
          |  Hello This is a comment
          |*/
          |foo n = /* foo */ "catfoog" /*
          |
          |bar
          |
          |*/
          |
          |
          |
        """.stripMargin).map(_.length) must_== Full(1)
    }

    "Multiline if then else" in {
      VisiParse.code(
        """
          |foo n =
          |  if
          |    n > 5 // cats
          |  then
          |    "foo"
          |  else
          |    "bar"
          |
        """.stripMargin).map(_.length) must_== Full(1)
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
          |/*
          | A comment
          |*/
          |"bar" = dog
          |
          |dog = len foo
          |```
          |
        """.stripMargin).map(_.length) must_== Full(3)
    }

    "Work with an inner function" in {
      VisiParse.code(
        """
          |// the function
          |f n =
          |   fact n = if n == 0 then 1 else n * fact (n - 1)
          |   app n fact
          |
          |fact n = if n == 0 then 1 else n * fact (n - 1)
          |
          |
        """.stripMargin).map(_.length) must_== Full(2)
    }


    "Fenced if/then/else" in {
      VisiParse.code(
        """
          |# Complex closing over local scope
          |
          |A key feature of functional programing languages is having a temporary function that closes over local scope.  This is an example of closing over local scope.  The `f` function takes a parameter, `b`, and returns a partially applied function that takes two more parameters.  All three of the parameters (the origin value of `b` as well as the two additional parameters), are multiplied together.
          |
          |`q` is a function that calls `f` with 8 so it returns a partially applied function.  The function returned by `q` will multiple the two incoming parameters by each other and then by 8.
          |
          |`app 8 q` returns a function that takes a parameter and multiplies the parameter by 64.  `(app 9 (app 8 q))` evaluates to 576.
          |
          |`z 8 9` is 10 * 8 * 9, or 720.
          |
          |The value of `res` should be 154: 576 - (720 + 10).
          |
          |The test insures that the local scope is closed over so that the partially applied function returned by `f` closes over the variable `b`.
          |
          |```
          |f b = /* test that the function closes over local scope */
          |  timesb n m = n * b * m
          |  timesb
          |
          |app v f = f v
          |
          |q = f 8
          |
          |z = f 10
          |
          |res = (app 9 (app 8 q)) - ((z 8 9) + (z 1 1))
          |```
          |
          |
        """.stripMargin).map(_.length) must_== Full(5)
    }

    val dog: List[(String, File)] = {
      val f = new File(new File((new File(".")).getCanonicalFile.  getParentFile.getParentFile.getParentFile, "visi.wiki"), "tests")

      if (f.exists()) {
      val kids = f.listFiles().toList.filter(_.getName.endsWith(".md")).filterNot(_.getName.toLowerCase.startsWith("index"))
      val all = kids.map(f => new String(Helpers.readWholeFile(f), "UTF-8") -> f)
      all
      } else Nil
    }

    "Can parse all test files" in {
      dog.map{
        case (str, f) =>
          val res = VisiParse.code(str)
          if (!res.isDefined) {
            println("Running "+f.getName)
            println("Failure: "+VisiParse.code(str, true))
          }
          (res.isDefined, f.getName) must_== (true, f.getName)
      }
    }
  }


}
