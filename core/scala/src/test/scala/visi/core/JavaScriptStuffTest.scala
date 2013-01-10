package visi.core

import org.specs2.mutable.Specification
import javax.script.ScriptEngineManager
import net.liftweb.common.Full

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/28/12
 * Time: 10:48 AM
 * To change this template use File | Settings | File Templates.
 */
class JavaScriptStuffTest  extends Specification {
  val mgr = new ScriptEngineManager();



  "JavaScript code" should {
    "Run hello" in {
      runScript(Compiler.jsLibrary +
        """
          | "hello";
        """.stripMargin) must_== "hello"
    }

    "Run a $ in a function name" in {
      runScript(
        Compiler.jsLibrary +
        """
          |var moo = {};
          |moo.$_frog = function() {return "meow";};
          |moo.$_frog();
        """.stripMargin) must_== "meow"
    }

    "Create a thunk prototype" in {
      runScript(
        Compiler.jsLibrary +
        """
          |var q = new Thunk(function() {return "hello thunk";});
          |q.$_get();
        """.stripMargin) must_== "hello thunk"
    }

    "Create a thunk and const prototype" in {
      runScript(
        Compiler.jsLibrary +
        """
          |
          |var q = new Const("hello thunk");
          |q.$_get();
        """.stripMargin) must_== "hello thunk"
    }

    "Create a function and apply it" in {
      runScript(
        Compiler.jsLibrary +
        """
          |
          |var q = new Func($_concat_core, [], 2, {}, false);
          |var q1 = q.$_apply(new Const("hello "));
          |var q2 = q1.$_apply(new Const("thunk"));
          |q2.$_get();
        """.stripMargin) must_== "hello thunk"
    }

    "Parse a script" in {
     val test =
       for {
        script <- Visi.compile(
          """
            |x = if 44 == 44 then 44 else 55
            |
            |mult y = times x y
            |
            |times a b = a * b
          """.stripMargin)
      } yield {
         runScript(
         Compiler.jsLibrary +
         script +
         "\n\nvar frog = scope['mult'].$_apply(new Const(10)); frog.$_get();"
         )
       }

      test must_== Full(440)
    }

    "Run factorial" in {
      val test =
        for {
          script <- Visi.compile(
            """
              |fact n = if n == 0 then 1 else n * fact (n - 1)
              |res = fact 10
            """.stripMargin)
        } yield {
          runScript(
            Compiler.jsLibrary +
              script +
              "\n\nvar frog = scope['res']; frog.$_get();"
          )
        }

      test must_== Full(3628800)
    }


  }

  def runScript(str: String): Any = {
    val engine = mgr.getEngineByName("JavaScript")
    engine.eval(str)
  }
}
