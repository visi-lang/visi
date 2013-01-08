package visi.core

import org.specs2.mutable.Specification
import javax.script.ScriptEngineManager

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
          |var q = new Func($_concat_core, [], 2);
          |var q1 = q.$_apply(new Const("hello "));
          |var q2 = q1.$_apply(new Const("thunk"));
          |q2.$_get();
        """.stripMargin) must_== "hello thunk"
    }
  }

  def runScript(str: String): Any = {
    val engine = mgr.getEngineByName("JavaScript")
    engine.eval(str)
  }
}
