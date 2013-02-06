package visi.core

import org.specs2.mutable.Specification
import javax.script.ScriptEngineManager
import net.liftweb.common.Full
import org.mozilla.javascript.{Context, Scriptable}


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
            |?in
            |
            |"Out" = in + 1
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

    "Partial Application #1" in {
      val test =
        for {
          script <- Visi.compile(
            """
              |divit = (1 /)
              |
              |id n = n
              |
              |apply f n = f n
              |
              |res = apply (id divit) 2
            """.stripMargin)
        } yield {
          runScript(
            Compiler.jsLibrary +
              script +
              "\n\nvar frog = scope['res']; frog.$_get();"
          )
        }

      test must_== Full(0.5D)
    }

    "Partial Application #2" in {
      val test =
        for {
          script <- Visi.compile(
            """
              |divit = (/ 2)
              |
              |res = divit 2
            """.stripMargin)
        } yield {
          runScript(
            Compiler.jsLibrary +
              script +
              "\n\nvar frog = scope['res']; frog.$_get();"
          )
        }

      test must_== Full(1)
    }

    "Run sources and find sink values" in {
      val test =
        for {
          script <- Visi.compile(
            """
              |?in
              |
              |test x = 1
              |
              |cnt = cnt + test(in)
              |
              |sum = sum + in
              |
              |"Out" = in + 1
              |"Cnt" = cnt
              |"Sum" = sum
            """.stripMargin)
        } yield {

          val cx = Context.enter()
          try {
            val scope = cx.initStandardObjects()
            cx.evaluateString(scope, Compiler.jsLibrary +
              script, "test", 1, null)

          List(
            cx.evaluateString(scope, "$_exec({'in': 4});", "test", 1, null) match {
              case s: Scriptable => (s.get("Out", s), s.get("Cnt", s), s.get("Sum", s))
            },
            cx.evaluateString(scope, "$_exec({'in': 8});", "test", 1, null) match {
              case s: Scriptable => (s.get("Out", s), s.get("Cnt", s), s.get("Sum", s))
            }
          )
          } finally {
            Context.exit()
          }
        }

      test must_== Full(List((5, 1, 4), (9, 2, 12)))
    }


    "Choose" in {
      val test =
        for {
          script <- Visi.compile(
            """
              |?num1
              |?num2
              |?b
              |
              |choose b x y = if b then x else y
              |
              |mult = (*)
              |
              |ch = choose b
              |
              |"Out" = choose b (+) mult num1 num2
              |"The Num" = ch num1 num2
            """.stripMargin)
        } yield {

          val cx = Context.enter()
          try {
            val scope = cx.initStandardObjects()
          cx.evaluateString(scope, Compiler.jsLibrary +
            script, "test", 1, null)

          List(
            cx.evaluateString(scope, "$_exec({'num1': 4, 'num2':8, 'b': true});", "test", 1, null) match {
              case s: Scriptable => (s.get("Out", s), s.get("The Num", s))
            },
            cx.evaluateString(scope, "$_exec({'num1': 4, 'num2':8, 'b': false});", "test", 1, null) match {
              case s: Scriptable => (s.get("Out", s), s.get("The Num", s))
            },
            cx.evaluateString(scope, "$_exec({'num1': 44});", "test", 1, null) match {
              case s: Scriptable => (s.get("Out", s), s.get("The Num", s))
            },
            cx.evaluateString(scope, "$_exec({'num2': 2});", "test", 1, null) match {
              case s: Scriptable => (s.get("Out", s), s.get("The Num", s))
            }

          )
          } finally {
            Context.exit()
          }
        }

      test must_== Full(List(12 -> 4, 32 -> 8, 352 -> 8, 88 -> 2))
    }


  }

  def runScript(str: String): Any = {
    val cx = Context.enter()
    try {
      val scope = cx.initStandardObjects()
      cx.evaluateString(scope, str, "Test", 1, null)
    } finally {
      Context.exit()
    }
  }
}
