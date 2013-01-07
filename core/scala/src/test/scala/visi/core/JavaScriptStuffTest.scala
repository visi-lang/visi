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

  lazy val jsLibrary =
    """
      |function Thunk(f) {
      |  this.$_func = f;
      |  this.$_get = function() {
      |    var ret = f();
      |    this.$_get = function() {return ret;}
      |    return ret;
      |  }
      |}
      |
      |function cloner(obj) {
      |    if(obj == null || typeof(obj) != 'object')
      |        return obj;
      |
      |    var temp = obj.constructor();
      |
      |    for(var key in obj)
      |        temp[key] = obj[key];
      |    return temp;
      |}
      |
      |function Var(scope) {
      |
      |}
      |
      |function Func(compute, args, arity) {
      |  this.$_arity = arity;
      |  this.$_compute = compute;
      |  this.$_args = args;
      |  if (args.length >= arity) {
      |    this.$_apply = function(ignore) {return this;};
      |  } else {
      |    this.$_apply = function(param) {
      |      var a1 = args;
      |      var a2 = [];
      |      for (i in a1) a2.push(a1[i]);
      |      a2.push(param);
      |      return new Func(compute, a2, arity);
      |    }
      |  }
      |
      |  if (arity > args.length) {
      |    this.$_get = function() {return this;};
      |  } else {
      |    this.$_get = function() {
      |      var ret = compute(args);
      |      this.$_get = function() {return ret;};
      |      return ret;
      |    }
      |  }
      |}
      |
      |function $_plusFunc() {
      |  return new Func($_plusFunc_core, [], 2);
      |}
      |
      |function $_plusFunc_core(args) {
      |  return args[0].$_get() + args[1].$_get();
      |}
      |
      |function $_timesFunc_core(args) {
      |  return args[0].$_get() * args[1].$_get();
      |}
      |
      |function $_ifelse_core(args) {
      |  if (args[0].$_get()) return args[1].$_get();
      |  return args[2].$_get();
      |}
      |
      |function $_concat_core(args) {
      |  return args[0].$_get() + args[1].$_get();
      |}
      |
      |function $_equals_core(args) {
      |  return args[0].$_get() == args[1].$_get();
      |}
      |
      |
      |function Const(v) {
      |  this.$_get = function() {return v;}
      |}
      |
    """.stripMargin

  "JavaScript code" should {
    "Run hello" in {
      runScript(jsLibrary +
        """
          | "hello";
        """.stripMargin) must_== "hello"
    }

    "Run a $ in a function name" in {
      runScript(
      jsLibrary +
        """
          |var moo = {};
          |moo.$_frog = function() {return "meow";};
          |moo.$_frog();
        """.stripMargin) must_== "meow"
    }

    "Create a thunk prototype" in {
      runScript(
      jsLibrary +
        """
          |var q = new Thunk(function() {return "hello thunk";});
          |q.$_get();
        """.stripMargin) must_== "hello thunk"
    }

    "Create a thunk and const prototype" in {
      runScript(
      jsLibrary +
        """
          |
          |var q = new Const("hello thunk");
          |q.$_get();
        """.stripMargin) must_== "hello thunk"
    }

    "Create a function and apply it" in {
      runScript(
      jsLibrary +
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
