package visi.core

import net.liftweb._
import util._
import Helpers._

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/28/12
 * Time: 4:56 PM
 * To change this template use File | Settings | File Templates.
 */
object Compiler {

  lazy val jsLibrary =
    """
      |function Thunk(f) {
      |  this.$_get = getFunc()
      |
      |  function getFunc() {
      |    return function() {
      |      var ret = f();
      |      this.$_get = function() {return ret;}
      |      return ret;
      |    };
      |  }
      |
      |  this.$_reset = function() {
      |    this.$_get = getFunc()
      |  }
      |}
      |
      |function $_cloner(obj) {
      |    if(obj == null || typeof(obj) != 'object')
      |        return obj;
      |
      |    var temp = {};
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
      |function Func(compute, args, arity, scope, subfunc) {
      |  if (args.length >= arity) {
      |    this.$_apply = function(ignore) {throw "trying to apply when the arity is complete"};
      |  } else {
      |    this.$_apply = function(param) {
      |      var a1 = args;
      |      var a2 = [];
      |      for (i in a1) a2.push(a1[i]);
      |      a2.push(param);
      |      if (subfunc) return compute(scope, a2);
      |      return new Func(compute, a2, arity, scope, false);
      |    }
      |  }
      |
      |  if (arity > args.length) {
      |    this.$_get = function() {throw "Trying to get a function"};
      |  } else {
      |    this.$_get = function() {
      |      var ret = compute(scope, args);
      |      this.$_get = function() {return ret;};
      |      return ret;
      |    }
      |  }
      |
      |  this.$_reset = function() {};
      |}
      |
      |function $_plusFunc_core(scope, args) {
      |  return args[0].$_get() + args[1].$_get();
      |}
      |
      |function $_minusFunc_core(scope, args) {
      |  return args[0].$get() - args[1].$_get();
      |}
      |
      |function $_timesFunc_core(scope, args) {
      |  return args[0].$_get() * args[1].$_get();
      |}
      |
      |function $_ifelse_core(scope, args) {
      |  if (args[0].$_get()) return args[1].$_get();
      |  return args[2].$_get();
      |}
      |
      |function $_concat_core(scope, args) {
      |  return args[0].$_get() + args[1].$_get();
      |}
      |
      |function $_equals_core(scope, args) {
      |  return args[0].$_get() == args[1].$_get();
      |}
      |
      |
      |function Const(v) {
      |  this.$_get = function() {return v;}
      |  this.$_reset = function() {}
      |}
      |
    """.stripMargin

  lazy val builtIn: Map[FuncName, Expression] = Map(
    FuncName("true") -> BuiltIn(NoSourceLoc, LetId("true"), FuncName("true"), TPrim(PrimBool),
      sb => sb.append("new Const(true)")),
    FuncName("false") -> BuiltIn(NoSourceLoc, LetId("false"), FuncName("false"), TPrim(PrimBool),
      sb => sb.append("new Const(false)")),
    FuncName("$ifelse") -> BuiltIn(NoSourceLoc, LetId("ifelse"), FuncName("$ifelse"),
    {
      val tvar = TVar("ifelseTVar")
      Expression.tFun(TPrim(PrimBool), Expression.tFun(tvar, Expression.tFun(tvar, tvar)))
    }, sb => sb.append("new Func($_ifelse_core, [], 3, scope, false)")),
    FuncName("+") -> BuiltIn(NoSourceLoc, LetId("plus"), FuncName("+"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new Func($_plusFunc_core, [], 2, scope, false)")),
    FuncName("-") -> BuiltIn(NoSourceLoc, LetId("minus"), FuncName("-"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new Func($_minusFunc_core, [], 2, scope, false)")),
    FuncName("*") -> BuiltIn(NoSourceLoc, LetId("times"), FuncName("*"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new Func($_timesFunc_core, [], 2, scope, false)")),
    FuncName("&") -> BuiltIn(NoSourceLoc, LetId("concat"), FuncName("&"),
    {
      Expression.tFun(TPrim(PrimStr), Expression.tFun(TPrim(PrimStr), TPrim(PrimStr)))
    }, sb => sb.append("new Func($_concat_core, [], 2, scope, false)")),

    FuncName("==") -> BuiltIn(NoSourceLoc, LetId("equals"), FuncName("=="),
    {
      val tvar = TVar("equalsTVar")
      Expression.tFun(tvar, Expression.tFun(tvar, TPrim(PrimBool)))
    }, sb => sb.append("new Func($_equals_core, [], 2, scope, false)"))


  )

  def compile(theExp: Expression): String = {
    val sb = new StringBuilder
    compileToJavaScript(theExp, sb)
    sb.toString()
  }

  def compileToJavaScript(theExp: Expression, sb: StringBuilder) {
    theExp match {
      case LetExp(loc, id, FuncName(name), generic, tpe, exp: FuncExp) =>
        sb.append("scope["+name.encJs+"] = ")
        compileToJavaScript(exp, sb)
        sb.append(";\n\n")

      case LetExp(loc, id, FuncName(name), generic, tpe, exp) =>
        sb.append("scope["+name.encJs+"] = new Thunk(function() { return ")
        compileToJavaScript(exp, sb)
        sb.append(".$_get();});\n\n")

      case InnerLet(loc, tpe, exp1, exp2) =>
        sb.append("var scope = $_cloner(scope);\n") // clone the local scope so we don't mess with the original ref
        compileToJavaScript(exp1, sb)
        compileToJavaScript(exp2, sb)

      case SinkExp(loc, id, name, tpe, exp) =>


      case SourceExp(loc, id, name, tpe) =>


      case FuncExp(loc, id, FuncName(name), tpe, exp) =>
        val subfunc = exp.isInstanceOf[FuncExp]
        sb.append("new Func(")
        sb.append("function(zscope, zargs) {\n")
        sb.append("var scope = $_cloner(zscope);\n")
        sb.append("scope["+name.encJs+"] = zargs[0];\n")
        sb.append("return ")
        compileToJavaScript(exp, sb)
        if (!subfunc) sb.append(".$_get()")
        sb.append(";\n")
        sb.append("\n}, [], 1, scope, "+subfunc+")")


      case Apply(loc, id, tpe, exp1, exp2) =>
        compileToJavaScript(exp1, sb)
        sb.append(".$_apply(\n   ")
        compileToJavaScript(exp2, sb)
        sb.append(")")


      case vexp@Var(loc, id, FuncName(name), tpe) =>
        sb.append("scope["+name.encJs+"]")

      case BuiltIn(loc, id, FuncName(name), tpe, func) =>
        sb.append("scope["+name.encJs+"] = ")
        func(sb)
        sb.append(";\n\n")


      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        sb.append("new Const("+value.toJsString+")")

      case Group(map) =>
        sb.append("var scope = {};\n")
        map.values.foreach(e => compileToJavaScript(e, sb))
    }

  }
}
