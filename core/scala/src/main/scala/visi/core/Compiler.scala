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
      |  this.$_func = f;
      |  this.$_get = function() {
      |    var ret = f();
      |    this.$_get = function() {return ret;}
      |    return ret;
      |  }
      |}
      |
      |function $_cloner(obj) {
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
      |function Func(compute, args, arity, scope) {
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
      |      var ret = compute(scope, args);
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
      |function $_plusFunc_core(scope, args) {
      |  return args[0].$_get() + args[1].$_get();
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
      |}
      |
    """.stripMargin

  def compileToJavaScript(theExp: Expression, sb: StringBuilder) {
    theExp match {
      case LetExp(loc, id, FuncName(name), generic, tpe, exp) =>
        sb.append("scope["+name.encJs+"] = ")
        compileToJavaScript(exp, sb)
        sb.append(";\n\n")

      case InnerLet(loc, tpe, exp1, exp2) =>
        sb.append("scope = $_cloner(scope);\n") // clone the local scope so we don't mess with the original ref
        compileToJavaScript(exp1, sb)
        compileToJavaScript(exp2, sb)

      case SinkExp(loc, id, name, tpe, exp) =>


      case SourceExp(loc, id, name, tpe) =>


      case FuncExp(loc, id, name, tpe, exp) =>


      case Apply(loc, id, tpe, exp1, exp2) =>
        compileToJavaScript(exp1, sb)
        sb.append(".$_apply(\n   ")
        compileToJavaScript(exp2, sb)
        sb.append(")\n")


      case vexp@Var(loc, id, FuncName(name), tpe) =>
        sb.append("scope["+name.encJs+"]")

      case BuiltIn(loc, id, FuncName(name), tpe, func) =>
        sb.append("scope["+name.encJs+"] = ")
        func(sb)
        sb.append(";\n\n")


      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        sb.append("new Const("+value.toJsString+")")

      case Group(map) =>
        map.values.foreach(e => compileToJavaScript(e, sb))
    }

  }
}
