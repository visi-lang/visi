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
      |function Thunk(f, scope) {
      |  this.$_get = getFunc()
      |
      |  function getFunc() {
      |    return function() {
      |      this.$_get = function() {return this.$_old;};
      |      var ret = f(scope);
      |      this.$_get = function() {return ret;}
      |      this.$_old = ret;
      |      return ret;
      |    };
      |  }
      |
      |  this.$_reset = function() {
      |    this.$_get = getFunc()
      |  }
      |
      |  this.$_apply = function(x) {
      |    return this.$_get().$_apply(x);
      |  }
      |
      |  this.$_old = 0
      |}
      |
      |function Value(v) {
      |  this.$_set = function(nv) {v = nv;};
      |  this.$_get = function() {return v;};
      |
      |  this.$_reset = function() {};
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
      |function Func(compute, args, arity, scope, subfunc) {
      |  if (args.length >= arity) {
      |    this.$_apply = function(param) {
      |    var got = this.$_get();
      |    var ret = got.$_apply(param);
      |    return ret;
      |    };
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
      |    this.$_get = function() {return this;};
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
      |  return args[0].$_get() - args[1].$_get();
      |}
      |
      |function $_timesFunc_core(scope, args) {
      |  return args[0].$_get() * args[1].$_get();
      |}
      |
      |function $_divFunc_core(scope, args) {
      |  return args[0].$_get() / args[1].$_get();
      |}
      |
      |function $_ifelse_core(scope, args) {
      |  if (args[0].$_get()) return args[1].$_get();
      |  return args[2].$_get();
      |}
      |
      |function $_swapfunc_core(scope, args) {
      |  var ret = new Func(function(zscope, zargs) {
      |   return args[0].$_apply(zargs[1]).$_apply(zargs[0]).$_get();
      |  }, [], 2, scope, false);
      |
      |  return ret;
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
      |function $_exec_str(sources) {
      |  var res = null;
      |
      |  try {
      |    res = {success: $_exec(sources)};
      |  } catch (e) {
      |    res = {failure: e};
      |  }
      |
      |  return JSON.stringify(res);
      |
      |}
      |
      |function $_exec(sources) {
      |  var sinksToRun = {};
      |  var lets = {};
      |  for (i in sources) {
      |    scope[i].$_set(sources[i]);
      |    sourceInfo[i] = true;
      |    var sa = sourceToSink[i];
      |    for (j in sa) {
      |      sinksToRun[sa[j]] = true;
      |    }
      |
      |    sa = sourceToLets[i]
      |    for (j in sa) {
      |      lets[sa[j]] = true;
      |    }
      |  }
      |
      |  for (i in sourceInfo) {
      |    if (!sourceInfo[i]) throw ("Cannot execute because the source '"+i+"' has not been set");
      |  }
      |
      |  for (i in lets) {
      |    scope[i].$_reset();
      |  }
      |
      |  var ret = {};
      |
      |  for (i in sinksToRun) {
      |    sinks[i].$_reset();
      |    ret[i] = sinks[i].$_get();
      |  }
      |
      |  return ret;
      |}
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
    FuncName("$swapfunc") -> BuiltIn(NoSourceLoc, LetId("swapfunc"), FuncName("$swapfunc"),
    {
      val tvar1 = TVar("swapfuncVar1")
      val tvar2 = TVar("swapfuncVar2")
      val tvar3 = TVar("swapfuncVar3")
      Expression.tFun(Expression.tFun(tvar1, Expression.tFun(tvar2, tvar3)), Expression.tFun(tvar2, Expression.tFun(tvar1, tvar3)))
    }, sb => sb.append("new Func($_swapfunc_core, [], 1, scope, false)")),
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
    FuncName("/") -> BuiltIn(NoSourceLoc, LetId("div"), FuncName("/"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new Func($_divFunc_core, [], 2, scope, false)")),
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

  def compile(theExp: Expression, depInfo: Typer.DependencyMap): String = {

    val sb = new StringBuilder

    val pred = Typer.findAllTopLevelPredicates(depInfo)
    val deps = Typer.whatDependsOnSource(depInfo, pred)
    val rec = Typer.findRecursive(pred)


    sb.append("var scope = {};\n")
    sb.append("var sourceInfo = {};\n")
    sb.append("var sinks = {};\n")
    sb.append("var sourceToSink = {};\n")
    sb.append("var sourceToLets = {};\n")

    sb.append("// enumerate the sources... they must all be satisfied to execute\n\n")
    deps.foreach{
      case (source, (sinkList, recList)) =>
        sb.append("sourceInfo["+source.name.name.encJs+"] = false;\n")

        sb.append("sourceToSink["+source.name.name.encJs+"] = [")
        sb.append(sinkList.map(_.name.name.encJs).mkString(", "))
        sb.append("];\n")

        sb.append("sourceToLets["+source.name.name.encJs+"] = [")
        sb.append(recList.map(_.name.name.encJs).mkString(", "))
        sb.append("];\n")

    }



    compileToJavaScript(theExp, sb)
    sb.toString()
  }

  private def compileToJavaScript(theExp: Expression, sb: StringBuilder) {
    theExp match {
      case LetExp(loc, id, FuncName(name), generic, tpe, exp: FuncExp) =>
        sb.append("scope["+name.encJs+"] = ")
        compileToJavaScript(exp, sb)
        sb.append(";\n\n")

      case LetExp(loc, id, FuncName(name), generic, tpe, exp) =>
        sb.append("scope["+name.encJs+"] = new Thunk(function(scope) { return ")
        compileToJavaScript(exp, sb)
        sb.append(".$_get();}, scope);\n\n")

      case InnerLet(loc, tpe, exp1, exp2) =>
        sb.append("var scope = $_cloner(scope);\n") // clone the local scope so we don't mess with the original ref
        compileToJavaScript(exp1, sb)
        compileToJavaScript(exp2, sb)

      case SinkExp(loc, id, FuncName(name), tpe, exp) =>
        sb.append("sinks["+name.encJs+"] = new Thunk(function(scope) { return ")
        compileToJavaScript(exp, sb)
        sb.append(".$_get();}, scope);\n\n")

      case SourceExp(loc, id, FuncName(name), tpe) =>
        sb.append("scope["+name.encJs+"] = new Value(false);\n")

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
        map.values.foreach(e => compileToJavaScript(e, sb))
    }

  }
}
