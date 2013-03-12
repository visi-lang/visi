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
      |this.Thunk = function(f, scope) {
      |
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
      |  this.$_get = getFunc()
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
      |};
      |
      |var thunkFunc = this.Thunk;
      |
      |this.Value = function(v) {
      |  this.$_set = function(nv) {v = nv;};
      |  this.$_get = function() {return v;};
      |
      |  this.$_reset = function() {};
      |};
      |
      |var valueFunc = this.Value;
      |
      |this.$_cloner = function(obj) {
      |    if(obj == null || typeof(obj) != 'object')
      |        return obj;
      |
      |    var temp = {};
      |
      |    for(var key in obj)
      |        temp[key] = obj[key];
      |    return temp;
      |};
      |
      |this.Func = function(compute, args, arity, scope, subfunc) {
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
      |      for (var i in a1) a2.push(a1[i]);
      |      a2.push(param);
      |      if (subfunc) return compute(scope, a2);
      |      return new funcFunc(compute, a2, arity, scope, false);
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
      |};
      |
      |var funcFunc = this.Func;
      |
      |this.$_plusFunc_core = function(scope, args) {
      |  return args[0].$_get() + args[1].$_get();
      |};
      |
      |this.$_minusFunc_core = function(scope, args) {
      |  return args[0].$_get() - args[1].$_get();
      |};
      |
      |this.$_timesFunc_core = function(scope, args) {
      |  return args[0].$_get() * args[1].$_get();
      |};
      |
      |this.$_len_core = function(scope, args) {
      |  return args[0].$_get().length;
      |};
      |
      |this.$_divFunc_core = function(scope, args) {
      |  return args[0].$_get() / args[1].$_get();
      |};
      |
      |this.$_ifelse_core = function(scope, args) {
      |  if (args[0].$_get()) return args[1].$_get();
      |  return args[2].$_get();
      |};
      |
      |this.$_swapfunc_core = function(scope, args) {
      |  var ret = new funcFunc(function(zscope, zargs) {
      |   return args[0].$_apply(zargs[1]).$_apply(zargs[0]).$_get();
      |  }, [], 2, scope, false);
      |
      |  return ret;
      |};
      |
      |this.$_concat_core = function(scope, args) {
      |  return args[0].$_get() + args[1].$_get();
      |};
      |
      |this.$_equals_core = function(scope, args) {
      |  return args[0].$_get() == args[1].$_get();
      |};
      |
      |
      |this.Const = function(v) {
      |  this.$_get = function() {return v;}
      |  this.$_reset = function() {}
      |};
      |
      |var constFunc = this.Const;
      |
      |this.$_exec_str = function(sources) {
      |  return JSON.stringify(this.$_exec_maybe(sources));
      |};
      |
      |this.$_exec_maybe = function(sources) {
      |  try {
      |    return {success: this.$_exec(sources)};
      |  } catch (e) {
      |    return {failure: e};
      |  }
      |};
      |
      |this.$_doneOne = false;
      |
      |this.$_exec = function(sources) {
      |  var sinksToRun = {};
      |  var lets = {};
      |  for (var i in sources) {
      |    this.scope[i].$_set(sources[i]);
      |    this.sourceInfo[i] = true;
      |    var sa = this.sourceToSink[i];
      |    for (var j in sa) {
      |      sinksToRun[sa[j]] = true;
      |    }
      |
      |    sa = this.sourceToLets[i]
      |    for (var j in sa) {
      |      lets[sa[j]] = true;
      |    }
      |  }
      |
      |  for (var i in this.sourceInfo) {
      |    if (!this.sourceInfo[i]) throw ("Cannot execute because the source '"+i+"' has not been set");
      |  }
      |
      |  if (!this.$_doneOne) sinksToRun = this.sinks;
      |
      |  for (var i in lets) {
      |    this.scope[i].$_reset();
      |  }
      |
      |  var ret = {};
      |
      |
      |
      |  for (var i in sinksToRun) {
      |    this.sinks[i].$_reset();
      |    ret[i] = this.sinks[i].$_get();
      |  }
      |
      |  this.$_doneOne = true;
      |
      |  return ret;
      |};
    """.stripMargin

  lazy val builtIn: Map[String, Expression] = Map(
    ("true") -> BuiltIn(NoSourceLoc, LetId("true"), ("true"), TPrim(PrimBool),
      sb => sb.append("new constFunc(true)")),
    ("false") -> BuiltIn(NoSourceLoc, LetId("false"), ("false"), TPrim(PrimBool),
      sb => sb.append("new constFunc(false)")),
    ("$ifelse") -> BuiltIn(NoSourceLoc, LetId("ifelse"), ("$ifelse"),
    {
      val tvar = TVar("ifelseTVar")
      Expression.tFun(TPrim(PrimBool), Expression.tFun(tvar, Expression.tFun(tvar, tvar)))
    }, sb => sb.append("new funcFunc(this.$_ifelse_core, [], 3, scope, false)")),
    ("$swapfunc") -> BuiltIn(NoSourceLoc, LetId("swapfunc"), ("$swapfunc"),
    {
      val tvar1 = TVar("swapfuncVar1")
      val tvar2 = TVar("swapfuncVar2")
      val tvar3 = TVar("swapfuncVar3")
      Expression.tFun(Expression.tFun(tvar1, Expression.tFun(tvar2, tvar3)), Expression.tFun(tvar2, Expression.tFun(tvar1, tvar3)))
    }, sb => sb.append("new funcFunc(this.$_swapfunc_core, [], 1, scope, false)")),
    ("+") -> BuiltIn(NoSourceLoc, LetId("plus"), ("+"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new funcFunc(this.$_plusFunc_core, [], 2, scope, false)")),
    ("-") -> BuiltIn(NoSourceLoc, LetId("minus"), ("-"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new funcFunc(this.$_minusFunc_core, [], 2, scope, false)")),
    ("*") -> BuiltIn(NoSourceLoc, LetId("times"), ("*"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new funcFunc(this.$_timesFunc_core, [], 2, scope, false)")),
    ("/") -> BuiltIn(NoSourceLoc, LetId("div"), ("/"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, sb => sb.append("new funcFunc(this.$_divFunc_core, [], 2, scope, false)")),
    ("&") -> BuiltIn(NoSourceLoc, LetId("concat"), ("&"),
    {
      Expression.tFun(TPrim(PrimStr), Expression.tFun(TPrim(PrimStr), TPrim(PrimStr)))
    }, sb => sb.append("new funcFunc(this.$_concat_core, [], 2, scope, false)")),

    ("len") -> BuiltIn(NoSourceLoc, LetId("len"), ("len"),
    {
      Expression.tFun(TPrim(PrimStr), TPrim(PrimDouble))
    }, sb => sb.append("new funcFunc(this.$_len_core, [], 1, scope, false)")),

    ("==") -> BuiltIn(NoSourceLoc, LetId("equals"), ("=="),
    {
      val tvar = TVar("equalsTVar")
      Expression.tFun(tvar, Expression.tFun(tvar, TPrim(PrimBool)))
    }, sb => sb.append("new funcFunc(this.$_equals_core, [], 2, scope, false)"))



  )

  def compile(theExp: Expression, depInfo: Typer.DependencyMap): String = {

    val sb = new StringBuilder

    val pred = Typer.findAllTopLevelPredicates(depInfo)
    val deps = Typer.whatDependsOnSource(depInfo, pred)
    val rec = Typer.findRecursive(pred)


    sb.append("this.scope = {};\n")
    sb.append("var scope = this.scope;\n")
    sb.append("this.sourceInfo = {};\n")
    sb.append("this.sinks = {};\n")
    sb.append("this.sourceToSink = {};\n")
    sb.append("this.sourceToLets = {};\n")

    sb.append("// enumerate the sources... they must all be satisfied to execute\n\n")
    deps.foreach{
      case (source, (sinkList, recList)) =>
        sb.append("this.sourceInfo["+source.name.encJs+"] = false;\n")

        sb.append("this.sourceToSink["+source.name.encJs+"] = [")
        sb.append(sinkList.map(_.name.encJs).mkString(", "))
        sb.append("];\n")

        sb.append("this.sourceToLets["+source.name.encJs+"] = [")
        sb.append(recList.map(_.name.encJs).mkString(", "))
        sb.append("];\n")

    }



    compileToJavaScript(theExp, sb)
    sb.toString()
  }

  private def compileToJavaScript(theExp: Expression, sb: StringBuilder) {
    theExp match {
      case LetExp(loc, id, (name), generic, tpe, exp: FuncExp) =>
        sb.append("scope["+name.encJs+"] = ")
        compileToJavaScript(exp, sb)
        sb.append(";\n\n")

      case LetExp(loc, id, (name), generic, tpe, exp) =>
        sb.append("scope["+name.encJs+"] = new thunkFunc(function(scope) { return ")
        compileToJavaScript(exp, sb)
        sb.append(".$_get();}, scope);\n\n")

      case InnerLet(loc, tpe, exp1, exp2) =>
        sb.append("var scope = $_cloner(scope);\n") // clone the local scope so we don't mess with the original ref
        compileToJavaScript(exp1, sb)
        compileToJavaScript(exp2, sb)

      case SinkExp(loc, id, (name), tpe, exp) =>
        sb.append("this.sinks["+name.encJs+"] = new thunkFunc(function(scope) { return ")
        compileToJavaScript(exp, sb)
        sb.append(".$_get();}, scope);\n\n")

      case SourceExp(loc, id, (name), tpe) =>
        sb.append("scope["+name.encJs+"] = new valueFunc(false);\n")

      case FuncExp(loc, id, (name), tpe, exp) =>
        val subfunc = exp.isInstanceOf[FuncExp]
        sb.append("new funcFunc(")
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


      case vexp@Var(loc, id, (name), tpe) =>
        sb.append("scope["+name.encJs+"]")

      case BuiltIn(loc, id, (name), tpe, func) =>
        sb.append("scope["+name.encJs+"] = ")
        func(sb)
        sb.append(";\n\n")


      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        sb.append("new constFunc("+value.toJsString+")")

      case Group(map) =>
        map.values.foreach(e => compileToJavaScript(e, sb))
    }

  }
}
