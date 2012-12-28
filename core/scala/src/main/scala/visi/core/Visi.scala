package visi.core

import net.liftweb.common._
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.BooleanType

/**
 * The main access to Visi
 */
object Visi {

  lazy val builtIn: Map[FuncName, Expression] = Map(
    FuncName("true") -> BuiltIn(NoSourceLoc, LetId("true"), FuncName("true"), TPrim(PrimBool), (v: Value) => BoolValue(true)),
    FuncName("false") -> BuiltIn(NoSourceLoc, LetId("false"), FuncName("false"), TPrim(PrimBool), (v: Value) => BoolValue(false)),
    FuncName("$ifelse") -> BuiltIn(NoSourceLoc, LetId("ifelse"), FuncName("$ifelse"),
    {
      val tvar = TVar("ifelseTVar")
      Expression.tFun(TPrim(PrimBool), Expression.tFun(tvar, Expression.tFun(tvar, tvar)))
    }, (v: Value) => BoolValue(true)),
    FuncName("+") -> BuiltIn(NoSourceLoc, LetId("plus"), FuncName("+"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, (v: Value) => BoolValue(true)),
    FuncName("-") -> BuiltIn(NoSourceLoc, LetId("minus"), FuncName("-"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, (v: Value) => BoolValue(true)),
    FuncName("*") -> BuiltIn(NoSourceLoc, LetId("times"), FuncName("*"),
    {
      Expression.tFun(TPrim(PrimDouble), Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble)))
    }, (v: Value) => BoolValue(true)),
    FuncName("&") -> BuiltIn(NoSourceLoc, LetId("concat"), FuncName("&"),
    {
      Expression.tFun(TPrim(PrimStr), Expression.tFun(TPrim(PrimStr), TPrim(PrimStr)))
    }, (v: Value) => BoolValue(true)),

      FuncName("==") -> BuiltIn(NoSourceLoc, LetId("equals"), FuncName("=="),
    {
      val tvar = TVar("equalsTVar")
      Expression.tFun(tvar, Expression.tFun(tvar, TPrim(PrimBool)))
    }, (v: Value) => BoolValue(true))


  )

  def parseAndType(src: String): Box[RunableInfo] = {
    for {
      parsed <- VisiParse.code(src)
      typed <- Typer.infer(builtIn ++ parsed)
    } yield RunableInfo(parsed, typed)
  }

  final case class RunableInfo(functions: Map[FuncName, Expression], types: Map[FuncName, Type])
}

