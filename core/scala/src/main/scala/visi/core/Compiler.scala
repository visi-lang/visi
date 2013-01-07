package visi.core


/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/28/12
 * Time: 4:56 PM
 * To change this template use File | Settings | File Templates.
 */
object Compiler {
  def compileToJavaScript(theExp: Expression, sb: StringBuilder) {
    theExp match {
      case LetExp(loc, id, name, generic, tpe, exp) =>


      case InnerLet(loc, tpe, exp1, exp2) =>


      case SinkExp(loc, id, name, tpe, exp) =>


      case SourceExp(loc, id, name, tpe) =>


      case FuncExp(loc, id, name, tpe, exp) =>


      case Apply(loc, id, tpe, exp1, exp2) =>

      case vexp@Var(loc, id, name, tpe) =>

      case BuiltIn(loc, id, name, tpe, _) =>

      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        sb.append("new Const("+value.toJsString+")")

      case Group(map) =>
        map.values.foreach(e => compileToJavaScript(e, sb))
    }

  }
}
