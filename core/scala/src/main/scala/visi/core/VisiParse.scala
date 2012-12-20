package visi.core

import net.liftweb.common.{Full, ParamFailure, Empty, Box}
import org.parboiled.scala._
import net.liftweb.util.Helpers
import org.parboiled.Context
import org.parboiled.errors.InvalidInputError
import collection.mutable.ListBuffer
import org.parboiled.support.Chars

object VisiParse extends VisiParse

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/5/12
 * Time: 3:01 PM
 * To change this template use File | Settings | File Templates.
 */
class VisiParse extends Parser  {
  // def code(str: String): Box[List[Expression]] = code(str, false)

  def code(str: String, showChars: Boolean = false): Box[List[Expression]] = synchronized {
    // turn the CRLR or CR stuff into LF
    val s = str.replace("\r\n", "\n").replace("\r", "\n")

    // if we find fenced codeblocks, then just parse the stuff inside the fences

    val xf = s.transformIndents(lineCommentStart = "//")

    val ib = xf.inputBuffer

    val sc = new IHateScalaC

    val res = ReportingParseRunner(sc.theExpr).run(xf)

    if (res.matched) res.result
    else {
      val stuff = res.parseErrors.collect{
        case i: InvalidInputError => i
      }

      if (showChars) {

        val lb = new ListBuffer[Char]

        @scala.annotation.tailrec
        def doIt(x: Int) {
          ib.charAt(x) match {
            case Chars.EOI => println("Bailing "+x)
            case c => lb.append(c); doIt(x + 1)
          }
        }
        doIt(0)

        val lst = lb.toList.map {
          case x if x >= ' ' && x.toInt <= 127 => "'"+x+"'"
          case x => x.toInt.toString
        }

        println("Chars "+lst.mkString(", "))
      }

      ParamFailure("Failed to parse", stuff.map(x => (x.getErrorMessage, x.getFailedMatchers.toArray.toList, x.getStartIndex, x.getEndIndex, x)))
    }
  }

  private class IHateScalaC extends Parser {

   def theExpr: Rule1[List[Expression]] = rule("Top level") {
    (fencedExpressions | oneOrMore(expression)) ~ EOI // zeroOrMore(EOL | INDENT | " " | "\t" | DEDENT) ~ EOI
  }

  private def fencedExpressions: Rule1[List[Expression]] =
    oneOrMore(notInFence ~ fence ~ zeroOrMore(expression) ~ fence ~ notInFence) ~~> (x => x.flatten)


  private def fence: Rule0 = colZero ~ "```" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)

  private def notInFence: Rule0 = oneOrMore(!fence ~ AnyChar)



  private def BeginComment: Rule0 = rule("Begin comment"){ str("/*") }
  private def EndComment: Rule0 = rule("End comment"){ str( "*/") }
  private def Comment: Rule0 = rule("Comment") {BeginComment ~ zeroOrMore(CommentBody) ~ EndComment}
  private def CommentBody: Rule0 = rule("Comment Body") {Comment | (!BeginComment ~ !EndComment ~ AnyChar)}
  private def AnyChar: Rule0 = ANY

  private def colZero: Rule0 = !INDENT ~  toTestAction(ctx => {

    val  ret = ctx.getPosition.column == 1 && ctx.getCurrentChar.toInt < 64000

    ret
  })

  private def EOL: Rule0 = rule("End of line"){"\n"}

  private def LineComment: Rule0 = rule {"//" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)}

  private def integer: Rule0 = rule { optional("-") ~ (("1" - "9") ~ digits | digit) }

  private def digits: Rule0 = rule { oneOrMore(digit) }

  private def digit: Rule0 = rule { "0" - "9" }

  private def expression: Rule1[Expression] = rule("Expression"){
  (letExpr | funcExpr | source | sink  )  ~ zeroOrMore(EOL)
  }

  private def idStart: Rule1[String] = rule {((("a" - "z") | "_")) ~> withContext((s1: String, ctx) => {
    val s = s1.toString
    s
  })}

  private def identifierStr: Rule1[String] = rule {
    (idStart ~ (zeroOrMore("a" - "z" | "_" | "A" - "Z" | "'" | "0" - "9"  ) ~> ((s: String) => s))) ~~> ((s: String, s2: String) => {
      val ret = s + s2

      ret
    })
  }

  private def stringConst: Rule1[Expression] = rule {
    "\"" ~ (zeroOrMore(!EOL ~ !"\"" ~ AnyChar) ~> withContext((s: String, ctx) => ValueConst(calcLoc(ctx),
      StrValue(s), TPrim(PrimStr)))) ~ "\""
  }

  private def ifElseExp: Rule1[Expression] = rule {
    spaces ~ "if" ~ spaces ~ rightExp ~ spaces ~ "then" ~ spaces ~ rightExp ~ spaces ~ "else " ~ spaces ~ rightExp ~~>
    withContext((boolExp: Expression, trueExp: Expression, falseExp: Expression, ctx) => {
      val curType = Type.vendVar
      val loc = calcLoc(ctx)
      Apply( loc, LetId.make, curType,
                         Apply(loc, LetId.make, Expression.tFun( curType,curType),
                         Apply(loc, LetId.make, ifType(curType),
                           Var(loc, FuncName("$ifelse"), ifApplyType(curType)), boolExp), trueExp
                         ), falseExp)
    })
  }

  private def ifType(in: Type): Type = Expression.tFun(in, Expression.tFun(in, in))
  private def ifApplyType(in: Type): Type = Expression.tFun(TPrim(PrimBool), Expression.tFun(in, Expression.tFun(in, in)))

  private def operator: Rule1[Expression] = rule("Operator") {
    (spaces ~ anyOf("+/-*&><") ~ spaces) ~> withContext((s: String, ctx) => Var(calcLoc(ctx), FuncName(s), Type.vendVar))
  }

  private def parenExp: Rule1[Expression] = rule {
    (spaces ~ "(" ~ spaces ~ operator ~ spaces ~ ")" ~ spaces) |
    (spaces ~ "(" ~ spaces ~ rightExp ~ spaces ~ operator ~ spaces ~ ")" ~ spaces) ~~> withContext((r: Expression, op: Expression, ctx) => null) | // FIXME partially apply
    ((spaces ~ "(" ~ spaces  ~ operator ~ rightExp ~ spaces ~ spaces ~ ")" ~ spaces)) ~~> withContext((op: Expression, r: Expression, ctx) => null) | // FIXME partially apply
    spaces ~ "(" ~ spaces ~ rightExp ~ spaces ~ ")" ~ spaces
  }

  private def operatorExp: Rule1[Expression] = rule("Operator Expression") {
    (parenExp | ifElseExp | constExp | identifier) ~ spaces ~ operator ~ spaces ~ rightExp ~~> ((a,b,c) => b) // FIXME do the right thing

  }

  private def rightExp: Rule1[Expression] = {
    def thing = parenExp |
      ifElseExp |
      operatorExp |
      constExp |
      funcParamExp | identifier
   rule("Right hand expression") {
     (zeroOrMore(EOL) ~ INDENT ~ oneOrMore(letExpr | funcExpr) ~ spaces~ rightExp ~ spaces ~ DEDENT ~ spaces) ~~> ((a, b) => b) | // FIXME Group them
     (spaces ~ zeroOrMore(EOL) ~ spaces ~ INDENT ~ spaces ~ thing ~ spaces ~ zeroOrMore(EOL) ~ DEDENT ~ spaces) | (spaces ~ thing ~ spaces)
   }
  }

  private def constExp: Rule1[Expression] = rule("Constant") {
    NumberConst | stringConst
  }

  private def funcParamExp: Rule1[Expression] = rule{
    (parenExp | identifier) ~ spaces ~ oneOrMore((parenExp | ifElseExp | identifier | constExp) ~ spaces) ~~>
      withContext((funcExp: Expression, rest: List[Expression], ctx) => {
        val loc = calcLoc(ctx)
        val restWithVars = rest.map(e => e -> Type.vendVar)
        val letId = LetId.make
        restWithVars.foldLeft(funcExp){
          case (exp, (exp2, t2)) => Apply(loc, letId, t2, exp, exp2)
        }
      })
  }


  private def identifier: Rule1[Expression] =
    rule{
      identifierStr ~~> withContext((s: String, ctx) => Var(calcLoc(ctx), FuncName(s), Type.vendVar))
    }

  private def spaces: Rule0 = rule {
    zeroOrMore(" " | "\t")
  }


  private def lineFeed: Rule0 = rule {
    zeroOrMore(EOL | EOI)
  }


/*
    private def lineFeed: Rule0 = rule {
      zeroOrMore(EOL)
    }
*/


    private def sink: Rule1[Expression] = {
    def anyChars: Rule1[String] = rule {oneOrMore(!"\"" ~ !EOL ~ !EOI ~ AnyChar) ~> (s => s)}
    rule {
       colZero ~ "\"" ~ anyChars ~ "\"" ~ spaces ~ "=" ~ spaces ~ rightExp ~~>
        withContext((name: String, exp: Expression, ctx) => {
          SinkExp(calcLoc(ctx), LetId.make, FuncName(name), exp.tpe, exp)
        })
    }
  }

  private def letExpr: Rule1[Expression] = rule("Let Expression") {
    (identifierStr ~ spaces ~ str("=") ~ spaces ~ rightExp ~ spaces ~ lineFeed )  ~~>
      withContext((name: String, exp: Expression, ctx) => {
        LetExp(calcLoc(ctx), LetId.make, FuncName(name), false, exp.tpe, exp)})
  }

  private def funcExpr: Rule1[Expression] = rule {
    (identifierStr ~ spaces ~ oneOrMore(identifierStr ~ spaces) ~ str("=") ~ spaces ~ rightExp ~ spaces ~ lineFeed )  ~~>
      withContext((name: String, params: List[String], exp: Expression, ctx) => {

        val loc = calcLoc(ctx)
        val pTypes = params.map(name => FuncName(name) -> Type.vendVar)
        val (wholeExp, ft) = pTypes.foldRight(exp -> Type.vendVar){
          case ((fn, tpe), (e, r)) => FuncExp(loc, fn, tpe, e) -> Expression.tFun(tpe, r)
        }

        LetExp(loc, LetId.make, FuncName(name), true, ft, wholeExp)})
  }


  private def source: Rule1[Expression] = rule {
    (colZero ~ ch('?') ~ identifierStr ~ spaces ~ oneOrMore(EOL)) ~~> withContext((s: String, ctx) => {
      SourceExp(calcLoc(ctx), LetId.make, FuncName(s), Type.vendVar)
    })
  }


  private def NumberConst: Rule1[Expression] = rule("Number") {
    integer ~> withContext((x, ctx) => ValueConst(calcLoc(ctx), DoubleValue(Helpers.toInt(x)), TPrim(PrimDouble)))
  }

  private def calcLoc(ctx: Context[Any]): SourceLoc = SourceFromURL("filename", ctx.getStartIndex -> ctx.getCurrentIndex)
  }

  private lazy val findFences = """(?m)(?:^|\n)```.*\n((?:.*\n)*)(?:^|\n)```""".r

  def hasFences(in: String): Boolean = {
    findFences.findFirstIn(in).isDefined
  }

}
