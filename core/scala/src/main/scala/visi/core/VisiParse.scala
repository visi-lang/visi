package visi.core

import net.liftweb.common.{ParamFailure, Empty, Box}
import org.parboiled.scala._
import net.liftweb.util.Helpers
import org.parboiled.Context
import org.parboiled.errors.InvalidInputError

object VisiParse extends VisiParse

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/5/12
 * Time: 3:01 PM
 * To change this template use File | Settings | File Templates.
 */
class VisiParse extends Parser  {
  def code(str: String): Box[List[Expression]] = synchronized {
    // turn the CRLR or CR stuff into LF
    val s = str.replace("\r\n", "\n").replace("\r", "\n")

    // if we find fenced codeblocks, then just parse the stuff inside the fences

    val res = ReportingParseRunner(expressions).run(s.transformIndents(lineCommentStart = "//"))

    if (res.matched) res.result
    else {
      val stuff = res.parseErrors.collect{
        case i: InvalidInputError => i
      }

      ParamFailure("Failed to parse", stuff.map(x => (x.getErrorMessage, x.getFailedMatchers.toArray.toList, x)))
    }

  }

  def expressions: Rule1[List[Expression]] = rule("Top level") {
    (fencedExpressions | oneOrMore(expression)) ~ zeroOrMore(EOL) ~ EOI
  }

  def fencedExpressions: Rule1[List[Expression]] =
    oneOrMore(notInFence ~ fence ~ zeroOrMore(expression) ~ fence ~ notInFence) ~~> (x => x.flatten)


  def fence: Rule0 = colZero ~ "```" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)

  def notInFence: Rule0 = zeroOrMore(!fence ~ AnyChar)



  def BeginComment: Rule0 = rule("Begin comment"){ str("/*") }
  def EndComment: Rule0 = rule("End comment"){ str( "*/") }
  def Comment: Rule0 = rule("Comment") {BeginComment ~ zeroOrMore(CommentBody) ~ EndComment}
  def CommentBody: Rule0 = rule("Comment Body") {Comment | (!BeginComment ~ !EndComment ~ AnyChar)}
  def AnyChar: Rule0 = ANY

  def colZero: Rule0 = toTestAction(ctx => {
    ctx.getPosition.column == 1
    true
  })

  def EOL: Rule0 = rule{str("\n")}

  def LineComment: Rule0 = rule {"//" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)}

  def Integer: Rule0 = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) }

  def Digits: Rule0 = rule { oneOrMore(Digit) }

  def Digit: Rule0 = rule { "0" - "9" }

  def expression: Rule1[Expression] =
  (letExpr | funcExpr | source | sink) ~ zeroOrMore(EOL)

  def identifierStr: Rule1[String] = rule {
    (("a" - "z" | "_") ~> ((s: String) => s) ~ zeroOrMore("a" - "z" | "_" | "A" - "Z" | "'" | "0" - "9"  ) ~> ((s: String) => s)) ~~> ((s: String, s2: String) => s + s2)
  }

  def stringConst: Rule1[Expression] = rule {
    "\"" ~ (zeroOrMore(!EOL ~ !"\"" ~ AnyChar) ~> withContext((s: String, ctx) => ValueConst(calcLoc(ctx),
      StrValue(s), TPrim(PrimStr)))) ~ "\""
  }

  def ifElseExp: Rule1[Expression] = rule {
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

  def parenExp: Rule1[Expression] = rule {
    spaces ~ "(" ~ spaces ~ rightExp ~ spaces ~ ")" ~ spaces
  }

  def rightExp: Rule1[Expression] =
   rule{ parenExp |
    ifElseExp | constExp |
   funcParamExp | identifier
   }

  def constExp: Rule1[Expression] = rule {
    NumberConst | stringConst
  }

  def funcParamExp: Rule1[Expression] = rule{
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


  def identifier: Rule1[Expression] =
    rule{
      identifierStr ~~> withContext((s: String, ctx) => Var(calcLoc(ctx), FuncName(s), Type.vendVar))
    }

  def spaces: Rule0 = rule {
    zeroOrMore(" " | "\t")
  }

  def lineFeed: Rule0 = zeroOrMore(EOL)


  def sink: Rule1[Expression] = {
    def anyChars: Rule1[String] = oneOrMore(!"\"" ~ !EOL ~ !EOI ~ AnyChar) ~> (s => s)
    rule {
       colZero ~ "\"" ~ anyChars ~ "\"" ~ spaces ~ "=" ~ spaces ~ rightExp ~~>
        withContext((name: String, exp: Expression, ctx) => {
          SinkExp(calcLoc(ctx), LetId.make, FuncName(name), exp.tpe, exp)
        })
    }
  }

  def letExpr: Rule1[Expression] = rule {
    (identifierStr ~ spaces ~ str("=") ~ spaces ~ rightExp ~ spaces ~ lineFeed )  ~~>
      withContext((name: String, exp: Expression, ctx) => {
        LetExp(calcLoc(ctx), LetId.make, FuncName(name), false, exp.tpe, exp)})
  }

  def funcExpr: Rule1[Expression] = rule {
    (identifierStr ~ spaces ~ oneOrMore(identifierStr ~ spaces) ~ str("=") ~ spaces ~ rightExp ~ spaces ~ lineFeed )  ~~>
      withContext((name: String, params: List[String], exp: Expression, ctx) => {
        val loc = calcLoc(ctx)
        val pTypes = params.map(name => FuncName(name) -> Type.vendVar)
        val (wholeExp, ft) = pTypes.foldRight(exp -> Type.vendVar){
          case ((fn, tpe), (e, r)) => FuncExp(loc, fn, tpe, e) -> Expression.tFun(tpe, r)
        }

        LetExp(loc, LetId.make, FuncName(name), true, ft, wholeExp)})
  }

  def source: Rule1[Expression] = rule {
    colZero ~ "?" ~ identifierStr ~ spaces ~ zeroOrMore(EOL ~ EOI) ~~> withContext((s: String, ctx) => {
      SourceExp(calcLoc(ctx), LetId.make, FuncName(s), Type.vendVar)
    })
  }

  def NumberConst: Rule1[Expression] = rule {
    Integer ~> withContext((x, ctx) => ValueConst(calcLoc(ctx), DoubleValue(Helpers.toInt(x)), TPrim(PrimDouble)))
  }

  def calcLoc(ctx: Context[Any]): SourceLoc = SourceFromURL("filename", ctx.getStartIndex -> ctx.getCurrentIndex)


  lazy val findFences = """(?m)(?:^|\n)```.*\n((?:.*\n)*)(?:^|\n)```""".r

  def hasFences(in: String): Boolean = {
    findFences.findFirstIn(in).isDefined
  }

}
