package visi.core

import net.liftweb.common.{Empty, Box}
import org.parboiled.scala._
import net.liftweb.util.Helpers
import org.parboiled.Context

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

    ReportingParseRunner(expressions).run(s.transformIndents(lineCommentStart = "//")).result

  }

  def expressions: Rule1[List[Expression]] = oneOrMore(Expression) ~ zeroOrMore(EOL) ~ EOI

  def BeginComment: Rule0 = rule{ str("/*") }
  def EndComment: Rule0 = rule{ str( "*/") }
  def Comment: Rule0 = rule {BeginComment ~ zeroOrMore(CommentBody) ~ EndComment}
  def CommentBody: Rule0 = rule {Comment | (!BeginComment ~ !EndComment ~ AnyChar)}
  def AnyChar: Rule0 = ANY

  def colZero: Rule0 = toTestAction(ctx => {
    println("col zero test "+ctx.getCurrentIndex+" pos "+ctx.getPosition)
    ctx.getPosition.column == 1
    true
  })

  def EOL: Rule0 = rule{str("\n")}

  def LineComment: Rule0 = rule {"//" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)}

  def Integer: Rule0 = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) }

  def Digits: Rule0 = rule { oneOrMore(Digit) }

  def Digit: Rule0 = rule { "0" - "9" }

  def Expression: Rule1[Expression] =
  (LetExpr | source | sink) ~ zeroOrMore(EOL)

  def Identifier: Rule1[String] = rule {
    (("a" - "z" | "_") ~> ((s: String) => s) ~ zeroOrMore("a" - "z" | "_" | "A" - "Z" | "'" | "0" - "9"  ) ~> ((s: String) => s)) ~~> ((s: String, s2: String) => s + s2)
  }

  def rightExp: Rule1[Expression] = NumberConst

  def spaces: Rule0 = rule {
    zeroOrMore(" " | "\t")
  }

  def lineFeed: Rule0 = zeroOrMore(EOL)


  def sink: Rule1[Expression] = {
    def anyChars: Rule1[String] = oneOrMore(!"\"" ~ !EOL ~ !EOI ~ AnyChar) ~> (s => s)
    rule {
       "\"" ~ anyChars ~ "\"" ~ spaces ~ "=" ~ spaces ~ rightExp ~~>
        withContext((name: String, exp: Expression, ctx) => {
          println("Got sink "+name)
          SinkExp(calcLoc(ctx), LetId.make, FuncName(name), exp.tpe, exp)
        })
    }
  }

  def LetExpr: Rule1[Expression] = rule {
    (Identifier ~ spaces ~ str("=") ~ spaces ~ rightExp ~ spaces ~ lineFeed )  ~~>
      withContext((name: String, exp: Expression, ctx) => {
        println("Got let "+name)
        LetExp(calcLoc(ctx), LetId.make, FuncName(name), false, exp.tpe, exp)})
  }

  def source: Rule1[Expression] = rule {
    "?" ~ Identifier ~ spaces ~ zeroOrMore(EOL ~ EOI) ~~> withContext((s: String, ctx) => {
      println("Got source "+s)
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
