package visi.core

import net.liftweb.common.{ParamFailure, Empty, Box}
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

  def code(str: String, showStuff: Boolean = false): Box[List[Expression]] = synchronized {
    // turn the CRLR or CR stuff into LF
    val s = str.replace("\r\n", "\n").replace("\r", "\n")

    // if we find fenced codeblocks, then just parse the stuff inside the fences

    val input: Input = s.transformIndents(lineCommentStart = "//")

    if (showStuff) {
      val lb: ListBuffer[String] = new ListBuffer

      var pos = 0
      var ch: Char = ' '

      while ({ch = input.inputBuffer.charAt(pos); ch != Chars.EOI}) {
        input.inputBuffer.charAt(pos) match {
          case EOI =>
          case c if c >= ' ' && c.toInt <= 127 => lb.append(c.toString)
          case c => lb.append(c.toInt.toString)
        }
        pos += 1
      }

      println("Buffer: "+lb.mkString(", "))

    }

    val res = ReportingParseRunner(expressions).run(input)

    if (res.matched) res.result
    else {
      val stuff = res.parseErrors.collect{
        case i: InvalidInputError => i
      }



      ParamFailure("Failed to parse", stuff.map(x => {
        val st = math.max(0, x.getStartIndex - 5)
        val end = math.min(x.getEndIndex + 5, input.input.length - 1)
        val str = (st to end).map(input.input(_)).mkString("")
        (x.getErrorMessage, x.getFailedMatchers.toArray.toList, x.getStartIndex, x.getEndIndex, x, str)
      }))
    }

  }

  def expressions: Rule1[List[Expression]] = rule("Top level") {
    (fencedExpressions | oneOrMore(expression)) ~ endSpace ~ EOI
  }

  def endSpace: Rule0 = zeroOrMore(EOL | " " | "\t" | comment)

  def expression: Rule1[Expression] =
    endSpace ~ (letExpr | funcExpr | source | sink) ~ endSpace

  def fencedExpressions: Rule1[List[Expression]] =
    oneOrMore(notInFence ~ fence ~ zeroOrMore(spaces ~ expression ~ spaces) ~ fence ~ notInFence) ~~> (x => x.flatten)


  def fence: Rule0 = colZero ~ "```" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)

  def notInFence: Rule0 = zeroOrMore(!fence ~ AnyChar)



  def beginComment: Rule0 = rule("Begin comment"){ str("/*") }
  def endComment: Rule0 = rule("End comment"){ str( "*/") }
  def comment: Rule0 = rule("Comment") {beginComment ~ zeroOrMore(commentBody) ~ endComment ~ optional(EOL ~ DEDENT)}
  def commentBody: Rule0 = rule("Comment Body") {comment | (!beginComment ~ !endComment ~ AnyChar)}
  def AnyChar: Rule0 = ANY

  def colZero: Rule0 = toTestAction(ctx => {
    ctx.getPosition.column == 1
  })

  def EOL: Rule0 = rule{str("\n")}

  def LineComment: Rule0 = rule {"//" ~ zeroOrMore(!EOL ~ AnyChar) ~ (EOL | EOI)}

  def Integer: Rule0 = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) }

  def Digits: Rule0 = rule { oneOrMore(Digit) }

  def Digit: Rule0 = rule { "0" - "9" }


  def identifierStr: Rule1[String] = rule {
    (("a" - "z" | "_") ~> ((s: String) => s) ~ zeroOrMore("a" - "z" | "_" | "A" - "Z" | "'" | "0" - "9"  ) ~> ((s: String) => s)) ~~> ((s: String, s2: String) => s + s2)
  }

  def stringConst: Rule1[Expression] = rule {
    "\"" ~ (zeroOrMore(!EOL ~ !"\"" ~ AnyChar) ~> withContext((s: String, ctx) => ValueConst(calcLoc(ctx),
      StrValue(s), TPrim(PrimStr)))) ~ "\""
  }

  def ifElseExp: Rule1[Expression] = rule {
    spaces ~ "if" ~ spaces ~ rightExp ~ spaces ~ "then" ~ spaces ~ rightExp ~ spaces ~ "else" ~ spaces ~ rightExp ~~>
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

  def operator: Rule1[Expression] = rule("Operator") {
    (spaces ~ ((oneOrMore(anyOf("+-*/&><") | "==" | ">=" | "<=")) ~> (s => s)) ~ spaces) ~~> withContext((s: String, ctx) => Var(calcLoc(ctx), FuncName(s), Type.vendVar))
  }

  def parenExp: Rule1[Expression] = rule {
    (spaces ~ "(" ~ spaces ~ operator ~ spaces ~ ")" ~ spaces) |
    (spaces ~ "(" ~ spaces ~ rightExp ~ spaces ~ operator ~ spaces ~ ")" ~ spaces) ~~> withContext((r: Expression, op: Expression, ctx) => op) | // FIXME partially apply
    ((spaces ~ "(" ~ spaces  ~ operator ~ rightExp ~ spaces ~ spaces ~ ")" ~ spaces)) ~~> withContext((op: Expression, r: Expression, ctx) => op) | // FIXME partially apply
    spaces ~ "(" ~ spaces ~ rightExp ~ spaces ~ ")" ~ spaces
  }

  def operatorExp: Rule1[Expression] = rule("Operator Expression") {
    (parenExp | ifElseExp | constExp | identifier) ~ spaces ~ operator ~ spaces ~ rightExp ~~> ((a,b,c) => b) // FIXME do the right thing

  }

  def rightExp: Rule1[Expression] = {
    def thing = (
      operatorExp |
      parenExp |
      ifElseExp |
      constExp |
      funcParamExp | identifier) ~ spaces
   rule("Right hand expression") {
     (EOL ~ INDENT ~ (letExpr | funcExpr) ~ endSpace ~ rightExp ~ zeroOrMore(EOL) ~ DEDENT) ~~> ((a, b) => b) | // FIXME Group them
     ((letExpr | funcExpr) ~ endSpace ~ rightExp) ~~> ((a, b) => b) | // FIXME Group them
     (EOL ~ INDENT ~ thing ~ spaces ~ zeroOrMore(EOL) ~ DEDENT ~ spaces) | thing
   }
  }

  def constExp: Rule1[Expression] = rule {
    NumberConst | stringConst
  }

  def funcParamExp: Rule1[Expression] = rule("Func Param Exp") {
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
      identifierStr ~? (s => s match {
        case "then" | "else" => false
        case _ => true
      }) ~~> withContext((s: String, ctx) => Var(calcLoc(ctx), FuncName(s), Type.vendVar))
    }

  def spaces: Rule0 = rule("Spaces") {
    zeroOrMore(" " | "\t" | comment)
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
    (identifierStr ~ spaces ~ oneOrMore(identifierStr ~ spaces) ~ str("=") ~ spaces ~ rightExp ~ spaces)  ~~>
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
