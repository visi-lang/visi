package visi.core

import net.liftweb.common._
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.BooleanType

/**
 * The main access to Visi
 */
object Visi {



  def parseAndType(src: String): Box[RunableInfo] = {
    for {
      parsed <- VisiParse.code(src)
      (typed, graph) <- Typer.infer(Compiler.builtIn ++ parsed)
    } yield RunableInfo(parsed, typed, graph)
  }

  /**
   * Parses the incoming Visi program into JavaScript
   * @param src The source code
   *
   * @return the executable JavaScript
   */
  def compile(src: String): Box[String] = {
    for {
      info <- parseAndType(src)
    } yield Compiler.compile(Group(info.functions ++ Compiler.builtIn))
  }

  final case class RunableInfo(functions: Map[FuncName, Expression], types: Map[FuncName, Type], dependencies: Typer.DependencyMap)
}

