package visi.core

import net.liftweb.common._
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.BooleanType

/**
 * The main access to Visi
 */
object Visi {
  def parseAndType(src: String): Box[RunnableInfo] = {
    for {
      (parsed, structs) <- VisiParse.code(src)
      (typed, graph) <- Typer.infer(Compiler.builtIn ++ parsed)
    } yield {
      val p2 = parsed.map {
        case (n, e: HasLetId) => n -> e.updateType(typed(e.id))
        case x => x
      }
      val sources: List[SourceExp] = p2.values.collect{
        case s: SourceExp => s
      }.toList

      val sinks: List[SinkExp] = p2.values.collect{
        case s: SinkExp => s
      }.toList

      RunnableInfo(p2, typed, graph, sources, sinks)
    }
  }

  /**
   * Parses the incoming Visi program into JavaScript
   * @param src The source code
   *
   * @return the executable JavaScript
   */
  def compileWithRunnableInfo(src: String): Box[(String, RunnableInfo)] = {
    for {
      info <- parseAndType(src)
    } yield Compiler.compile(Group(info.functions ++ Compiler.builtIn), info.dependencies) -> info
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
    } yield Compiler.compile(Group(info.functions ++ Compiler.builtIn), info.dependencies)
  }

  final case class RunnableInfo(functions: Map[String, Expression], types: Map[LetId, Type],
                                dependencies: Typer.DependencyMap, sources: List[SourceExp],
                                 sinks: List[SinkExp])
}

