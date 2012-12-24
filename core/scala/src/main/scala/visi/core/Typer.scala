package visi.core

/**
 * Does the type inference
 */

import Expression._
import net.liftweb.common._
import net.liftweb.common.Box._
import java.util.concurrent.ConcurrentHashMap

object Typer {

  private case class Ref[T](var it: T)

  private type TypePtr = Ref[Type]

  // private type VarScope = Map[FuncName, Expression]
  private case class CurState(refs: ConcurrentHashMap[Type, TypePtr])

  private case class Depends(whatId: LetId, stack: List[Expression], what: Expression, dependants: Set[LetId], predicates: Set[LetId])

  private type LetMap = ConcurrentHashMap[LetId, Depends]

  def infer(in: Map[FuncName, Expression]): Box[Map[FuncName, Type]] = {
    val graph = new LetMap

    for {
      one <- buildGraph(graph, Map.empty, Nil, Group(in)) ?~ "Hmmmm..."
      // FIXME now that we've built the dependency graph, let's do type checking
    } yield Map(FuncName("choose") -> TPrim(PrimBool))
  }

  private def reduce[T](bs: Stream[Box[T]]): Box[T] = {
    var ret: Box[T] = Empty

    @scala.annotation.tailrec
    def doAll(in: Stream[Box[T]]): Box[T] = {
      if (in.isEmpty) ret
      else (in.head, in.tail) match {
        case (x: EmptyBox, _) => x
        case (x, rest) => ret = x; doAll(rest)
      }
    }

    doAll(bs)
  }

  private def crossRef(stuff: LetMap, from: HasLetId, to: HasLetId) {
    val fromster = stuff.get(from.id)
    val f2 = fromster.copy(predicates = fromster.predicates + to.id)
    stuff.put(from.id, f2)
    val toster = stuff.get(to.id)
    val t2 = toster.copy(dependants = toster.dependants + from.id)
    stuff.put(to.id, t2)
  }

  private def buildGraph(stuff: LetMap, scope: LetScope, stack: List[Expression], theExp: Expression): Box[Boolean] = {
    theExp match {
      case LetExp(loc, id, name, generic, tpe, exp) =>
        val newMap = scope + (name -> theExp)
        if (!stuff.contains(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        buildGraph(stuff, newMap, theExp :: stack, exp)

      case InnerLet(loc, tpe, exp1, exp2) =>
        val newScope = exp1 match {
          case LetExp(loc, id, name, generic, tpe, exp) =>
            scope + (name -> exp)
          case _ => scope
        }
        reduce(Stream(buildGraph(stuff, newScope, theExp :: stack, exp1),
        buildGraph(stuff, newScope, theExp :: stack, exp2)))

      case SinkExp(loc, id, name, tpe, exp) =>
        if (!stuff.contains(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        val newMap = scope + (name -> theExp)
        buildGraph(stuff, newMap, theExp :: stack, exp)

      case SourceExp(loc, id, name, tpe) =>
        if (!stuff.contains(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        Full(true)

      case FuncExp(loc,id, name, tpe, exp) =>
        val newScope = scope + (name -> theExp)
        if (!stuff.contains(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        buildGraph(stuff, newScope, theExp :: stack, exp)

      case Apply(loc, id, tpe, exp1, exp2) =>
        val r1 = buildGraph(stuff, scope, theExp :: stack, exp1)
        if (r1.isDefined) buildGraph(stuff, scope, theExp :: stack, exp2)
        else r1

      case vexp @ Var(loc, id, name, tpe) =>
        if (!stuff.contains(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        for {
          target1 <- scope.get(name) ?~ ("Unable to find function "+name.name) ~> theExp
          target <- Full(target1).asA[HasLetId]
        } yield {
          crossRef(stuff, vexp, target)
          true
        }

      case BuiltIn(loc, id, name, tpe, _) =>
        if (!stuff.contains(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        Full(true)

      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
      // Do Nothing
        Full(true)

      case Group( map: Map[FuncName, Expression]) =>
        val newMap = map.foldLeft(scope) {
          case (sc, (n, e2)) => sc + (n -> e2)
        }

        map.values.foreach {
          case hl: HasLetId => stuff.put(hl.id, Depends(hl.id, theExp::stack, hl, Set.empty, Set.empty))
          case _ =>
        }

        reduce(map.values.toStream.map(v => buildGraph(stuff, newMap, theExp :: stack, v)))

    }
  }


  /**
   * Unify types
   *
   * @param tpe one side of things to unify
   * @param ptr the other side of things to unify
   * @param state the current mapping of types
   * @return Full if the types unify, otherwise, an error
   */
  private def unify(tpe: Type, ptr: TypePtr, state: CurState): Box[Unit] = Full(())

  private def fresh(state: CurState, nonGen: Boolean, tpe: TypePtr): Box[TypePtr] = Empty

  private def calcType(scope: VarScope, state: CurState, nonGen: Boolean, expr: Expression): Box[TypePtr] =
    expr match {
      case Var(_, _, name, tpe) =>
        for {
          t <- scope.get(name) ?~ ("Unable to locate function " + name)
          ret <- fresh(state, nonGen, Ref(t))
        } yield ret
      case x => ParamFailure("Unable to calculate the type for ", x)
    }
}
