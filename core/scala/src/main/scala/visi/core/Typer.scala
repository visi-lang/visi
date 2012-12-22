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
  private case class Depends(whatId: LetId, what: Expression, dependants: Set[LetId], predicates: Set[LetId])
  private type LetMap = ConcurrentHashMap[LetId, Depends]

  def infer(in: Expression): Box[(String, Type)] = {
    val graph = new LetMap

    buildGraph(graph, Map.empty, Nil,
      in)

    Empty
  }

  private def buildGraph(stuff: LetMap, scope: LetScope, stack: List[Expression], theExp: Expression) {
    theExp match {
    case LetExp(loc: SourceLoc, id: LetId, name: FuncName, generic: CanBeGeneric, tpe: Type, exp: Expression) =>
      val newMap = scope + (name -> theExp)
      stuff.put(id, Depends(id, theExp, Set.empty, Set.empty))
      buildGraph(stuff, newMap, theExp :: stack, exp)

    case InnerLet(loc: SourceLoc,
    tpe: Type, exp1: Expression, exp2: Expression) =>

    case SinkExp(loc: SourceLoc,
    id: LetId, name: FuncName, tpe: Type, exp: Expression) =>
      stuff.put(id, Depends(id, theExp, Set.empty, Set.empty))
      val newMap = scope + (name -> theExp)
      buildGraph(stuff, newMap, theExp :: stack, exp)

    case SourceExp(loc: SourceLoc,
    id: LetId,
    name: FuncName,
    tpe: Type) =>
      stuff.put(id, Depends(id, theExp, Set.empty, Set.empty))

    case InvokeMethod(loc: SourceLoc,
    id: LetId,
    name: FuncName,
    tpe: Type) =>

    case FuncExp(loc: SourceLoc,
    name: FuncName,
    tpe: Type,
    exp: Expression) =>

    case Apply(loc: SourceLoc,
    id: LetId,
    tpe: Type,
    exp1: Expression,
    exp2: Expression) =>

    case Var(loc: SourceLoc, id: LetId, name: FuncName, tpe: Type) =>

    case BuiltIn(loc: SourceLoc, id: LetId, name: FuncName, tpe: Type, _) =>
      stuff.put(id, Depends(id, theExp, Set.empty, Set.empty))

    case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
      // Do Nothing

    case Group(loc: SourceLoc, map: Map[FuncName, Expression], tpe: Type, exp: Expression) =>
        val newMap = map.foldLeft(scope){case (sc, (n, e2)) => sc + (n -> e2)}
        map.values.foreach(e2 => buildGraph(stuff, newMap, theExp:: stack, e2))
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
        t <- scope.get(name) ?~ ("Unable to locate function "+name)
        ret <- fresh(state, nonGen, Ref(t))
      } yield ret
    case x => ParamFailure("Unable to calculate the type for ", x)
  }
}
