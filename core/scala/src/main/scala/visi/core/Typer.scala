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
  private type VarScope = Map[FuncName, TypePtr]
  private case class CurState(refs: ConcurrentHashMap[Type, TypePtr])
  private case class Depends(whatId: LetId, what: Expression, dependants: Set[LetId], predicates: Set[LetId])
  private type LetMap = ConcurrentHashMap[LetId, Depends]

  def infer(in: Expression): Box[(String, Type)] = {
    val graph = new LetMap

    buildGraph(graph, )

    Empty
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
    case Var(_, name, tpe) =>
      for {
        t <- scope.get(name) ?~ ("Unable to locate function "+name)
        ret <- fresh(state, nonGen, t)
      } yield ret
    case x => ParamFailure("Unable to calculate the type for ", x)
  }
}
