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

  private case class Depends(whatId: LetId, stack: List[Expression], what: Expression, dependants: Set[LetId], predicates: Set[LetId]) {
    /**
     * The call stack with the outmost part of the call up front
     */
    lazy val rstack = stack.reverse
  }

  private type LetMap = ConcurrentHashMap[LetId, Depends]

  def infer(in: Map[FuncName, Expression]): Box[Map[FuncName, Type]] = {
    val graph = new LetMap

    implicit val stuff = new ConcurrentHashMap[Type, TypeAliasInfo]()

    for {
      one <- buildGraph(graph, Map.empty, Nil, Group(in)) ?~ "Hmmmm..."
      two <- buildTypes(Set.empty, Map.empty, Nil, Group(in))
    } yield {
      val newScope = in.foldLeft[Map[FuncName, Type]](Map.empty) {
        case (sc, (_, se: SinkExp)) => sc
        case (sc, (n, e2)) => prune(e2.tpe) match {
          case Full(t) => sc + (n -> t)
          case _ => sc
        }
      }

      newScope
    }
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

  private case class TypeAliasInfo(alias: Type, history: List[Type])

  def updateType(source: Type, becomes: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]) {
    val cur = stuff.get(source)
    cur match {
      case null => stuff.put(source, TypeAliasInfo(becomes, Nil))
      case _ => stuff.put(source, TypeAliasInfo(becomes, cur.alias :: cur.history))
    }

  }



  def occursInType(t1: Type, t2: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Boolean = false // FIXME

  def unify(t1: Type, t2: Type, stack: List[Expression])(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Box[Type] =
    if (t1 == t2) Full(t1)
    else {
      for {
        t1p <- prune(t1)
        t2p <- prune(t2)
        res <- (((t1p, t2p) match {
          case (x1, x2) if x1 == x2 => Full(x1)
          case (a: TVar, b) =>
            if (occursInType(t1, t2p)) ParamFailure("Recursive Type Unification", (t1, t2, stack))
            else {
              updateType(t1, t2p)
              prune(t1)
            }
          case (b, a: TVar) => unify(a, b, stack)
          case (a@TOper(na, pa), b@TOper(nb, pb)) if na == nb && pa.length == pb.length =>
            val unified: Stream[Box[Type]] = pa.toStream.zip(pb).map {
              case (a, b) => unify(a, b, stack).flatMap(prune(_))
            }
            val reduced = reduce(unified)
            if (reduced.isEmpty) reduced
            else {
              val newU = unified.toList.map(_.openOrThrowException("We tested that all the boxes are Full with the reduce method"))
              val newOpr = TOper(na, newU)
              updateType(t1, t2p)
              updateType(t1, newOpr)
              updateType(t1p, t2p)
              Full(newOpr)
            }
          case (a, b) =>
            println("Hey t1 "+t1)
            println("Hey t2 "+t2)
            println("---------------")
            ParamFailure("Failed to unify types", (t1, t2, stack))

        }): Box[Type])
      } yield res
    }

  def prune(t1: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Box[Type] = {
    stuff.get(t1) match {
      case null => Full(t1)
      case TypeAliasInfo(nt, hist) =>
        val thing = prune(nt)
        if (thing != Full(nt)) {
          thing.foreach{
            ut =>
              stuff.put(t1, TypeAliasInfo(ut, nt :: hist))
          }
        }
        thing
    }
  }

  def fresh(e: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Type = e match {
    case TVar(x) => Type.vendVar
    case TOper(n, lst) => TOper(n, lst.map(fresh(_)))
    case x => x
  }


  private def buildTypes(nongen: Set[Type], scope: LetScope, stack: List[Expression], theExp: Expression)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Box[Type] = {


    theExp match {
      case LetExp(loc, id, name, generic, tpe, exp) =>
        val newScope = scope + (name -> theExp)
        for {
          expType <- buildTypes(if (generic) nongen else (nongen + tpe), newScope, theExp :: stack, exp)
          res <- unify(tpe, expType, stack)
        } yield res

      case InnerLet(loc, tpe, exp1, exp2) =>
        val newScope = exp1 match {
          case le@ LetExp(loc, id, name, generic, tpe, exp) =>
            scope + (name -> le)
          case _ => scope
        }

        for {
          _ <- buildTypes( nongen, newScope, theExp :: stack, exp1)
          ret1 <- buildTypes(  nongen, newScope, theExp :: stack, exp2)
          ret <- unify(tpe, ret1, stack)
        } yield ret

      case SinkExp(loc, id, name, tpe, exp) =>
        for {
          rt <- buildTypes( nongen, scope, theExp :: stack, exp)
          ret <- unify(tpe, rt, stack)
        } yield ret

      case SourceExp(loc, id, name, tpe) =>
        prune(tpe)

      case FuncExp(loc,id, name, tpe, exp) =>
        val newScope = scope + (name -> theExp)
        for {
          t2 <- prune(tpe)
          rt <- buildTypes( nongen + t2, newScope, theExp :: stack, exp)
          t3 <- prune(t2)
          rrt <- prune(rt)
        } yield Expression.tFun(t3, rrt)


      case Apply(loc, id, tpe, exp1, exp2) =>
       for {
         tpe2 <- prune(tpe)
         funType <- buildTypes( nongen, scope, theExp :: stack, exp1)
         argType <- buildTypes( nongen, scope, theExp :: stack, exp2)
         synt = Expression.tFun(argType, tpe2)
         ret <- unify(synt, funType, stack)
       } yield ret

      case vexp @ Var(loc, id, name, tpe) =>
        for {
          target1 <- scope.get(name) ?~ ("Unable to find function "+name.name) ~> theExp
          targetType = fresh(target1.tpe)
          ret <- unify(tpe, targetType, stack)
        } yield ret

      case BuiltIn(loc, id, name, tpe, _) =>
        prune(tpe)

      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        prune(tpe)

      case Group( map: Map[FuncName, Expression]) =>
        val newScope = map.foldLeft(scope) {
          case (sc, (_, se: SinkExp)) => sc
          case (sc, (n, e2)) => sc + (n -> e2)
        }

        reduce(map.values.toStream.map(v => buildTypes(nongen, newScope, stack, v)))

    }
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
