package visi.core

/**
 * Does the type inference
 */

import Expression._
import net.liftweb.common._
import net.liftweb.common.Box._
import java.util.concurrent.ConcurrentHashMap
import java.util

object Typer {

  private case class Ref[T](var it: T)

  private type TypePtr = Ref[Type]

  private case class CurState(refs: ConcurrentHashMap[Type, TypePtr])

  case class Depends(whatId: LetId, stack: List[Expression], what: Expression, dependants: Set[LetId], predicates: Set[LetId]) {
    /**
     * The call stack with the outmost part of the call up front
     */
    lazy val rstack = stack.reverse
  }

  private type LetMap = ConcurrentHashMap[LetId, Depends]

  type DependencyMap = Map[LetId, Depends]

  private implicit def lmToDM[A, B](in: ConcurrentHashMap[A, B]): Map[A,B] = {
    import scala.collection.JavaConversions._
    Map(in.keys().toList.map(k => k -> in.get(k)) :_*)
  }

  def infer(in: Map[String, Expression]): Box[(Map[LetId, Type], DependencyMap)] = {
    val graph = new LetMap

    implicit val stuff = new ConcurrentHashMap[Type, TypeAliasInfo]()

    for {
      one <- buildGraph(graph, Map.empty, Nil, Group(in)) ?~ "Hmmmm..."
      two <- buildTypes(Set.empty, Map.empty, Nil, Group(in))
      three <- buildTypes(Set.empty, Map.empty, Nil, Group(in)) // FIXME deal with recursive references
      four <- buildTypes(Set.empty, Map.empty, Nil, Group(in)) // FIXME deal with recursive references
      five <- buildTypes(Set.empty, Map.empty, Nil, Group(in)) // FIXME deal with recursive references
      six <- buildTypes(Set.empty, Map.empty, Nil, Group(in)) // FIXME deal with recursive references
    } yield {
      val rm: DependencyMap = graph
      val ret = (rm.map {
        case (n, d) => n -> d.copy(what = d.what.updateType(prune(d.what.tpe)))
      })
      val newScope = in.foldLeft[Map[LetId, Type]](Map.empty) {
        case (sc, (n, e2: HasLetId)) =>
          sc + (e2.id -> prune(e2.tpe))
        case (sc, _) => sc
      }


      newScope -> ret
    }
  }

  @scala.annotation.tailrec
  private def findLetThingy(lst: List[Expression]): Box[HasLetId with HasName] =
  lst match {
    case Nil => Empty
    case (e: LetExp) :: _ => Full(e)
    case (e: SourceExp) :: _ => Full(e)
    case (e: SinkExp) :: _ => Full(e)
    case _ :: rest => findLetThingy(rest)
  }

  /**
   * Given the dependency map and the predicates computed by findAllToLevelPredicates,
   * create a list of all the Sources with the dependent Sinks and Parameterless Let expressions (ones that need to
   * be recomputed
   * @param info
   * @param predicates
   * @return
   */
  def whatDependsOnSource(info: DependencyMap, predicates: Map[LetId, Set[LetId]]): List[(SourceExp, (List[SinkExp], List[LetExp]))] = {
    val ret: ConcurrentHashMap[SourceExp, (List[SinkExp], List[LetExp])] = new ConcurrentHashMap()

    def getOrCreate(key: SourceExp): (List[SinkExp], List[LetExp]) = ret.get(key) match {
      case null => (Nil, Nil)
      case x => x
    }

    val preds = predicates.map {
      case (k, v) => info(k).what -> v
    }

    preds.foreach {
      case (k: SinkExp, set) =>

        set.map(info(_).what).foreach {
          case e: SourceExp => ret.put(e, getOrCreate(e) match {
            case (sinks, lets) => (k :: sinks, lets)
          })
          case _ =>
        }

        // ignore the Let expressions that take parameters
      case (k@LetExp(_, letId, _, _, _, exp: FuncExp), set) =>

        // this must be a parameterless let expressionOrStruct
      case (k@LetExp(_, letId, _, _, _, _), set) =>
        set.map(info(_).what).foreach {
          case e: SourceExp => ret.put(e, getOrCreate(e) match {
            case (sinks, lets) => (sinks,k :: lets)
          })
          case _ =>
        }


      case _ =>
    }

    ret.toList
  }


  /**
   * Given the DependencyMap returned from infer, find all the top level predicates
   * @param info the dependency map
   * @return a map of LetId to the predicates
   */
  def findAllTopLevelPredicates(info: DependencyMap): Map[LetId, Set[LetId]] = {
    import scala.collection.JavaConversions._

    val first: ConcurrentHashMap[LetId, Set[LetId]] = new ConcurrentHashMap()

    // find all the first level predicates
    for {
      (key, theInfo) <- info
      top <- findLetThingy(theInfo.rstack)
      pred <- theInfo.predicates
      predInfo = info(pred)
      predTop <- findLetThingy(predInfo.rstack)
    } {
      if (top.id != predTop.id || predTop.id == pred) {
        val cur: Set[LetId] = first.get(top.id) match {
          case null => Set()
          case x => x
        }

        first.put(top.id, cur + predTop.id)
      }
    }

    var ret: Map[LetId, Set[LetId]] = Map()

    def deepDive(target: LetId, curSet: util.HashSet[LetId], depth: Int = 0): List[Set[LetId]] = {
      curSet.add(target)
      def doSet(s: Set[LetId]): List[Set[LetId]] = {
        val s2 = s.filter(!curSet.contains(_))
        s2.toList match {
          case Nil => List(s)
          case xs => s :: xs.flatMap(deepDive(_, curSet, depth + 1))
        }
      }

      ret.get(target) match {
        case None =>
          first.get(target) match {
            case null => Nil
            case s if s.isEmpty => Nil
            case s => doSet(s)
          }
        case Some(s) if s.isEmpty => Nil
        case Some(s) => List(s)
      }
    }

    for {
      key <- first.keys()
    } {
      val curSet =  new util.HashSet[LetId]()
      curSet.add(key)
      val set = deepDive(key, curSet)
      ret = ret + (key -> set.foldLeft[Set[LetId]](Set.empty)(_ ++ _))
    }

    ret
  }

  /**
   * Find all the defintions that are recursive
   * @param in the Map returned from findALlTopLevelPredicates
   * @return a List of LetIds that are recursive
   */
  def findRecursive(in: Map[LetId, Set[LetId]]): List[LetId] =
  in.filter {
    case (key, value) => value.contains(key)
  }.keys.toList

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

  private def updateType(source: Type, becomes: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]) {
    val cur = stuff.get(source)
    cur match {
      case null => stuff.put(source, TypeAliasInfo(becomes, Nil))
      case _ => stuff.put(source, TypeAliasInfo(becomes, cur.alias :: cur.history))
    }

  }


  private def occursInType(t1: Type, t2: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Boolean = {
    if (t1 == t2) true else {
      (t1, t2) match {
        case (tv: TVar, opr: TOper) => opr.types.find(tz => occursInType(tv, tz)).isDefined
        case _ => false
      }
    }
  }

  private def unify(t1: Type, t2: Type, stack: List[Expression])(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Box[Type] =
    if (t1 == t2) Full(t1)
    else {
      val t1p = prune(t1)
      val t2p = prune(t2)
      for {

        res <- (((t1p, t2p) match {
          case (x1, x2) if x1 == x2 => Full(x1)
          case (a: TVar, b) =>
            if (occursInType(t1, t2p)) {
              ParamFailure("Recursive Type Unification", (t1, t2, stack))
            }
            else {
              updateType(t1, t2p)
              Full(prune(t1))
            }
          case (b, a: TVar) => unify(a, b, stack)
          case (a@TOper(na, pa), b@TOper(nb, pb)) if na == nb && pa.length == pb.length =>
            val unified: Stream[Box[Type]] = pa.toStream.zip(pb).map {
              case (a, b) => unify(a, b, stack).map(prune(_))
            }
            val reduced = reduce(unified)
            if (reduced.isEmpty) reduced
            else {
              val newU = unified.toList.map(_.openOrThrowException("We tested that all the boxes are Full with the reduce method"))
              val newOpr = TOper(na, newU)
              //updateType(t1, t2p)
              //updateType(t1, newOpr)
              //updateType(t1p, t2p)
              Full(newOpr)
            }
          case (a, b) =>
            ParamFailure("Failed to unify types", (t1, t2, stack))

        }): Box[Type])
      } yield res
    }

  private def prune(t1: Type)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Type = {
    t1 match {
      case TVar(tv) =>
        stuff.get(t1) match {
          case null => t1
          case TypeAliasInfo(nt, hist) =>
            val thing = prune(nt)
            if (thing != nt) {
              stuff.put(t1, TypeAliasInfo(thing, nt :: hist))
            }
            thing
        }
      case TOper(n, lst) => TOper(n, lst.map(prune))
      case x => x
    }
  }

  private def fresh(e: Type, freshMap: ConcurrentHashMap[Type, Type], nongen: Set[Type])(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Type = e match {
    case v if nongen.contains(v) => v
    case v if freshMap.containsKey(v) =>
      freshMap.get(v)
    case TVar(x) => val ret = Type.vendVar
    freshMap.put(e, ret)
    ret
    case TOper(n, lst) => val ret = TOper(n, lst.map(fresh(_, freshMap, nongen)))
    freshMap.put(e, ret)
    ret
    case x => x
  }

  private def tVars(in: Type): Set[Type] = in match {
    case tv: TVar => Set(tv)
    case TOper(_, lst) => lst.foldLeft(Set.empty: Set[Type])((s, t) => s ++ tVars(t))
    case _ => Set.empty
  }


  private def buildTypes(nongen: Set[Type], scope: LetScope, stack: List[Expression], theExp: Expression)(implicit stuff: ConcurrentHashMap[Type, TypeAliasInfo]): Box[Type] = {
    theExp match {
      case LetExp(loc, id, name, generic, tpe, exp) =>
        val newScope = scope + (name -> theExp)
        for {
          expType <- buildTypes(if (generic) nongen else (nongen ++ tVars(tpe)), newScope, theExp +: stack, exp)
          res <- unify(tpe, expType, stack)
        } yield res

      case InnerLet(loc, tpe, exp1, exp2) =>
        val newScope = (exp1: @unchecked) match {
          case le@LetExp(loc, id, name, generic, tpe, exp) =>
            scope + (name -> le)
        }

        for {
          _ <- buildTypes(nongen, newScope, theExp +: stack, exp1)
          ret1 <- buildTypes(nongen, newScope, theExp +: stack, exp2)
          ret <- unify(tpe, ret1, stack)
        } yield ret

      case SinkExp(loc, id, name, tpe, exp) =>
        for {
          rt <- buildTypes(nongen, scope, theExp +: stack, exp)
          ret <- unify(tpe, rt, stack)
        } yield ret

      case SourceExp(loc, id, name, tpe) =>
        Full(prune(tpe))

      case FuncExp(loc, id, name, tpe, exp) =>
        val newScope = scope + (name -> theExp)
        val t2 = prune(tpe)
        for {
          rt <- buildTypes(nongen + t2, newScope, theExp +: stack, exp)
          t3 = prune(t2)
          rrt = prune(rt)
        } yield Expression.tFun(t3, rrt)


      case Apply(loc, id, tpe, exp1, exp2) =>
        val tpe2 = prune(tpe)

        for {
          funType <- buildTypes(nongen, scope, theExp +: stack, exp1)
          argType <- buildTypes(nongen, scope, theExp +: stack, exp2)
          synt = prune(Expression.tFun(prune(argType), prune(tpe2)))
          ret <- unify(synt, prune(funType), stack)
        } yield {
          prune(tpe)
        }

      case vexp@Var(loc, id, name, tpe) =>
        for {
          target1 <- scope.get(name) ?~ ("Unable to find function " + name) ~> theExp
          pruned = prune(target1.tpe)
          targetType = fresh(pruned, new ConcurrentHashMap[Type, Type](), nongen)
          ret <- unify(tpe, targetType, stack)
        } yield ret

      case BuiltIn(loc, id, name, tpe, _) =>
        Full(prune(tpe))

      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        Full(prune(tpe))

      case Group(map) =>
        val (newScope, newNongen) = map.foldLeft((scope, nongen)) {
          case (sc, (_, se: SinkExp)) => sc

          case ((sc, ng), (n, e2: SourceExp)) => (sc + (n -> e2)) -> (ng + e2.tpe)
          case ((sc, ng), (n, e2)) => (sc + (n -> e2)) -> ng
        }

        reduce(map.values.toStream.map(v => buildTypes(newNongen, newScope, stack, v)))

    }
  }

  private def buildGraph(stuff: LetMap, scope: LetScope, stack: List[Expression], theExp: Expression): Box[Boolean] = {
    theExp match {
      case LetExp(loc, id, name, generic, tpe, exp) =>
        val newMap = scope + (name -> theExp)
        if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        buildGraph(stuff, newMap, theExp +: stack, exp)

      case InnerLet(loc, tpe, exp1, exp2) =>
        val newScope = (exp1: @unchecked) match {
          case le@LetExp(loc, id, name, generic, tpe, exp) =>
            scope + (name -> le)
        }

        reduce(Stream(buildGraph(stuff, newScope, theExp +: stack, exp1),
          buildGraph(stuff, newScope, theExp +: stack, exp2)))

      case SinkExp(loc, id, name, tpe, exp) =>
        if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        val newMap = scope + (name -> theExp)
        buildGraph(stuff, newMap, theExp +: stack, exp)

      case SourceExp(loc, id, name, tpe) =>
        if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        Full(true)

      case FuncExp(loc, id, name, tpe, exp) =>
        val newScope = scope + (name -> theExp)
        if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        buildGraph(stuff, newScope, theExp +: stack, exp)

      case Apply(loc, id, tpe, exp1, exp2) =>
        val r1 = buildGraph(stuff, scope, theExp +: stack, exp1)
        if (r1.isDefined) buildGraph(stuff, scope, theExp +: stack, exp2)
        else r1

      case vexp@Var(loc, id, name, tpe) =>
        if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        for {
          target1 <- scope.get(name) ?~ ("Unable to find function " + name) ~> theExp
          target <- Full(target1).asA[HasLetId]
        } yield {
          crossRef(stuff, vexp, target)
          true
        }

      case BuiltIn(loc, id, name, tpe, _) =>
        if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
        Full(true)

      case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
        // Do Nothing
        Full(true)

      case Group(map) =>
        val newMap = map.foldLeft(scope) {
          case (sc, (n, e2)) => sc + (n -> e2)
        }

        map.values.foreach {
          case hl: HasLetId with HasName =>
            stuff.put(hl.id, Depends(hl.id, hl +: theExp +: stack, hl, Set.empty, Set.empty))
          case _ =>
        }

        reduce(map.map(_._2).toStream.map(v => buildGraph(stuff, newMap, theExp +: stack, v)))

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
