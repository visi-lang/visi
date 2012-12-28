package visi.core

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/5/12
 * Time: 1:49 PM
 * To change this template use File | Settings | File Templates.
 */
object Expression {
  type CanBeGeneric = Boolean

  type SourcePoint = Int
  type SourceSpan = (SourcePoint, SourcePoint)
  // type SourceInfo = (SourceSpan, String)

  type VarScope = Map[FuncName, Type]

  type LetScope = Map[FuncName, Expression]


  val FuncOperName = "->"

  def tFun(t1: Type, t2: Type): Type = TOper(FuncOperName, List(t1, t2))
}

import Expression._
import net.liftweb.util.Helpers

final case class LetId(id: String)
object LetId {
  def make: LetId = new LetId(Helpers.nextFuncName)
}

sealed trait Prim
case object PrimDouble extends Prim
case object PrimBool extends Prim
case object PrimStr extends Prim


object Type {
  def vendVar: Type = TVar(Helpers.nextFuncName)
}

sealed trait Type

final case class TVar(id: String) extends Type
final case class TPrim(prim: Prim) extends Type {
  override def toString = prim.toString
}
final case class TOper(opr: String, types: List[Type]) extends Type {
  override def toString = types match {
    case a :: b :: Nil if opr == Expression.FuncOperName => a.toString+" -> "+b.toString
    case _ => "TOper("+opr+" "+types.mkString(", ")+")"
  }
}
final case class StructuralType(structure: Map[String, Type]) extends Type

final case class FuncName(name: String)

/**
 * The location of source
 */
sealed trait SourceLoc

final case object NoSourceLoc extends SourceLoc
final case class BuiltInSource(src: String, loc: SourceSpan) extends SourceLoc
final case class SourceFromURL(src: String, loc: SourceSpan) extends SourceLoc

/**
 * An expression
 */
sealed trait Expression {
  def loc: SourceLoc
  def tpe: Type
}

sealed trait HasName extends Expression {
  def name: FuncName
}

sealed trait HasLetId extends Expression {
  def id: LetId
}

final case class LetExp(loc: SourceLoc, id: LetId, name: FuncName, generic: CanBeGeneric, tpe: Type, exp: Expression) extends Expression with HasLetId with HasName {
  override def toString = "Let "+name.name+" ("+tpe+") -> "+exp
}

final case class InnerLet(loc: SourceLoc,
                           tpe: Type, exp1: Expression, exp2: Expression) extends Expression
final case class SinkExp(loc: SourceLoc,
                          id: LetId, name: FuncName, tpe: Type, exp: Expression) extends Expression with HasLetId with HasName
final case class SourceExp(loc: SourceLoc,
                            id: LetId,
                            name: FuncName,
                            tpe: Type) extends Expression with HasLetId with HasName
/*
final case class InvokeMethod(loc: SourceLoc,
                               id: LetId,
                               name: FuncName,
                               tpe: Type) extends Expression with HasLetId
*/

final case class FuncExp(loc: SourceLoc,
                         id: LetId,
                         name: FuncName,
                         tpe: Type,
                         exp: Expression) extends Expression with HasLetId {
  override def toString = "Func "+name.name+" ("+tpe+") -> "+exp
}

final case class Apply(loc: SourceLoc,
                        id: LetId,
                        tpe: Type,
                        exp1: Expression,
                        exp2: Expression) extends Expression with HasLetId {
  override def toString = "Apply("+tpe+" ["+exp1+"] ["+exp2+"])"
}
final case class Var(loc: SourceLoc, id: LetId, name: FuncName, tpe: Type) extends Expression with HasLetId {
  override def toString = "Var "+name.name+" "+tpe
}
final case class BuiltIn(loc: SourceLoc, id: LetId, name: FuncName, tpe: Type, func: Value => Value) extends Expression with HasLetId with HasName
final case class ValueConst(loc: SourceLoc, value: Value, tpe: Type) extends Expression
final case class Group(map: Map[FuncName, Expression]) extends Expression {
  def loc: SourceLoc = sys.error("No Sourceloc for Group")
  def tpe: Type = sys.error("No type for Group")
}


sealed trait Value {
  type T
  def value: T
}
final case class DoubleValue(value: Double) extends Value{type T = Double}
final case class StrValue(value: String) extends Value{type T = String}
final case class BoolValue(value: Boolean) extends Value{type T = Boolean}
final case class FuncValue(value: Value => Value) extends Value{type T = Value => Value}
// UNDEFINED?




