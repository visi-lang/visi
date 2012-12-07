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

  type SourcePoint = scala.util.parsing.input.Position
  type SourceSpan = (SourcePoint, SourcePoint)
  // type SourceInfo = (SourceSpan, String)

  type VarScope = Map[FuncName, Type]

  type LetScope = Map[FuncName, Expression]


  def funcOperName = "->"

  def tFun(t1: Type, t2: Type): Type = TOper(funcOperName, List(t1, t2))
}

import Expression._

final case class LetId(id: String)

sealed trait Prim
case object PrimDouble extends Prim
case object PrimBool extends Prim
case object PrimStr extends Prim

sealed trait Type

final case class TVar(id: String) extends Type
final case class TPrim(prim: Prim) extends Type
final case class TOper(opr: String, types: List[Type]) extends Type
final case class StructuralType(structure: Map[String, Type]) extends Type

final case class FuncName(name: String)

/**
 * The location of source
 */
sealed trait SourceLoc

final case object NoSourceLoc extends SourceLoc
final case class BuiltInSource(src: String, loc: SourceLoc) extends SourceLoc
final case class SourceFromURL(src: String, loc: SourceLoc) extends SourceLoc

/**
 * An expression
 */
sealed trait Expression {
  def loc: SourceLoc
  def tpe: Type
}

final case class LetExp(loc: SourceLoc, id: LetId, name: FuncName, generic: CanBeGeneric, tpe: Type, exp: Expression) extends Expression
final case class InnerLet(loc: SourceLoc,
                           tpe: Type, exp1: Expression, exp2: Expression) extends Expression
final case class SinkExp(loc: SourceLoc,
                          id: LetId, name: FuncName, tpe: Type, exp: Expression) extends Expression
final case class SourceExp(loc: SourceLoc,
                            id: LetId,
                            name: FuncName,
                            tpe: Type) extends Expression
final case class InvokeMethod(loc: SourceLoc,
                               id: LetId,
                               name: FuncName,
                               tpe: Type) extends Expression

final case class FuncExp(loc: SourceLoc,
                          name: FuncName,
                          tpe: Type,
                          exp: Expression) extends Expression

final case class Apply(loc: SourceLoc,
                        id: LetId,
                        tpe: Type,
                        exp1: Expression,
                        exp2: Expression) extends Expression
final case class Var(loc: SourceLoc, name: FuncName, tpe: Type) extends Expression
final case class BuiltIn(loc: SourceLoc, name: FuncName, tpe: Type, func: Value => Value) extends Expression
final case class ValueConst(loc: SourceLoc, value: Value, tpe: Type) extends Expression
final case class Group(loc: SourceLoc, map: Map[FuncName, Expression], tpe: Type, exp: Expression) extends Expression


sealed trait Value {
  type T
  def value: T
}
final case class DoubleValue(value: Double) extends Value{type T = Double}
final case class StrValue(value: String) extends Value{type T = String}
final case class BoolValue(value: Boolean) extends Value{type T = Boolean}
final case class FuncValue(value: Value => Value) extends Value{type T = Value => Value}
// UNDEFINED?




