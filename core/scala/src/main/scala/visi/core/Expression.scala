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

  type VarScope = Map[String, Type]

  type LetScope = Map[String, Expression]


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

/**
 * The location of source
 */
sealed trait SourceLoc

final case object NoSourceLoc extends SourceLoc
final case class BuiltInSource(src: String, loc: SourceSpan) extends SourceLoc
final case class SourceFromURL(src: String, loc: SourceSpan) extends SourceLoc

/**
 * An expressionOrStruct
 */
sealed trait Expression {
  def loc: SourceLoc
  def tpe: Type
  def updateType(nt: Type): Expression
}

sealed trait HasName {
  def name: String
}

sealed trait HasLetId extends Expression {
  def id: LetId
}

final case class LetExp(loc: SourceLoc, id: LetId, name: String, generic: CanBeGeneric, tpe: Type, exp: Expression) extends Expression with HasLetId with HasName {
  override def toString = "Let "+name+" ("+tpe+") -> "+exp
  def updateType(nt: Type): Expression = this.copy(tpe = nt)
}

final case class InnerLet(loc: SourceLoc,
                           tpe: Type, exp1: Expression, exp2: Expression) extends Expression {
  def updateType(nt: Type): Expression = this.copy(tpe = nt)

}
final case class SinkExp(loc: SourceLoc,
                          id: LetId, name: String, tpe: Type, exp: Expression) extends Expression with HasLetId with HasName {
  def updateType(nt: Type): Expression = this.copy(tpe = nt)

}

final case class SourceExp(loc: SourceLoc,
                            id: LetId,
                            name: String,
                            tpe: Type) extends Expression with HasLetId with HasName {
  def updateType(nt: Type): Expression = this.copy(tpe = nt)
}

final case class FuncExp(loc: SourceLoc,
                         id: LetId,
                         name: String,
                         tpe: Type,
                         exp: Expression) extends Expression with HasLetId {
  override def toString = "Func "+name+" ("+tpe+") -> "+exp
  def updateType(nt: Type): Expression = this.copy(tpe = nt)
}

final case class Apply(loc: SourceLoc,
                        id: LetId,
                        tpe: Type,
                        exp1: Expression,
                        exp2: Expression) extends Expression with HasLetId {
  override def toString = "Apply("+tpe+" ["+exp1+"] ["+exp2+"])"
  def updateType(nt: Type): Expression = this.copy(tpe = nt)
}

final case class Var(loc: SourceLoc, id: LetId, name: String, tpe: Type) extends Expression with HasLetId {
  override def toString = "Var "+name+" "+tpe
  def updateType(nt: Type): Expression = this.copy(tpe = nt)
}
final case class BuiltIn(loc: SourceLoc, id: LetId, name: String, tpe: Type, func: StringBuilder => Unit) extends Expression with HasLetId with HasName {
  def updateType(nt: Type): Expression = this.copy(tpe = nt)

}
final case class ValueConst(loc: SourceLoc, value: Value, tpe: Type) extends Expression {
  def updateType(nt: Type): Expression = this.copy(tpe = nt)

}
final case class Group(map: Map[String, Expression]) extends Expression {
  def loc: SourceLoc = sys.error("No Sourceloc for Group")
  def tpe: Type = sys.error("No type for Group")
  def updateType(nt: Type): Expression = sys.error("No type for Group")
}


sealed trait Value {
  type T
  def value: T
  def toJsString: String
}
final case class DoubleValue(value: Double) extends Value{type T = Double

  def toJsString = value.toString
}
final case class StrValue(value: String) extends Value{type T = String
import Helpers._
  def toJsString = value.encJs

}
final case class BoolValue(value: Boolean) extends Value{type T = Boolean

  def toJsString = value.toString
}
//final case class FuncValue(value: Value => Value) extends Value{type T = Value => Value}
// UNDEFINED?

/**
 * The definition of a data structure
 *
 * @param loc the location in the source where the struct is defined
 * @param name the name of the data structure
 * @param typeParams the list of type parameters for this struct
 * @param fields the fields that are available on all sum subtypes of this struct
 * @param sums if the struct is a sum-type, then the sums and the fields describe the sums
 */
final case class Struct(loc: SourceLoc, name: String, typeParams: List[String], fields: List[StructField], sums: List[StructSum]) extends HasName

/**
 * Information about a field
 * @param name the name of the field
 * @param nominalType the nominal type of the field (this will be reified to an actual type in the typer)
 */
final case class StructField(name: String, nominalType: NominalType)

/**
 * The nominal type
 * @param name the name of the type (e.g., Number, String)
 */
final case class NominalType(name: String, params: List[String])

/**
 * A description of a sum type
 */
sealed trait StructSum {
  /**
   * The name of the sum element
   * @return the name of the sum element
   */
  def name: String
}

/**
 * A singleton (e.g., Empty)
 * @param name the name of the singleton
 */
final case class StructSingleton(name: String) extends StructSum

/**
 * A non-singleton sum type thingy (gotta get a better name here). Anyway, something like `Full(item)`
 *
 * @param name the name of the sum type thingy
 * @param fields the fields for the sum type thingy
 */
final case class StructSumItem(name: String, fields: List[StructField]) extends StructSum
