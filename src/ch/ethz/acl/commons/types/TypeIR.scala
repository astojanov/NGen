/**
  *      ___    ______ __       ______ ____   __  ___ __  ___ ____   _   __ _____
  *     /   |  / ____// /      / ____// __ \ /  |/  //  |/  // __ \ / | / // ___/
  *    / /| | / /    / /      / /    / / / // /|_/ // /|_/ // / / //  |/ / \__ \
  *   / ___ |/ /___ / /___   / /___ / /_/ // /  / // /  / // /_/ // /|  / ___/ /
  *  /_/  |_|\____//_____/   \____/ \____//_/  /_//_/  /_/ \____//_/ |_/ /____/
  *
  *  Advanced Computing Laboratory
  *  Department of Computer Science
  *  ETH Zurich, Switzerland
  *
  *  Copyright (C) 2017 Alen Stojanov (astojanov@inf.ethz.ch)
  *
  *  This program is free software: you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program. If not, see http://www.gnu.org/licenses/.
  */

package ch.ethz.acl.commons.types

import java.io.PrintWriter

import ch.ethz.acl.commons.cir.codegen.{CDefinition, CEnum, CUnparser}

import scala.reflect.SourceContext
import ch.ethz.acl.passera.unsigned.{UByte, UInt, ULong, UShort}

import scala.lms.common._
import scala.lms.internal.GenericCodegen

trait TypeRep extends Base
{
  def infix_=:=  (tA: Rep[Type], tB: Rep[Type]): Rep[Boolean]
  def infix_<:<  (tA: Rep[Type], tB: Rep[Type]): Rep[Boolean]
  def infix_&:&  (tA: Rep[Type], tB: Rep[Type]): Rep[Type]
}

trait TypeExp extends TypeRep with BaseExp  { this: BooleanOpsExp =>

  implicit def typeTyp    : Typ[Type]       = manifestTyp[Type]
  implicit def valTyp     : Typ[ValType]    = manifestTyp[ValType]
  implicit def stagedTyp  : Typ[StagedType] = manifestTyp[StagedType]

  case class UnionTypes (types: List[Exp[Type]])     extends Def[Type]
  case class EqualTypes (a: Exp[Type], b: Exp[Type]) extends Def[Boolean]
  case class SubType    (a: Exp[Type], b: Exp[Type]) extends Def[Boolean]
  case class SizeOf     (a: Exp[Type])               extends Def[Int]

  def infix_=:= (tA: Exp[Type], tB: Exp[Type]): Exp[Boolean] = (tA, tB) match {
    case (a, b) if a == b => Const(true)
    case (a, b) => boolean_and(infix_<:<(a, b), infix_<:<(b, a))
  }

  def infix_<:<  (tA: Exp[Type], tB: Exp[Type]): Exp[Boolean] = (tA, tB) match {
    case (Def(UnionTypes(listA)), Def(UnionTypes(listB))) if listA.toSet.subsetOf(listB.toSet) => Const(true)
    case (a, Def(UnionTypes(listB))) if listB.contains(a) => Const(true)
    case (a, b) if a == b => Const(true)
    case _ => SubType(tA, tB)
  }

  def getUnionList(a: Exp[Type]): List[Exp[Type]] = a match {
    case Def(UnionTypes(list)) => list.flatMap(getUnionList).distinct
    case _ => List(a)
  }

  def infix_&:& (list: List[Exp[Type]]): Exp[Type] = {
    val types = list.flatMap(getUnionList).distinct
    types.size match {
      case 1 => types.head
      case _ => UnionTypes(types)
    }
  }

  def infix_&:& (tA: Exp[Type], tB: Exp[Type]): Exp[Type] = {
    infix_&:&(List(tA, tB))
  }

  def size_of(tpe: Exp[Type]): Exp[Int] = SizeOf(tpe)

  override def mirror[A:Typ](e: Def[A], f: Transformer) (implicit pos: SourceContext): Exp[A] = (e match {
    case UnionTypes(a)    => infix_&:&(a.map(a => f(a)))
    case EqualTypes(a, b) => infix_=:=(f(a), f(b))
    case SubType(a, b)    => infix_<:<(f(a), f(b))
    case SizeOf(a)        => size_of(f(a))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]


}

// TODO: (Alen Stojanov) Quick hack here, it has to be fixed in the future
trait TypeIR extends TypeExp { this: BooleanOpsExp =>

  def toType[T](implicit m: Typ[T]): Exp[Type] = m match {
    case _ if m.toString().equals("Nothing") || m.toString().equals("Any") => {
      infix_&:& (ValType.getAll.map(t => Const(t)))
    }
    case _ => Const(ValType.fromTyp[this.type, T](this)(m))
  }

  def infix_| (lhs: Type, rhs: Type): Exp[Type] =
    infix_&:&(List(Const(lhs), Const(rhs)))
  def infix_| (lhs: Exp[Type], rhs: Type): Exp[Type] =
    infix_&:&(List(lhs, Const(rhs)))
  def infix_| (lhs: Type, rhs: Exp[Type]): Exp[Type] =
    infix_&:&(List(Const(lhs), rhs))

  implicit def valTypeToExpType(tpe: ValType): Exp[Type] = Const(tpe)
}


trait GenTypeIR extends GenericCodegen {

  val IR: TypeIR with BooleanOpsExp
  import IR._

  def typeEnumName = "type_t"

  override def remap[A](m: Typ[A]) : String = m match {
    case _ if m <:< TheTyp.toTyp[IR.type, Type](IR)    => typeEnumName
    case _ if m <:< TheTyp.toTyp[IR.type, ValType](IR) => typeEnumName
    case _ if m <:< TheTyp.toTyp[IR.type, RefType](IR) => typeEnumName
    case _ => super.remap(m)
  }

  override def isPrimitiveType[T](m: Typ[T]): Boolean = m.toString() match {
    case s if s.equals(typeEnumName) => true
    case _ => super.isPrimitiveType(m)
  }

  override def quote(x: Exp[Any]) = x match {
    case Const(t: ValType) => t.toString() + "Type"
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case UnionTypes(a) => emitValDef(sym, a.map(quote).mkString(" | "))
    case EqualTypes(a, b) => emitValDef(sym, src"($a == $b)")
    case SubType(a, b) => emitValDef(sym, src"($a & $b) == $a")
    case _ => super.emitNode(sym, rhs)
  }

}

trait CGenTypeIR extends CUnparser with GenTypeIR {

  import IR._

  override def isPrimitiveType(s: String): Boolean = s match {
    case s if s.equals(typeEnumName) => true
    case _ => super.isPrimitiveType(s)
  }

  override def emitGlobalNodes (block: Block[Any]) = {
    val allTypes = ValType.getAll
    val typeEnums = allTypes.map(t => {
      val value = allTypes.indexOf(t)
      t.toString() + "Type = " + value
    }).mkString("," + System.getProperty("line.separator"))
    val definition = "{\n" + typeEnums + "\n}"
    val mappings = "{\n" + allTypes.map(t => {
      "sizeof(" + remap(t.toTyp(IR)) + ")"
    }).mkString("," + System.getProperty("line.separator")) + "\n}"
    cApp.addStructure(CEnum(typeEnumName, definition))
    cApp.addStructure(CDefinition("int size_of[]", mappings))
    super.emitGlobalNodes(block)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case UnionTypes(a) => emitValDef(sym, a.map(quote).mkString(" | "))
      case EqualTypes(a, b) => emitValDef(sym, src"($a == $b)")
      case SubType(a, b) => emitValDef(sym, src"($a & $b) == $a")
      case SizeOf(a) => a match {
        case Const(tpe: ValType) => emitValDef(sym, src"sizeof(${ remap(tpe.toTyp(IR)) })")
        case _ => emitValDef(sym, src"size_of[$a]")
      }
      case _ => super.emitNode(sym, rhs)
    }
  }
}


