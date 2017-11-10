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

import ch.ethz.acl.passera.unsigned._
import ch.ethz.acl.commons.types.ValTypeObjects._
import ch.ethz.acl.commons.util.Utilities._
import ch.ethz.acl.commons.util.NumericExtensions._

import scala.lms.common.BaseExp

class ValTypeNotFound(msg: String) extends RuntimeException(msg)

sealed abstract class StagedType

sealed abstract class Type {
  def isValType  (): Boolean
  def isRefType  (): Boolean
}

abstract class RefType extends Type {
  def isValType (): Boolean = false
  def isRefType (): Boolean = true
}

abstract class ValType extends Type {
  ValType.addType(this)
  def toManifest   () : Manifest[Any]
  def toNumeric    () : Numeric [Any]
  def toIntegral   () : Integral[Any]
  def toFractional () : Fractional[Any]
  def toOrdering   () : Ordering[Any]
  def isValType    () : Boolean = true
  def isRefType    () : Boolean = false
  def isIntegral   () : Boolean
  def toTyp[T <: BaseExp](IR: T): IR.Typ[Any] = {
    IR.ManifestTyp(toManifest())
  }
  override def toString() = manifestSimple(toManifest())
}

object ValTypeObjects {

  private def createType[T:Manifest:Numeric:Ordering](valIntegral: Option[Integral[Any]], valFractional: Option[Fractional[Any]]) = new ValType {
    def toManifest   () : Manifest[Any]   = manifest[T].asInstanceOf[Manifest[Any]]
    def toNumeric    () : Numeric [Any]   = numeric [T].asInstanceOf[Numeric [Any]]
    def toOrdering   () : Ordering[Any]   = implicitly[Ordering[T]].asInstanceOf[Ordering[Any]]
    def isIntegral   () : Boolean         = valIntegral.nonEmpty
    def toIntegral   () : Integral[Any]   = valIntegral.get
    def toFractional () : Fractional[Any] = valFractional.get
  }

  private def getIntegral  [T](implicit m: Integral[T])   = Some(m.asInstanceOf[Integral[Any]])
  private def getFractional[T](implicit m: Fractional[T]) = Some(m.asInstanceOf[Fractional[Any]])

  val DoubleType  =  createType [Double ](None, getFractional[Double])
  val FloatType   =  createType [Float  ](None, getFractional[Float])
  val CharType    =  createType [Char   ](getIntegral[Char], None)
  val BooleanType =  createType [Boolean](getIntegral[Boolean], None)
  val LongType    =  createType [Long   ](getIntegral[Long], None)
  val IntType     =  createType [Int    ](getIntegral[Int], None)
  val ShortType   =  createType [Short  ](getIntegral[Short], None)
  val ByteType    =  createType [Byte   ](getIntegral[Byte], None)
  val ULongType   =  createType [ULong  ](getIntegral[ULong], None)(manifest[ULong], implicitly[Numeric[ULong]], ULongOrdering)
  val UIntType    =  createType [UInt   ](getIntegral[UInt], None)(manifest[UInt], implicitly[Numeric[UInt]], UIntOrdering)
  val UShortType  =  createType [UShort ](getIntegral[UShort], None)(manifest[UShort], implicitly[Numeric[UShort]], UShortOrdering)
  val UByteType   =  createType [UByte  ](getIntegral[UByte], None)(manifest[UByte], implicitly[Numeric[UByte]], UByteOrdering)

  val primitiveOrdering = List (
    DoubleType,
    FloatType,
    ULongType,
    LongType,
    UIntType,
    IntType,
    UShortType,
    ShortType,
    UByteType,
    ByteType,
    CharType,
    BooleanType
  )
}

object ValType {

  def isUnsigned (a: ValType): Boolean = a match {
    case BooleanType | ULongType | UIntType => true
    case UShortType  | UByteType => true
    case _ => false
  }

  def getSuperior(a: ValType) = {
    primitiveOrdering.indexOf(a) match {
      case -1 | 0 => a
      case idx => primitiveOrdering(idx - 1)
    }
  }

  def isSuperior(a: ValType, b: ValType): Boolean = {
    val aIdx = primitiveOrdering.indexOf(a)
    val bIdx = primitiveOrdering.indexOf(b)
    aIdx < bIdx
  }

  def getDominant(a: ValType, b: ValType): ValType = {
    val aIdx = primitiveOrdering.indexOf(a)
    val bIdx = primitiveOrdering.indexOf(b)
    if (aIdx < bIdx) a else b
  }

  private var allTypes = List.empty[ValType]
  def addType (t: ValType) = allTypes = allTypes :+ t

  // In this implementation, we don't really need to reference
  // DoubleType and add it to the list of allTypes before.
  // However, this is necessary in order to ensure reference
  // of ValTypeObjects and force the creation of the singleton
  // object. It's a hack to deceive the JVM.
  def getAll = {
    val list = ValTypeObjects.DoubleType :: allTypes
    list.distinct.sortBy(t => t.toManifest().toString())
  }

  def fromTyp[T <: BaseExp, U](IR: T)(tp: IR.Typ[U]): ValType = tp match {
    case IR.ManifestTyp(m) => fromManifest(m)
  }

  def fromManifest[T](implicit m: Manifest[T]): ValType = {
    val allVals = getAll
    allVals.find(t => m <:< t.toManifest()) match {
      case Some(valType) => valType
      case None => throw new ValTypeNotFound(m.toString())
    }
  }

}