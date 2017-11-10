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

package ch.ethz.acl.commons.util

import ch.ethz.acl.commons.compiler.ArchType
import ch.ethz.acl.commons.compiler.ArchType.ArchType
import ch.ethz.acl.passera.unsigned.{UByte, UInt, ULong, UShort}

object NumericExtensions {

  implicit object BooleanNumeric extends Integral[Boolean] {
    def quot (x: Boolean, y: Boolean): Boolean = fromInt(toInt(x) / toInt(y))
    def rem  (x: Boolean, y: Boolean): Boolean = fromInt(toInt(x) % toInt(y))
    def fromInt (x: Int): Boolean = if (x == 0) false else true
    def minus   (x: Boolean, y: Boolean): Boolean = x ^ y
    def negate  (x: Boolean): Boolean = !x
    def plus    (x: Boolean, y: Boolean): Boolean = x || y
    def times   (x: Boolean, y: Boolean): Boolean = x && y
    def toDouble(x: Boolean): Double = toInt(x).toDouble
    def toFloat (x: Boolean): Float = toInt(x).toFloat
    def toInt   (x: Boolean): Int = if (x) 1 else 0
    def toLong  (x: Boolean): Long = toInt(x).toLong
    def compare (x: Boolean, y: Boolean): Int = implicitly[Numeric[Int]].compare(toInt(x), toInt(y))
  }

  def numeric[T:Numeric] : Numeric[T] = implicitly[Numeric[T]]


  def convert[U:Numeric, T:Manifest](u: U): T = (manifest[T] match {
    case m if m <:< manifest[Double]  => numeric[U].toDouble(u)
    case m if m <:< manifest[Float]   => numeric[U].toFloat (u)
    case m if m <:< manifest[Char]    => numeric[U].toLong  (u).toChar
    case m if m <:< manifest[Boolean] => numeric[U].zero != u
    case m if m <:< manifest[Long]    => numeric[U].toLong(u)
    case m if m <:< manifest[Int]     => numeric[U].toInt(u)
    case m if m <:< manifest[Short]   => numeric[U].toInt(u).toShort
    case m if m <:< manifest[Byte]    => numeric[U].toInt(u).toByte
    case m if m <:< manifest[ULong]   => ULong (numeric[U].toLong(u))
    case m if m <:< manifest[UInt]    => UInt  (numeric[U].toInt (u))
    case m if m <:< manifest[UShort]  => UShort(numeric[U].toInt (u).toShort)
    case m if m <:< manifest[UByte]   => UByte (numeric[U].toInt (u).toByte)
  }).asInstanceOf[T]

  def manifestToNumeric[T](m: Manifest[T]): Option[Numeric[T]] = (m match {
    case m if m <:< manifest[Double]  => Some(numeric[Double])
    case m if m <:< manifest[Float]   => Some(numeric[Float])
    case m if m <:< manifest[Char]    => Some(numeric[Char])
    case m if m <:< manifest[Boolean] => Some(numeric[Boolean])
    case m if m <:< manifest[Long]    => Some(numeric[Long])
    case m if m <:< manifest[Int]     => Some(numeric[Int])
    case m if m <:< manifest[Short]   => Some(numeric[Short])
    case m if m <:< manifest[Byte]    => Some(numeric[Byte])
    case m if m <:< manifest[ULong]   => Some(numeric[ULong])
    case m if m <:< manifest[UInt]    => Some(numeric[UInt])
    case m if m <:< manifest[UShort]  => Some(numeric[UShort])
    case m if m <:< manifest[UByte]   => Some(numeric[UByte])
    case _ => None
  }).asInstanceOf[Option[Numeric[T]]]

  def bitWidth[T](arch: ArchType, m: Manifest[T]): Int = arch match {
    case ArchType.x86_64 | ArchType.x86 => m match {
      case m if m <:< manifest[Double]  => 64
      case m if m <:< manifest[Float]   => 32
      case m if m <:< manifest[Char]    => 8
      case m if m <:< manifest[Long]    => 64
      case m if m <:< manifest[Int]     => 32
      case m if m <:< manifest[Short]   => 16
      case m if m <:< manifest[Byte]    => 8
      case m if m <:< manifest[ULong]   => 64
      case m if m <:< manifest[UInt]    => 32
      case m if m <:< manifest[UShort]  => 16
      case m if m <:< manifest[UByte]   => 8
      case _ => 0
    }
  }

}