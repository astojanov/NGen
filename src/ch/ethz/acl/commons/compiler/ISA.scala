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

package ch.ethz.acl.commons.compiler

import ch.ethz.acl.commons.util.{NumericExtensions, Utilities}

object ISA extends Enumeration {

  type ISA = Value

  val None = Value

  // Major ISAs
  val MMX = Value
  val SSE = Value
  val SSE2 = Value
  val SSE3 = Value
  val SSSE3 = Value
  val SSE41 = Value
  val SSE42 = Value
  val AVX = Value
  val AVX2 = Value
  val FMA = Value
  val KNC = Value
  val AVX512 = Value

  // Other ISAs
  val ADX = Value
  val AES = Value
  val BMI1 = Value
  val BMI2 = Value
  val CLFLUSHOPT = Value
  val CLWB = Value
  val FP16C = Value
  val FSGSBASE = Value
  val FXSR = Value
  val INVPCID = Value
  val LZCNT = Value
  val MONITOR = Value
  val MPX = Value
  val PCLMULQDQ = Value
  val POPCNT = Value
  val PREFETCHWT1 = Value
  val RDPID = Value
  val RDRAND = Value
  val RDSEED = Value
  val RDTSCP = Value
  val RTM = Value
  val SHA = Value
  val TSC = Value
  val VPMADD52 = Value
  val XSAVE = Value
  val XSAVEC = Value
  val XSAVEOPT = Value
  val XSS = Value
  

  def getBitWidth(isa: ISA): Int = isa match {
    case MMX    => 64
    case SSE    => 128
    case SSE2   => 128
    case SSE3   => 128
    case SSSE3  => 128
    case SSE41  => 128
    case SSE42  => 128
    case AVX    => 256
    case AVX2   => 256
    case KNC    => 512
    case AVX512 => 512
  }

  def getInstructionSetVectorSize[T](isa: ISA) (implicit m: Manifest[T]): Int = isa match {
    case None => 1
    case _ => getBitWidth(isa) / NumericExtensions.bitWidth(ArchType.x86_64, m)
  }

}
