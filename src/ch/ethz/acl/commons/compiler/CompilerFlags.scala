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

object CompilerFlags extends Enumeration {
 type CompilerFlags = Value
  val cstd = Value
  val xHost = Value
  val Ofast = Value
  val O0 = Value
  val O1 = Value
  val O2 = Value
  val O3 = Value
  val fPIC = Value
  val shared = Value
  val c = Value
  val o = Value
  val w = Value
  val I = Value
  val L = Value
  val l = Value
  val flto = Value
  val noVec = Value

 val fastMath = Value
 val recipAll = Value

 val m64 = Value
 val m32 = Value

 val fma = Value
 val noFMA = Value

  // Other options:
 val noMultibyteChars = Value
 val noFormat = Value
}
