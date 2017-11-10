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

import ch.ethz.acl.commons.compiler.CompilerFlags.CompilerFlags
import ch.ethz.acl.commons.compiler.UArch.{UArch, _}
import ch.ethz.acl.commons.compiler.TuneType.TuneType
import ch.ethz.acl.commons.compiler.OSType.{OSType, _}
import ch.ethz.acl.commons.util.Utilities._

class GCC (exec: Option[String] = None, os: OSType = getOS(), arch: UArch = UArch.UNKNOWN, tune: TuneType = TuneType.UNKNOWN) extends AbstractCompiler {

  val osType   = os
  val uArch    = arch
  val tuneType = tune
  val binPath  = exec match {
    case Some(path) => path
    case _ => os match {
      case WINDOWS => "gcc.exe"
      case _ => "gcc"
    }
  }

  def getCompilerName = "gcc"

  def getCompilerExec () = binPath

  private def uArchString (): String = uArch match {
    case UArch.UNKNOWN => "native"
    case UArch.ARMV8_A_CRC => "armv8-a+crc"
    case _ => uArch.toString().toLowerCase.replace("_", "-")
  }

  override def getFlag(flag: CompilerFlags) = flag match {
    case CompilerFlags.noFormat => List("Wno-format")
    case CompilerFlags.recipAll => List("mrecip=all")
    case CompilerFlags.fastMath => List("ffast-math")
    case CompilerFlags.fma   => List("mfma")
    case CompilerFlags.noVec => List("fno-tree-vectorize")
    case CompilerFlags.xHost if tuneType == TuneType.UNKNOWN => List("march=" + uArchString())
    case CompilerFlags.xHost => List("march=" + uArchString(), "mtune=" + tuneType.convertToString())
    case _ => super.getFlag(flag)
  }
}
