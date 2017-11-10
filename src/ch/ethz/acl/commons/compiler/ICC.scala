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
import ch.ethz.acl.commons.compiler.OSType.{OSType, _}
import ch.ethz.acl.commons.util.Utilities._


class ICC (exec: Option[String] = None, os: OSType = getOS()) extends AbstractCompiler {

  val osType   = os
  val uArch    = UArch.UNKNOWN
  val tuneType = TuneType.UNKNOWN
  val binPath  = exec match {
    case Some(path) => path
    case _ => os match {
      case WINDOWS => "icl.exe"
      case _ => "icc"
    }
  }

  def getCompilerName = os match {
    case WINDOWS => "Intel(R) C++ Compiler"
    case _ => "icc"
  }

  def getCompilerExec () = binPath

  override def toOpts(sList: List[String]) = os match {
    case WINDOWS => sList.map(s => {
      if (!s.trim.equals("")) " /" + s else ""
    }).mkString("")
    case _ => super.toOpts(sList)
  }

  override def getFlag(flag: CompilerFlags) = (os, flag) match {
    case (_, CompilerFlags.w) => List("")
    case (LINUX, CompilerFlags.noMultibyteChars) => List("no-multibyte-chars")
    case (LINUX, CompilerFlags.noFMA)            => List("no-fma")
    case (MAC, CompilerFlags.shared)             => List("dynamiclib")
    case (WINDOWS, CompilerFlags.xHost)          => List("QxHost")
    case (WINDOWS, CompilerFlags.noVec)          => List("Qvec-")
    case (LINUX, CompilerFlags.noVec)            => List("no-vec")
    case _ => super.getFlag(flag)
  }
}
