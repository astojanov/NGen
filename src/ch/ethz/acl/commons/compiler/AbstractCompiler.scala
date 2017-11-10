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
import ch.ethz.acl.commons.compiler.OSType.OSType
import ch.ethz.acl.commons.compiler.TuneType.TuneType
import ch.ethz.acl.commons.util.Utilities
import Utilities._
import ch.ethz.acl.commons.compiler.UArch.UArch

abstract class AbstractCompiler {

  val osType    : OSType
  val uArch     : UArch
  val tuneType  : TuneType
  val binPath   : String

  val compilerInfo : String = ""

  def getCompilerName () : String
  def getCompilerExec () : String

  def toOpts (sList: List[String]): String = sList.map(s => {
    if (!s.trim.equals("")) " -" + s else ""
  }).mkString("")

  def getFlag(flag: CompilerFlags): List[String] = flag match {
    case CompilerFlags.cstd => List("std=c99")
    case _ => List(flag.toString())
  }

  def existLocally () : Boolean = !(findExec(getCompilerExec()) eq null)

  override def toString() = {
    "compiler: " + getCompilerName() + " running on " + osType + " arch: " + uArch
  }

}
