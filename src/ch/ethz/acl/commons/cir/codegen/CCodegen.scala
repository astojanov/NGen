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

package ch.ethz.acl.commons.cir.codegen

import ch.ethz.acl.commons.cir.CIR
import ch.ethz.acl.commons.cir.extensions.{CGenArrayOpsExpOptExtra, CGenHeapArray, GenComment}
import ch.ethz.acl.commons.extensions._
import ch.ethz.acl.commons.types.CGenTypeIR

import scala.lms.common._

trait CCodegen extends JNICodegen
  with CGenTypeIR
  with GenCast
  with GenComment
  with CGenAssertion
  with CGenSwitch
  with CGenExceptionOps
  with CGenArrayOpsExpOptExtra
  with CGenPrimitiveOpsExt
  with CGenForOps

  with CGenPrimitiveOps
  with CLikeGenNumericOps
  with CGenNumericMathOps
  with CLikeGenEqual
  with CLikeGenOrderingOps
  with CLikeGenBooleanOps
  with CGenSaturatedNumericsOps

  with CLikeGenVariablesRename
  with CGenIfThenElseOpt
  with CGenWhileMirror
  with CGenHeapArray
{
  val IR: CIR
}