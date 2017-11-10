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

object TuneType extends Enumeration {

  type TuneType = Value

  val arm2 = Value
  val arm250 = Value
  val arm3 = Value
  val arm6 = Value
  val arm60 = Value
  val arm600 = Value
  val arm610 = Value
  val arm620 = Value
  val arm7 = Value
  val arm7m = Value
  val arm7d = Value
  val arm7dm = Value
  val arm7di = Value
  val arm7dmi = Value
  val arm70 = Value
  val arm700 = Value
  val arm700i = Value
  val arm710 = Value
  val arm710c = Value
  val arm7100 = Value
  val arm720 = Value
  val arm7500 = Value
  val arm7500fe = Value
  val arm7tdmi = Value
  val arm7tdmi_s = Value
  val arm710t = Value
  val arm720t = Value
  val arm740t = Value
  val strongarm = Value
  val strongarm110 = Value
  val strongarm1100 = Value
  val strongarm1110 = Value
  val arm8 = Value
  val arm810 = Value
  val arm9 = Value
  val arm9e = Value
  val arm920 = Value
  val arm920t = Value
  val arm922t = Value
  val arm946e_s = Value
  val arm966e_s = Value
  val arm968e_s = Value
  val arm926ej_s = Value
  val arm940t = Value
  val arm9tdmi = Value
  val arm10tdmi = Value
  val arm1020t = Value
  val arm1026ej_s = Value
  val arm10e = Value
  val arm1020e = Value
  val arm1022e = Value
  val arm1136j_s = Value
  val arm1136jf_s = Value
  val mpcore = Value
  val mpcorenovfp = Value
  val arm1156t2_s = Value
  val arm1156t2f_s = Value
  val arm1176jz_s = Value
  val arm1176jzf_s = Value
  val cortex_a5 = Value
  val cortex_a7 = Value
  val cortex_a8 = Value
  val cortex_a9 = Value
  val cortex_a15 = Value
  val cortex_r4 = Value
  val cortex_r4f = Value
  val cortex_r5 = Value
  val cortex_m4 = Value
  val cortex_m3 = Value
  val cortex_m1 = Value
  val cortex_m0 = Value
  val xscale = Value
  val iwmmxt = Value
  val iwmmxt2 = Value
  val ep9312 = Value
  val fa526 = Value
  val fa626 = Value
  val fa606te = Value
  val fa626te = Value
  val fmp626 = Value
  val fa726te = Value

  val UNKNOWN = Value

  class TuneTypeValue(d: Value) {
    def convertToString () : String = d match {
      case _ => d.toString().toLowerCase.replace("_", "-")
    }
  }
  implicit def value2TuneTypeValue (d: Value) = new TuneTypeValue(d)

  def fromString(f: String): TuneType = f.trim.toLowerCase match {
    case fstr => TuneType.values.find(x => x.convertToString().equals(fstr)) match {
      case Some(flag) => flag
      case _ => UNKNOWN
    }
  }
}