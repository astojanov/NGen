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

package ch.ethz.acl.commons.extensions

import scala.lms.common.CLikeGenVariables

trait CLikeGenVariablesRename extends CLikeGenVariables {

  import IR._

  def quoteVar(x: Exp[Any]): String = x match {
    case Sym(x) if x >= 0 => "var" + x
    case _ => quote(x)
  }

  def emitVarDefRename(sym: Sym[Variable[Any]], rhs: String): Unit = {
    if(remap(sym.tp) != "void")
      stream.println(remap(sym.tp) + " " + quoteVar(sym) + " = " + rhs + ";")
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ReadVar(Variable(a))             => emitValDef(sym, quoteVar(a))
    case NewVar(init)                     => emitVarDefRename(sym.asInstanceOf[Sym[Variable[Any]]], quote(init))
    case Assign(Variable(a), b)           => stream.println(quoteVar(a) + " = " + quote(b) + ";")
    case VarPlusEquals(Variable(a), b)    => stream.println(quoteVar(a) + " += " + quote(b) + ";")
    case VarMinusEquals(Variable(a), b)   => stream.println(quoteVar(a) + " -= " + quote(b) + ";")
    case VarTimesEquals(Variable(a), b)   => stream.println(quoteVar(a) + " *= " + quote(b) + ";")
    case VarDivideEquals(Variable(a), b)  => stream.println(quoteVar(a) + " /= " + quote(b) + ";")
    case _ => super.emitNode(sym, rhs)
  }

}
