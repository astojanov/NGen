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

import scala.reflect.SourceContext
import scala.lms.common.{Base, BaseExp}
import scala.lms.internal.GenericCodegen

abstract class UnitMerge

trait Merge extends Base {
  def infix_merge(nodes: Rep[Any]*): Rep[UnitMerge]
}

trait MergeExp extends Merge with BaseExp {

  case class Merge(nodes: Seq[Rep[Any]]) extends Def[Unit]

  def infix_merge(nodes: Seq[Exp[Any]]): Exp[Unit] = Merge(nodes)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Merge(a) => infix_merge(a.map(n => f(n)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait GenMerge extends GenericCodegen {

  val IR: MergeExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Merge(nodes) => // do absolutely nothing
    case _ => super.emitNode(sym, rhs)
  }
}
