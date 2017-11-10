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

import ch.ethz.acl.commons.types.TheTyp
import ch.ethz.acl.commons.util.NumericExtensions._

import scala.reflect.SourceContext
import scala.lms.common.{Base, BaseExp}
import scala.lms.internal.GenericCodegen


trait Cast extends Base {
  def infix_cast[T:Typ](s: Rep[Any]): Rep[T]
}

trait CastExp extends Cast with BaseExp {

  case class Cast[T:Typ](s: Exp[Any]) extends Def[T] {
    val m = manifest[T]
  }

  case class ReinterpretCast[T:Typ](s: Exp[Any]) extends Def[T] {
    val m = manifest[T]
  }

  def infix_cast      [T:Typ](s: Exp[Any]): Exp[T] = Cast[T](s)
  def reinterpret_cast[T:Typ](s: Exp[Any]): Exp[T] = ReinterpretCast[T](s)


  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Cast(s) => infix_cast(f(s))(e.m)
    case e@ReinterpretCast(s) => reinterpret_cast(f(s))(e.m)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait CastExpOpt extends CastExp { self =>

  override def infix_cast[T](s: Exp[Any])(implicit m: Typ[T]): Exp[T] = s match {
    case _ if m <:< s.tp && s.tp <:< m => s.asInstanceOf[Exp[T]]
    case Const(v) => {
      val ms = TheTyp.toManifest(self)(s.tp)
      val mt = TheTyp.toManifest(self)(m)
      (manifestToNumeric(ms), manifestToNumeric(mt)) match {
        case (Some(n), Some(_)) => Const(convert(v)(n, mt))
        case _ => super.infix_cast[T](s)
      }
    }
    case _ => super.infix_cast[T](s)
  }
}

trait GenCast extends GenericCodegen {

  val IR: CastExp
  import IR._

  // (*(int32_t *) &u1

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Cast(s) => emitValDef(sym, "(" + remap(sym.tp) + ") " + quote(s))
    case e@ReinterpretCast(s) => emitValDef(sym, s"(*(${remap(sym.tp)}*)&${quote(s)})")
    case _ => super.emitNode(sym, rhs)
  }
}
