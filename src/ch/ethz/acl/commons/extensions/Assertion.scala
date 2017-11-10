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

import ch.ethz.acl.commons.cir.codegen.CUnparser
import ch.ethz.acl.commons.traversals.NodePreference
import scala.reflect.SourceContext
import scala.lms.common.EffectExp
import scala.lms.internal.GenericCodegen

trait Assertion extends EffectExp {

  case class Assert(cond: Exp[Boolean], msg: Exp[String]) extends Def[Unit]

  def assert_cond(cond: Exp[Boolean], msg: Exp[String]): Exp[Unit] = cond match {
    case Const(true) => // do nothing
    case Const(false) => throw new RuntimeException("Assertion failed: " + msg)
    case _ => reflectEffect(Assert(cond, msg))
  }

  override def mirror[A:Typ](d: Def[A], f: Transformer) (implicit pos: SourceContext): Exp[A] = (d match {
    case Assert(cond, msg) => assert_cond(f(cond), f(msg))
    case Reflect(Assert(cond, msg), u, es) =>
      reflectMirrored(Reflect(Assert(f(cond), msg), mapOver(f,u), f(es))) // (mtype(manifest[A]), pos)
    case _                    => super.mirror(d, f)
  }).asInstanceOf[Exp[A]]

}

trait AssertionTraversal extends NodePreference {

  val IR: Assertion
  import IR._

  override def minRank () = super.minRank() + 1

  override def getRank(s: Stm): Int = s match {
    case TP(_, Assert(cond, msg))                 => super.minRank()
    case TP(_, Reflect(Assert(cond, msg), u, es)) => super.minRank()
    case _ => super.getRank(s)
  }
}


trait GenAssertion extends GenericCodegen with AssertionTraversal {

  val IR: Assertion
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Assert(cond, msg) => msg match {
        case Const(str) if str.trim.equals("") => stream.println("assert(" + quote(cond) + ");")
        case _ => stream.println("assert(" + quote(cond) + " && " + quote(msg) + ");")
    }
    case Reflect(Assert(cond, msg), _, _) => msg match {
      case Const(str) if str.trim.equals("") => stream.println("assert(" + quote(cond) + ");")
      case _ => stream.println("assert(" + quote(cond) + " && " + quote(msg) + ");")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenAssertion extends GenAssertion with CUnparser {

  val IR: Assertion
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case Assert(_, _) | Reflect(Assert(_, _), _, _) => {
        cApp.addSystemHeader("assert.h")
      }
      case _ =>
    }
    super.emitNode(sym, rhs)
  }

}