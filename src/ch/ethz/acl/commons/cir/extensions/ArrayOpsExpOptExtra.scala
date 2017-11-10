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

package ch.ethz.acl.commons.cir.extensions

import ch.ethz.acl.commons.cir.codegen.CUnparser

import scala.reflect.SourceContext
import scala.lms.common.{ArrayOpsExpOpt, CLikeGenArrayOps, SeqOpsExp}
import scala.lms.internal.NestedBlockTraversal

trait ArrayOpsExpOptExtra extends ArrayOpsExpOpt {
  implicit def seqTyp[T:Typ]: Typ[Seq[T]] = {
    implicit val ManifestTyp(m) = typ[T]
    manifestTyp
  }
}



trait CGenArrayOpsExpOptExtra extends CLikeGenArrayOps with CUnparser { self =>

  val IR: ArrayOpsExpOptExtra
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case ArrayLength (x)       => throw new RuntimeException("Emitting length of an array at the CIR is impossible")
    case ArrayApply  (x, n)    => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
    case ArrayUpdate (x, n, y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
    case d@ArrayNew  (n)       => stream.println(remap(d.m) + " " + quote(sym) + "[" + quote(n) + "];")
    case Reify(s, _, _) =>
    case _ => super.emitNode(sym, rhs)
  }

}

