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
import scala.lms.common.{CGenWhile, WhileExp}

trait WhileMirror extends WhileExp {

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case While(a,b) => While(f(a),f(b))
    case _ => super.mirrorDef(e,f)
  }

  override def mirror[A:Typ](d: Def[A], f: Transformer) (implicit pos: SourceContext): Exp[A] = (d match {
    case Reflect(While(a, b), u, es) =>
      if (f.hasContext)
        __whileDo(f.reflectBlock(a), f.reflectBlock(b))
      else
        reflectMirrored(Reflect(While(f(a), f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case While(a, b) =>
      if (f.hasContext)
        __whileDo(f.reflectBlock(a), f.reflectBlock(b))
      else
        While(f(a), f(b))
    case _ => super.mirror(d, f)
  }).asInstanceOf[Exp[A]]

}


trait CGenWhileMirror extends CGenWhile {
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
      stream.println("while (1) {")
      emitBlock(c)
      stream.println("if (!"+quote(getBlockResult(c))+") break;")
      emitBlock(b)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}


