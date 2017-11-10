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

import scala.lms.common.{EffectExp, Base}
import scala.reflect.SourceContext
import scala.lms.internal.GenericCodegen

trait Comment extends Base {
  implicit def unit(s: String): Rep[String]
  def comment(s: String): Rep[Unit] = comment(unit(s))
  def comment(s: Rep[Any]): Rep[Unit] = gen_comment(s)
  def gen_comment(s: Rep[Any]): Rep[Unit]
}

trait CommentExp extends Comment with EffectExp {

  implicit def unit(s: String): Rep[String] = Const(s)(manifestTyp[String])

  case class Comment(s: Rep[Any]) extends Def[Unit]

  def gen_comment(s: Rep[Any]) = reflectEffect(Comment(s))

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case Comment(s) => Comment(f(s))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
      case Reflect(Comment(s), u, es) =>
        reflectMirrored(Reflect(Comment(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

}

trait GenComment extends GenericCodegen{
  val IR: CommentExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Comment(s) =>  stream.println("/* " + quote(s) + " */")//emitValDef(sym, "println(" + quote(s) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}