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

package ch.ethz.acl.commons.types

import scala.reflect.SourceContext
import scala.lms.common._

trait TypeUnionMerge extends TypeIR
  with EqualExpBridgeOpt
  with BooleanOpsExpOpt
  with OrderingOpsExpOpt
  with IfThenElseExpOpt
  with VariablesExpOpt
{

//  private var typeMergeEnabled = true
//
//  def setTypeMergeEnabled(v: Boolean) = {
//    typeMergeEnabled = v
//  }
//
//  override def ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: Block[T], elsep: Block[T])(implicit pos: SourceContext): Exp[T] = {
//    if (typeMergeEnabled && manifest[T] <:< manifest[Type]) {
//      infix_&:&(List(thenp.res.asInstanceOf[Exp[Type]], elsep.res.asInstanceOf[Exp[Type]]))
//    } else super.ifThenElse[T](cond, thenp, elsep)
//  }.asInstanceOf[Exp[T]]
//
//  // drop any writes (incl. if/else) to symbols that are not used
//  override def pruneContext(ctx: List[Exp[Any]]): List[Exp[Any]] = if (typeMergeEnabled) {
//    val allocs = ctx.collect { case s@Def(Reflect(_,u,_)) if mustMutable(u) => s }
//    ctx.filterNot { case s@Def(Reflect(_,u,_)) => allocs exists (e => s == e || u.mayWrite == List(e)) }
//  } else super.pruneContext(ctx)
//
//  // find definitions of symbols inside if/else, replace with Union
//  override implicit def readVar[T:Manifest](v: Var[T])(implicit pos: SourceContext) : Exp[T] = {
//    if (typeMergeEnabled && (context ne null) && (manifest[T] <:< manifest[Type])) {
//      // find the last modification of variable v
//      val vs = v.e.asInstanceOf[Sym[Variable[T]]]
//      def findWrite(es: List[Rep[Any]]): Option[Rep[Type]] = if (es.isEmpty) None else es.head match {
//        case w @ Def(Reflect(NewVar(rhs: Exp[Type]), _, _)) if w == vs => Some(rhs)
//        case Def(Reflect(Assign(`v`, rhs: Exp[Type]), _, _)) => Some(rhs)
//        case Def(Reflect(IfThenElse(c,Block(a),Block(b)), u, _)) if mayWrite(u, List(vs)) =>
//          val as = a match { case Def(Reify(a,_,as)) => as case _ => Nil }
//          val bs = b match { case Def(Reify(b,_,bs)) => bs case _ => Nil }
//          val left = findWrite(as.reverse ++ es.tail)
//          val right = findWrite(bs.reverse ++ es.tail)
//          (left,right) match {
//            case (Some(left), Some(right)) => Some(infix_&:&(List(left, right)))
//            case _ => None
//          }
//        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
//        case _ => findWrite(es.tail)
//      }
//      findWrite(context.reverse).getOrElse(super.readVar(v)).asInstanceOf[Rep[T]]
//    } else {
//      super.readVar(v)
//    }
//  }
}
