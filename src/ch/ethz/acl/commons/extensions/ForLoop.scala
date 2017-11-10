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
  *                2013 Georg Ofenbeck (ofenbeck@inf.ethz.ch)
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
import scala.lms.common._
import scala.lms.internal.GenericNestedCodegen

trait ForLoopOps extends EffectExp {

  class LoopCanNotBeUnrolled(msg: String) extends RuntimeException(msg)

  /** loop is a syntactic sugar for creating a ForLoop. It assume that a loop is given with upper bound only and
    * as a result, it creates a ForLoop with lower bound set to Const(0), and increment of Const(1).
    *
    * Look up 'forloop' for details
    *
    * @param end    The upper bound of the loop.
    * @param f      The function that describes the loop
    * @param staged Is this loop going to be a staged loop, or should we unroll it?
    * @tparam T     The type of the iterator as well as lower, upper bound and the increment
    * @return       Returns a staged or unrolled ForLoop, or fires an exception.
    */
  def loop[T:Typ:Numeric](end: Rep[T], f: Rep[T] => Rep[Unit], staged: Boolean = true): Rep[Unit] = {
    val (zero, one) = (implicitly[Numeric[T]].zero, implicitly[Numeric[T]].one)
    forloop(Const(zero), end, fresh[T], Const(one), f, staged)
  }

  /** forloop creates a ForLoop, given any Rep[T] => Rep[Unit] function, an iterator symbol, upper and lower bounds as
    * well as an increment. The 'staged' parameter tells whether this loop should be a staged loop or fully unrolled.
    * If staged is set to unroll the loop, but loop can not be unrolled, then this is treated as an error error and
    * LoopCanNotBeUnrolled exception is thrown. The default value of staged is the true, and when not specified, a
    * staged ForLoop is being returned.
    *
    * @param start  The lower bound of the ForLoop
    * @param end    The upper bound of the ForLoop
    * @param i      The iterator symbol used in the ForLoop
    * @param inc    The increment of the ForLoop
    * @param f      The function that describes the loop
    * @param staged Is this loop going to be a staged loop, or should we unroll it?
    * @tparam T     The type of the iterator as well as lower, upper bound and the increment
    * @return       Returns a staged or unrolled ForLoop, or fires an exception.
    */
  def forloop[T:Typ:Numeric](start: Rep[T], end: Rep[T], i: Sym[T], inc: Rep[T], f: Rep[T] => Rep[Unit], staged: Boolean = true): Rep[Unit] = {
    staged match {
      case true  => forloop_avoid_unroll [T](start, end, i, inc, f)
      case false => forloop_force_unroll[T](start, end, i, inc, f)
    }
  }

  /**
    * Similarly to loop, loop_unroll is a syntactic sugar for for_loop unroll and creates a ForLoop. It assumes that
    * the lower bound is Const(0) and the increment is Const(1).
    *
    * Look up 'forloop_unroll' for more details.
    *
    * @param end    The upper bound of the loop.
    * @param f      The function that describes the loop
    * @tparam T     The type of the iterator as well as lower, upper bound and the increment
    * @return       Returns a staged or unrolled ForLoop.
    */
  def loop_unroll[T:Typ:Numeric](end: Rep[T], f: Rep[T] => Rep[Unit]): Rep[Unit] = {
    val (zero, one) = (implicitly[Numeric[T]].zero, implicitly[Numeric[T]].one)
    forloop_unroll(Const(zero), end, fresh[T], Const(one), f)
  }


  /** forloop_unroll creates a ForLoop, given any Rep[T] => Rep[Unit] function, an iterator symbol, upper and lower
    * bounds as well as an increment. forloop_unroll opportunistically unrolls the loop, namely if the lower and upper
    * bounds are constants, and the increment is constant as well, then unrolling is performed. Otherwise, it silently
    * creates a staged loop.
    *
    * @param start  The lower bound of the ForLoop
    * @param end    The upper bound of the ForLoop
    * @param i      The iterator symbol used in the ForLoop
    * @param inc    The increment of the ForLoop
    * @param f      The function that describes the loop
    * @tparam T     The type of the iterator as well as lower, upper bound and the increment
    * @return       Returns a staged or unrolled ForLoop.
    */
  def forloop_unroll [T:Typ:Numeric](start: Rep[T], end: Rep[T], i: Sym[T], inc: Rep[T], f: Rep[T] => Rep[Unit]): Rep[Unit] = {
    (start, end, inc) match {
      case (Const(_), Const(_), Const(_)) => forloop_force_unroll(start, end, i, inc, f)
      case _                              => forloop_avoid_unroll(start, end, i, inc, f)
    }
  }

  protected def forloop_avoid_unroll [T:Typ:Numeric](start: Rep[T], end: Rep[T], i: Sym[T], inc: Rep[T], f: Rep[T] => Rep[Unit]): Rep[Unit]
  protected def forloop_force_unroll [T:Typ:Numeric](start: Rep[T], end: Rep[T], i: Sym[T], inc: Rep[T], f: Rep[T] => Rep[Unit]): Rep[Unit]

}

trait ForLoopExp extends ForLoopOps {

  case class ForLoop[T:Typ:Numeric](start: Exp[T], end: Exp[T], i: Sym[T], inc: Exp[T], body: Block[Unit]) extends Def[Unit] {
    val ma = manifest[T]
    val nu = implicitly[Numeric[T]]
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ForLoop(start, end, i, inc, y) => i :: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case e@ForLoop(start, end, i, inc, y) =>
      ForLoop(f(start), f(end), f(i).asInstanceOf[Sym[Any]], f(inc), f(y))(e.ma, e.nu)
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??


  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ForLoop(start, end, i, inc, y) =>
      forloop_internal(f(start), f(end), f(i).asInstanceOf[Sym[Any]], f(inc), f(y))(e.ma, e.nu)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  protected def forloop_avoid_unroll[T:Typ:Numeric](start: Exp[T], end: Exp[T], i: Sym[T], inc: Exp[T], block: Exp[T] => Exp[Unit]) : Exp[Unit] = {
    val a = reifyEffects(block(i))
    reflectEffect(ForLoop(start, end, i, inc, a), summarizeEffects(a).star)
  }

  protected def forloop_force_unroll[T:Typ:Numeric](start: Exp[T], end: Exp[T], i: Sym[T], inc: Exp[T], block: Exp[T] => Exp[Unit]) : Exp[Unit] = {
    val nu = implicitly[Numeric[T]]
    (start, end, inc) match {
      case (Const(s), Const(e), Const(increment)) => {
        var i = s
        while (nu.lt(i, e)) {
          block(Const(i))
          i = nu.plus(i, increment)
        }
      }
      case _ => throw new LoopCanNotBeUnrolled( s"""
        | Trying to unroll ForLoop($start, $end, $i, $inc, f: Rep[${ manifest[T] }] => Rep[Unit])
        |
        | ===> Loop can not be automatically unrolled.
        """.stripMargin)
    }
  }

  protected def forloop_internal[T:Typ:Numeric](start: Exp[T], end: Exp[T], i: Sym[T], inc: Exp[T], body: Block[Unit]) = {
    ForLoop(start, end, i, inc, body)
  }
}

trait ForLoopExpOpt extends ForLoopExp {
  override protected def forloop_avoid_unroll[T:Typ:Numeric](start: Exp[T], end: Exp[T], i: Sym[T], inc: Exp[T], block: Exp[T] => Exp[Unit]) : Exp[Unit] = {
    val nu = implicitly[Numeric[T]]
    (start, end, inc) match {
      case (Const(s), Const(e), Const(increment)) if nu.lteq(nu.minus(e, s), increment) => block(start)
      case _ => super.forloop_avoid_unroll(start, end, i, inc, block)
    }
  }
}


trait CGenForOps extends CGenEffect with GenericNestedCodegen {

  val IR: ForLoopExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case  ForLoop(qstart, qend, qi, qinc, body) =>
      // stream.println("#pragma ivdep")
      val start = quote(qstart)
      val end   = quote(qend)
      val i     = quote(qi)
      val inc   = quote(qinc)
      val tpe   = remap(qinc.tp)
      stream.println(s"for($tpe $i=$start; $i < $end; $i = $i + ($inc)) {")
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}

trait ForLoopFatExp extends ForLoopExp with LoopsFatExp

trait GenForLoopFat extends BaseGenLoopsFat {
  val IR: ForLoopFatExp
}
