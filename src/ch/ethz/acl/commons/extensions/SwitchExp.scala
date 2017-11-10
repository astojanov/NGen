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
import scala.lms.common.{BaseExp, CGenEffect, EffectExp}
import scala.lms.internal.GenericNestedCodegen

trait SwitchLift extends BaseExp {

  class SwitchCase[T:Typ] (const: Const[T]) {
    def ->(f: => Rep[Unit]) = (const, () => { f })
  }

  def _case[T <:AnyVal:Typ](v: T) = new SwitchCase[T](Const(v))
  def _case[T:Typ](v: Const[T]) = new SwitchCase[T](v)

  object _default {
    def ->(f: => Rep[Unit]) = () => { f }
  }

  def switch[T:Typ](exp: Rep[T])(casesSeq: (Const[T], () => Rep[Unit])*)(default: () => Rep[Unit]): Rep[Unit] = {
    switch(exp, casesSeq.toList, default)
  }

  def switch[T:Typ](exp: Rep[T], cases: List[(Const[T], () => Rep[Unit])], default: () => Rep[Unit]): Rep[Unit]

}

trait SwitchExp extends SwitchLift with EffectExp {

  case class Switch[T:Typ](exp: Exp[T], cases: List[(Const[T], Block[Unit])], default: Block[Unit]) extends Def[Unit]

  def switch[T:Typ](exp: Exp[T], cases: List[(Const[T], () => Exp[Unit])], default: () => Exp[Unit]): Exp[Unit] = {
    val reifiedCases = cases map {
      case (c, f) => (c, reifyEffectsHere(f()))
    }
    switch(exp, reifiedCases, reifyEffectsHere(default()))
  }

  def switch[T:Typ](exp: Exp[T], cases: List[(Const[T], Block[Unit])], default: Block[Unit]): Exp[Unit] = {
    val effects = (cases map {
      case (_, f) => summarizeEffects(f)
    }) ::: List(summarizeEffects(default))
    val summary = effects.tail.foldLeft(effects.head)((a, b) => a orElse b)
    reflectEffectInternal(Switch(exp, cases, default), summary)
  }

  override def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case Switch(exp, cases, default) => {
      val fc = cases map { case (c, b) => (c, f(b)) }
      Switch(f(exp), fc, f(default))(exp.tp)
    }
    case _ => super.mirrorDef(e,f)
  }


  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Reflect(Switch(exp, cases, default), u, es) => if (f.hasContext) {
      val fc = cases map { case (c, b) => (c, () => f.reflectBlock(b)) }
      switch (f(exp), fc, () => f.reflectBlock(default))(exp.tp)
    } else {
      val fc = cases map { case (c, b) => (c, f(b)) }
      val d = Switch(f(exp), fc, f(default))(exp.tp)
      reflectMirrored(Reflect(d, mapOver(f, u), f(es)))(mtype(manifest[A]), pos)
    }
    case Switch(exp, cases, default) => if (f.hasContext) {
      val fc = cases map { case (c, b) => (c, () => f.reflectBlock(b)) }
      switch (f(exp), fc, () => f.reflectBlock(default))(exp.tp)
    } else {
      // FIXME: should apply pattern rewrites (ie call smart constructor)
      val fc = cases map { case (c, b) => (c, f(b)) }
      Switch(f(exp), fc, f(default))(exp.tp)
    }
    case _ => super.mirror(e,f)
  }


  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case Switch(exp, cases, default) => {
      (cases flatMap { case (_, f) => syms(f) }) ::: syms(default)
    }
    case _ => super.aliasSyms(e)
  }

  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case Switch(exp, cases, default) => Nil
    case _ => super.containSyms(e)
  }

  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case Switch(exp, cases, default) => Nil
    case _ => super.extractSyms(e)
  }

  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case Switch(exp, cases, default) => Nil
    case _ => super.copySyms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Switch(exp, cases, default) => {
      (cases flatMap { case (_, f) => effectSyms(f) }) ::: effectSyms(default)
    }
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case Switch(exp, cases, default) => {
      freqNormal(exp) ++ (cases flatMap { case (_, f) => freqCold(f) }) ++ freqCold(default)
    }
    case _ => super.symsFreq(e)
  }
}


trait SwitchExpOpt extends SwitchExp {

  override def switch[T:Typ](exp: Exp[T], cases: List[(Const[T], () => Exp[Unit])], default: () => Exp[Unit]): Exp[Unit] = {
    exp match {
      case Const(c1) => cases.find{
        case (c2, _) if c1.equals(c2) => true
        case _ => false
      } match {
        case Some(c) => c._2()
        case None => default()
      }
      case _ => super.switch(exp, cases, default)
    }
  }

}

trait CGenSwitch extends CGenEffect with GenericNestedCodegen {

  val IR: SwitchExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Switch(exp, cases, default)  => {
      stream.println("switch (" + quote(exp) + ") {")
      cases.foreach({
        case (const, Block(Const(x: Unit))) => {
          stream.println("case " + quote(const) + ": break;")
        }
        case (const, block) => {
          stream.println("case " + quote(const) + ":")
          stream.println("{")
          emitBlock(block)
          stream.println("break;")
          stream.println("};")
        }
      })
      stream.println("default:")
      stream.println("{")
      emitBlock(default)
      stream.println("};")
      stream.println("}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}
