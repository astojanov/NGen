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

import scala.reflect.SourceContext

import scala.lms.common._
import scala.lms.internal.GenericNestedCodegen

trait NumericMathOps extends Base {

  object Math {

    def Pi    [T:Typ:Fractional](implicit pos: SourceContext) = math_pi[T]
    def E     [T:Typ:Fractional](implicit pos: SourceContext) = math_e[T]

    def ceil  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_ceil(x)
    def floor [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_floor(x)
    def round [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_round(x)
    def exp   [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_exp(x)
    def log   [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_log(x)
    def log10 [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_log10(x)
    def sqrt  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_sqrt(x)
    def sin   [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_sin(x)
    def sinh  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_sinh(x)
    def asin  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_asin(x)
    def cos   [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_cos(x)
    def cosh  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_cosh(x)
    def acos  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_acos(x)
    def tan   [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_tan(x)
    def tanh  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_tanh(x)
    def atan  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) = math_atan(x)
    def atan2 [T:Typ:Fractional](x: Rep[T], y: Rep[T])(implicit pos: SourceContext) = math_atan2(x,y)
    def pow   [T:Typ:Fractional](x: Rep[T], y: Rep[T])(implicit pos: SourceContext) = math_pow(x,y)

    def abs   [A:Typ:Numeric   ](x: Rep[A])(implicit pos: SourceContext) = math_abs(x)
    def max   [A:Typ:Numeric   ](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_max(x,y)
    def min   [A:Typ:Numeric   ](x: Rep[A], y: Rep[A])(implicit pos: SourceContext) = math_min(x,y)
  }

  def math_pi   [T:Typ:Fractional](implicit pos: SourceContext): Rep[T]
  def math_e    [T:Typ:Fractional](implicit pos: SourceContext): Rep[T]

  def math_ceil [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_floor[T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_round[T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_exp  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_log  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_log10[T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_sqrt [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_sin  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_sinh [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_asin [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_cos  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_cosh [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_acos [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_tan  [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_tanh [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_atan [T:Typ:Fractional](x: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_atan2[T:Typ:Fractional](x: Rep[T], y: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_pow  [T:Typ:Fractional](x: Rep[T], y: Rep[T])(implicit pos: SourceContext) : Rep[T]

  def math_abs  [T:Typ:Numeric   ](x: Rep[T])           (implicit pos: SourceContext) : Rep[T]
  def math_max  [T:Typ:Numeric   ](x: Rep[T], y: Rep[T])(implicit pos: SourceContext) : Rep[T]
  def math_min  [T:Typ:Numeric   ](x: Rep[T], y: Rep[T])(implicit pos: SourceContext) : Rep[T]

}

trait NumericMathOpsExp extends NumericMathOps with EffectExp {

  abstract class FDef[T:Typ:Fractional] extends Def[T] {
    val m = manifest[T]
    val f = implicitly[Fractional[T]]
  }

  abstract class NDef[T:Typ:Numeric] extends Def[T] {
    val m = manifest[T]
    val n = implicitly[Numeric[T]]
  }

  case class MathPi   [T:Typ:Fractional]() extends FDef[T]
  case class MathE    [T:Typ:Fractional]() extends FDef[T]
  
  case class MathCeil [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathFloor[T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathRound[T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathExp  [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathLog  [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathLog10[T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathSqrt [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathSin  [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathSinh [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathAsin [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathCos  [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathCosh [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathAcos [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathTan  [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathTanh [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathAtan [T:Typ:Fractional](x: Exp[T]) extends FDef[T]
  case class MathAtan2[T:Typ:Fractional](x: Exp[T], y: Exp[T]) extends FDef[T]
  case class MathPow  [T:Typ:Fractional](x: Exp[T], y: Exp[T]) extends FDef[T]

  case class MathAbs  [A:Typ:Numeric](x: Exp[A])            extends NDef[A]
  case class MathMax  [A:Typ:Numeric](x: Exp[A], y: Exp[A]) extends NDef[A]
  case class MathMin  [A:Typ:Numeric](x: Exp[A], y: Exp[A]) extends NDef[A]


  def math_ceil   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathCeil(x)
  def math_floor  [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathFloor(x)
  def math_round  [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathRound(x)
  def math_exp    [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathExp(x)
  def math_log    [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathLog(x)
  def math_log10  [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathLog10(x)
  def math_sqrt   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathSqrt(x)
  def math_sin    [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathSin(x)
  def math_sinh   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathSinh(x)
  def math_asin   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathAsin(x)
  def math_cos    [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathCos(x)
  def math_cosh   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathCosh(x)
  def math_acos   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathAcos(x)
  def math_tan    [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathTan(x)
  def math_tanh   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathTanh(x)
  def math_atan   [T:Typ:Fractional](x: Exp[T])(implicit pos: SourceContext): Exp[T] = MathAtan(x)
  def math_atan2  [T:Typ:Fractional](x: Exp[T], y: Exp[T])(implicit pos: SourceContext) = MathAtan2(x,y)
  def math_pow    [T:Typ:Fractional](x: Exp[T], y: Exp[T])(implicit pos: SourceContext) = MathPow(x,y)
  
  def math_abs[A:Typ:Numeric](x: Exp[A])(implicit pos: SourceContext): Exp[A] = MathAbs(x)
  def math_max[A:Typ:Numeric](x: Exp[A], y: Exp[A])(implicit pos: SourceContext): Exp[A] = MathMax(x, y)
  def math_min[A:Typ:Numeric](x: Exp[A], y: Exp[A])(implicit pos: SourceContext): Exp[A] = MathMin(x, y)

  def math_pi[T:Typ:Fractional](implicit pos: SourceContext) = MathPi[T]()
  def math_e [T:Typ:Fractional](implicit pos: SourceContext) = MathE [T]()

  override def mirror[A:Typ](d: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (d match {

    case e@MathCeil(x)    => math_ceil(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathFloor(x)   => math_floor(f(x))     (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathRound(x)   => math_round(f(x))     (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathExp(x)     => math_exp(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathPow(x,y)   => math_pow(f(x),f(y))  (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathSin(x)     => math_sin(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathCos(x)     => math_cos(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathAcos(x)    => math_acos(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathLog(x)     => math_log(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathSqrt(x)    => math_sqrt(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathAtan2(x,y) => math_atan2(f(x),f(y))(d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathLog10(x)   => math_log10(f(x))     (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathSinh(x)    => math_sinh(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathAsin(x)    => math_asin(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathCosh(x)    => math_cosh(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathTan(x)     => math_tan(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]], pos)
    case e@MathTanh(x)    => math_tanh(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]], pos)

    case e@MathAbs(x)     => math_abs(f(x))       (d.tp, e.n.asInstanceOf[Numeric[A]], pos)
    case e@MathMin(x,y)   => math_min(f(x),f(y))  (d.tp, e.n.asInstanceOf[Numeric[A]], pos)
    case e@MathMax(x,y)   => math_max(f(x),f(y))  (d.tp, e.n.asInstanceOf[Numeric[A]], pos)

    case Reflect(e@MathCeil(x)   , u, es) => reflectMirrored(Reflect(MathCeil(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathFloor(x)  , u, es) => reflectMirrored(Reflect(MathFloor(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathRound(x)  , u, es) => reflectMirrored(Reflect(MathRound(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathExp(x)    , u, es) => reflectMirrored(Reflect(MathExp(f(x))        (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathPow(x,y)  , u, es) => reflectMirrored(Reflect(MathPow(f(x),f(y))   (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathSin(x)    , u, es) => reflectMirrored(Reflect(MathSin(f(x))        (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathCos(x)    , u, es) => reflectMirrored(Reflect(MathCos(f(x))        (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathAcos(x)   , u, es) => reflectMirrored(Reflect(MathAcos(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathLog(x)    , u, es) => reflectMirrored(Reflect(MathLog(f(x))        (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathSqrt(x)   , u, es) => reflectMirrored(Reflect(MathSqrt(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathAtan2(x,y), u, es) => reflectMirrored(Reflect(MathAtan2(f(x),f(y)) (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathLog10(x)  , u, es) => reflectMirrored(Reflect(MathLog10(f(x))      (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathSinh(x)   , u, es) => reflectMirrored(Reflect(MathSinh(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathAsin(x)   , u, es) => reflectMirrored(Reflect(MathAsin(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathCosh(x)   , u, es) => reflectMirrored(Reflect(MathCosh(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathTan(x)    , u, es) => reflectMirrored(Reflect(MathTan(f(x))        (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathTanh(x)   , u, es) => reflectMirrored(Reflect(MathTanh(f(x))       (d.tp, e.f.asInstanceOf[Fractional[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case Reflect(e@MathAbs(x)    , u, es) => reflectMirrored(Reflect(MathAbs(f(x))        (d.tp, e.n.asInstanceOf[Numeric[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathMin(x,y)  , u, es) => reflectMirrored(Reflect(MathMin(f(x),f(y))   (d.tp, e.n.asInstanceOf[Numeric[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(e@MathMax(x,y)  , u, es) => reflectMirrored(Reflect(MathMax(f(x),f(y))   (d.tp, e.n.asInstanceOf[Numeric[A]]), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(d, f)
  }).asInstanceOf[Exp[A]]

}

trait NumericMathOpsExpOpt extends NumericMathOpsExp {

  override def math_max[A:Typ:Numeric](lhs: Exp[A], rhs: Exp[A])(implicit pos: SourceContext): Exp[A] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Numeric[A]].max(a, b))
    case _ => super.math_max(lhs, rhs)
  }

  override def math_min[A:Typ:Numeric](lhs: Exp[A], rhs: Exp[A])(implicit pos: SourceContext): Exp[A] = (lhs, rhs) match {
    case (Const(a), Const(b)) => Const(implicitly[Numeric[A]].min(a, b))
    case _ => super.math_min(lhs, rhs)
  }

}

trait BaseGenNumericMathOps extends GenericNestedCodegen {
  val IR: NumericMathOpsExp
  import IR._

  def isDouble     [T](m: Typ[T]) = m <:< ManifestTyp(manifest[Double])
  def isFloat      [T](m: Typ[T]) = m <:< ManifestTyp(manifest[Float])
  def isFractional [T](m: Typ[T]) = isDouble(m) || isFloat(m)

}

trait ScalaGenNumericMathOps extends BaseGenNumericMathOps with ScalaGenEffect {
  val IR: NumericMathOpsExp
  import IR._

  private val jMath = "java.lang.Math"

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathCeil(x)    => emitValDef(sym, src"$jMath.ceil($x)")
    case MathFloor(x)   => emitValDef(sym, src"$jMath.floor($x)")
    case MathRound(x)   => emitValDef(sym, src"$jMath.round($x)")
    case MathExp(x)     => emitValDef(sym, src"$jMath.exp($x)")
    case MathLog(x)     => emitValDef(sym, src"$jMath.log($x)")
    case MathLog10(x)   => emitValDef(sym, src"$jMath.log10($x)")
    case MathSqrt(x)    => emitValDef(sym, src"$jMath.sqrt($x)")
    case MathSin(x)     => emitValDef(sym, src"$jMath.sin($x)")
    case MathSinh(x)    => emitValDef(sym, src"$jMath.sinh($x)")
    case MathAsin(x)    => emitValDef(sym, src"$jMath.asin($x)")
    case MathCos(x)     => emitValDef(sym, src"$jMath.cos($x)")
    case MathCosh(x)    => emitValDef(sym, src"$jMath.cosh($x)")
    case MathAcos(x)    => emitValDef(sym, src"$jMath.acos($x)")
    case MathTan(x)     => emitValDef(sym, src"$jMath.tan($x)")
    case MathTanh(x)    => emitValDef(sym, src"$jMath.tanh($x)")
    case MathAtan(x)    => emitValDef(sym, src"$jMath.atan($x)")
    case MathAtan2(x,y) => emitValDef(sym, src"$jMath.atan2($x, $y)")
    case MathPow(x,y)   => emitValDef(sym, src"$jMath.pow($x,$y)")
    case MathAbs(x)     => emitValDef(sym, src"$jMath.abs($x)")
    case MathMax(x,y)   => emitValDef(sym, src"$jMath.max($x, $y)")
    case MathMin(x,y)   => emitValDef(sym, src"$jMath.min($x, $y)")
    case MathPi()       => emitValDef(sym, src"$jMath.PI")
    case MathE()        => emitValDef(sym, src"$jMath.E")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ScalaGenNumericMathOpsApacheCommons extends ScalaGenNumericMathOps {
  val IR: NumericMathOpsExp
  import IR._

  private val fastMath = "org.apache.commons.math.util.FastMath"

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match { // TODO: use java.lang.Math etc...
    case MathCeil(x)    => emitValDef(sym, src"$fastMath.ceil($x)")
    case MathFloor(x)   => emitValDef(sym, src"$fastMath.floor($x)")
    case MathExp(x)     => emitValDef(sym, src"$fastMath.exp($x)")
    case MathLog(x)     => emitValDef(sym, src"$fastMath.log($x)")
    case MathSqrt(x)    => emitValDef(sym, src"$fastMath.sqrt($x)")
    case MathSin(x)     => emitValDef(sym, src"$fastMath.sin($x)")
    case MathCos(x)     => emitValDef(sym, src"$fastMath.cos($x)")
    case MathAcos(x)    => emitValDef(sym, src"$fastMath.acos($x)")
    case MathAtan(x)    => emitValDef(sym, src"$fastMath.atan($x)")
    case MathAtan2(x,y) => emitValDef(sym, src"$fastMath.atan2($x, $y)")
    case MathPow(x,y)   => emitValDef(sym, src"$fastMath.pow($x,$y)")
    case MathAbs(x)     => emitValDef(sym, src"$fastMath.abs($x)")
    case MathMax(x,y)   => emitValDef(sym, src"$fastMath.max($x, $y)")
    case MathMin(x,y)   => emitValDef(sym, src"$fastMath.min($x, $y)")
    case MathPi()       => emitValDef(sym, src"$fastMath.PI")
    case MathE()        => emitValDef(sym, src"$fastMath.E")
    case _ => super.emitNode(sym, rhs)
  }
}


trait CLikeGenNumericMathOps extends BaseGenNumericMathOps with CLikeGenEffect {
  val IR: NumericMathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

    case MathCeil(x)    => emitValDef(sym, src"ceil($x)")
    case MathFloor(x)   => emitValDef(sym, src"floor($x)")
    case MathRound(x)   => emitValDef(sym, src"round($x)")
    case MathExp(x)     => emitValDef(sym, src"exp($x)")
    case MathLog(x)     => emitValDef(sym, src"log($x)")
    case MathSqrt(x)    => emitValDef(sym, src"sqrt($x)")
    case MathSin(x)     => emitValDef(sym, src"sin($x)")
    case MathCos(x)     => emitValDef(sym, src"cos($x)")
    case MathSinh(x)    => emitValDef(sym, src"sinh($x)")
    case MathCosh(x)    => emitValDef(sym, src"cosh($x)")
    case MathAcos(x)    => emitValDef(sym, src"acos($x)")
    case MathTan(x)     => emitValDef(sym, src"tan($x)")
    case MathTanh(x)    => emitValDef(sym, src"tanh($x)")
    case MathAtan(x)    => emitValDef(sym, src"atan($x)")
    case MathAtan2(x,y) => emitValDef(sym, src"atan2($x, $y)")
    case MathPow(x,y)   => emitValDef(sym, src"pow($x,$y)")

    case MathAbs(x)                           => emitValDef(sym, src"fabs($x)")
    case MathMax(x,y) if isFractional(sym.tp) => emitValDef(sym, src"fmax($x, $y)")
    case MathMax(x,y)                         => emitValDef(sym, src"max($x, $y)")
    case MathMin(x,y) if isFractional(sym.tp) => emitValDef(sym, src"fmin($x, $y)")
    case MathMin(x,y)                         => emitValDef(sym, src"min($x, $y)")

    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenNumericMathOps extends CLikeGenNumericMathOps with CudaGenEffect {
  val IR: NumericMathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "CUDART_PI_F")
    case MathE() => emitValDef(sym, "2.7182818284f")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLNumericGenMathOps extends CLikeGenNumericMathOps with OpenCLGenEffect {
  val IR: NumericMathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "M_PI")
    case MathE() => emitValDef(sym, "M_E")
    case MathMax(x,y) => emitValDef(sym, src"max($x, $y)")
    case MathMin(x,y) => emitValDef(sym, src"min($x, $y)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenNumericMathOps extends CLikeGenNumericMathOps with CUnparser {
  val IR: NumericMathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case _: FDef[_] | _: NDef[_] => {
        cApp.addSystemHeader("math.h")
        cApp.addSystemLibrary("m")
      }
      case _ =>
    }

    rhs match {
      case MathPi() => emitValDef(sym, "M_PI")
      case MathE() => emitValDef(sym, "M_E")
      case _ => super.emitNode(sym, rhs)
    }
  }

}
