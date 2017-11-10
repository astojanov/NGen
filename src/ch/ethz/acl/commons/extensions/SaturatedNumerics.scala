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

import java.io.PrintWriter

import ch.ethz.acl.commons.cir.codegen.{CFunction, CUnparser}
import ch.ethz.acl.passera.unsigned.{ULong, UInt, UShort, UByte}

import scala.reflect.SourceContext
import scala.lms.common._

trait SaturatedNumericsOps extends Base {
  def saturated_plus  [T:Integral:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def saturated_minus [T:Integral:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def saturated_times [T:Integral:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def saturated_divide[T:Integral:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
  def saturated_power [T:Integral:Typ](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
}

trait SaturatedNumericsOpsExp extends UnsignedPrimitiveOps with SaturatedNumericsOps {

  abstract class SaturatedDef[A:Integral:Typ] extends Def[A] {
    def mev = manifest[A]
    def aev = implicitly[Integral[A]]
  }

  case class Saturate[T:Integral:Typ, U:Numeric:Typ](lhs: Exp[U]) extends SaturatedDef[T] {
    def fmev = manifest[U]
    def faev = implicitly[Numeric[U]]
  }
  case class SaturatedPlus  [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T]) extends SaturatedDef[T]
  case class SaturatedMinus [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T]) extends SaturatedDef[T]
  case class SaturatedTimes [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T]) extends SaturatedDef[T]
  case class SaturatedDivide[T:Integral:Typ](lhs: Exp[T], rhs: Exp[T]) extends SaturatedDef[T]
  case class SaturatedPower [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T]) extends SaturatedDef[T]

  def saturated_cast  [T:Integral:Typ, U:Numeric:Typ](lhs: Exp[U])(implicit pos: SourceContext): Exp[T] = {
    def mev = manifest[T]
    def aev = implicitly[Integral[T]]
    def fmev = manifest[U]
    def faev = implicitly[Numeric[U]]
    Saturate (lhs)(aev, mev, faev, fmev)
  }
  def saturated_plus  [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = SaturatedPlus   (lhs, rhs)
  def saturated_minus [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = SaturatedMinus  (lhs, rhs)
  def saturated_times [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = SaturatedTimes  (lhs, rhs)
  def saturated_divide[T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = SaturatedDivide (lhs, rhs)
  def saturated_power [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = SaturatedPower  (lhs, rhs)

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@Saturate       (a)   => saturated_cast  (f(a))      (e.aev.asInstanceOf[Integral[A]], mtype(e.mev), e.faev, e.fmev, pos)
    case e@SaturatedPlus  (l,r) => saturated_plus  (f(l), f(r))(e.aev.asInstanceOf[Integral[A]], mtype(e.mev), pos)
    case e@SaturatedMinus (l,r) => saturated_minus (f(l), f(r))(e.aev.asInstanceOf[Integral[A]], mtype(e.mev), pos)
    case e@SaturatedTimes (l,r) => saturated_times (f(l), f(r))(e.aev.asInstanceOf[Integral[A]], mtype(e.mev), pos)
    case e@SaturatedDivide(l,r) => saturated_divide(f(l), f(r))(e.aev.asInstanceOf[Integral[A]], mtype(e.mev), pos)
    case e@SaturatedPower (l,r) => saturated_power (f(l), f(r))(e.aev.asInstanceOf[Integral[A]], mtype(e.mev), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait SaturatedNumericsOpsExpOpt extends SaturatedNumericsOpsExp { this: BooleanOpsExpOpt =>

  override def saturated_plus  [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs, rhs) match {
    case (Const(x), y) if x == implicitly[Integral[T]].zero => y
    case (x, Const(y)) if y == implicitly[Integral[T]].zero => x
    case _ if manifest[T] <:< manifest[Boolean] =>
      boolean_or(lhs.asInstanceOf[Exp[Boolean]], rhs.asInstanceOf[Exp[Boolean]]).asInstanceOf[Exp[T]]
    case _ => super.saturated_plus(lhs,rhs)
  }

  override def saturated_minus [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs, rhs) match {
    case (x, Const(y)) if y == implicitly[Integral[T]].zero => x
    case _ => super.saturated_minus(lhs, rhs)
  }

  override def saturated_times [T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs, rhs) match {
    case (Const(x), y) if x == implicitly[Integral[T]].zero => Const(x)
    case (x, Const(y)) if y == implicitly[Integral[T]].zero => Const(y)
    case (Const(x), y) if x == implicitly[Integral[T]].one => y
    case (x, Const(y)) if y == implicitly[Integral[T]].one => x
    case _ => super.saturated_times(lhs, rhs)
  }

  override def saturated_divide[T:Integral:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs, rhs) match {
    case (Const(y), _) if y == implicitly[Integral[T]].zero => lhs
    case (x, Const(y)) if y == implicitly[Integral[T]].one => x
    case _ => super.saturated_divide(lhs, rhs)
  }

}

trait CGenSaturatedNumericsOps extends CUnparser {

  val IR: SaturatedNumericsOpsExp
  import IR._

  def getTypeMinLimit[T](eav: Integral[T], tp: Typ[T]) = tp match {
    case _ if tp <:< ManifestTyp(manifest[Byte])  => "INT8_MIN"
    case _ if tp <:< ManifestTyp(manifest[Short]) => "INT16_MIN"
    case _ if tp <:< ManifestTyp(manifest[Int])   => "INT32_MIN"
    case _ if tp <:< ManifestTyp(manifest[Long])  => "INT64_MIN"
    case _ => "0"
  }

  def getTypeMaxLimit[T](eav: Integral[T], tp: Typ[T]) = tp match {
    case _ if tp <:< ManifestTyp(manifest[Byte])   => "INT8_MAX"
    case _ if tp <:< ManifestTyp(manifest[Short])  => "INT16_MAX"
    case _ if tp <:< ManifestTyp(manifest[Int])    => "INT32_MAX"
    case _ if tp <:< ManifestTyp(manifest[Long])   => "INT64_MAX"
    case _ if tp <:< ManifestTyp(manifest[UByte])  => "UINT8_MAX"
    case _ if tp <:< ManifestTyp(manifest[UShort]) => "UINT16_MAX"
    case _ if tp <:< ManifestTyp(manifest[UInt])   => "UINT32_MAX"
    case _ if tp <:< ManifestTyp(manifest[ULong])  => "UINT64_MAX"
    case _ => "0"
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    val tpe = remap(sym.tp)
    rhs match {
      case d: SaturatedDef[_] => {
        cApp.addSystemHeader("math.h")
        cApp.addSystemLibrary("m")
      }
      case _ =>
    }
    rhs match {
      case e@Saturate(a)   => {
        val lo = getTypeMinLimit(e.aev, e.mev)
        val hi = getTypeMaxLimit(e.aev, e.mev)
        emitValDef(sym, src"($a > $hi) ? $hi : ($a < $lo) ? $lo : ($tpe) $a")
      }
      case SaturatedPlus  (a,b) => {
        val funName = src"sadd_$tpe"
        val cFun = SaturationNumericImplementation.functions(funName)
        cApp.addStructure(cFun)
        emitValDef(sym, src"$funName($a, $b)")
      }
      case SaturatedMinus (a,b) => {
        val funName = src"ssub_$tpe"
        val cFun = SaturationNumericImplementation.functions(funName)
        cApp.addStructure(cFun)
        emitValDef(sym, src"$funName($a, $b)")
      }
      case SaturatedTimes (a,b) => {
        val funName = src"smul_$tpe"
        val cFun = SaturationNumericImplementation.functions(funName)
        funName match {
          case "smul_int64_t" =>
            val dep = SaturationNumericImplementation.functions("mul_wide_s64")
            cApp.addStructure(dep)
          case "smul_uint64_t" =>
            val dep = SaturationNumericImplementation.functions("mul_wide_u64")
            cApp.addStructure(dep)
          case _ =>
        }
        cApp.addStructure(cFun)
        emitValDef(sym, src"$funName($a, $b)")
      }
      case SaturatedDivide(a,b) => {
        val funName = src"sdiv_$tpe"
        val cFun = SaturationNumericImplementation.functions(funName)
        cApp.addStructure(cFun)
        emitValDef(sym, src"$funName($a, $b)")
      }
      case _ => super.emitNode(sym, rhs)
    }
  }
}

object SaturationNumericImplementation {

  val functions = Map (

    /* ================================================================= */
    /* ======================= Signed Addition ========================= */
    /* ================================================================= */

    "sadd_int8_t" -> CFunction("static inline int8_t sadd_int8_t(int8_t a, int8_t b)",
      """{
        |    int16_t c = (int16_t) a + (int16_t) b;
        |    if (c > INT8_MAX) {
        |        return INT8_MAX;
        |    } else if (c < INT8_MIN) {
        |        return INT8_MIN;
        |    } else {
        |        return (int8_t)c;
        |    }
        |}
      """.stripMargin),
    "sadd_int16_t" -> CFunction("static inline int16_t sadd_int16_t(int16_t a, int16_t b)",
      """{
        |    int32_t c = (int32_t) a + (int32_t) b;
        |    if (c > INT16_MAX) {
        |        return INT16_MAX;
        |    } else if (c < INT16_MIN) {
        |        return INT16_MIN;
        |    } else {
        |        return (int16_t)c;
        |    }
        |}
      """.stripMargin),

    "sadd_int32_t" -> CFunction("static inline int32_t sadd_int32_t(int32_t a, int32_t b)",
      """{
        |    int64_t c = (int64_t) a + (int64_t) b;
        |    if (c > INT32_MAX) {
        |        return INT32_MAX;
        |    } else if (c < INT32_MIN) {
        |        return INT32_MIN;
        |    } else {
        |        return (int32_t)c;
        |    }
        |}
      """.stripMargin),

    "sadd_int64_t" -> CFunction("static inline int64_t sadd_int64_t(int64_t a, int64_t b)",
      """{
        |    int64_t c = a + b;
        |    if ((a < 0L) && (b < 0L) && (c >= 0L)) {
        |        return INT64_MIN;
        |    } else if ((a > 0L) && (b > 0L) && (c <= 0L)) {
        |        return INT64_MAX;
        |    } else {
        |        return c;
        |    }
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ====================== Unsigned Addition ======================== */
    /* ================================================================= */

    "sadd_uint8_t" -> CFunction("static inline uint8_t sadd_uint8_t(uint8_t a, uint8_t b)",
      """{
        |    if (a > UINT8_MAX - b) {
        |        return UINT8_MAX;
        |    } else {
        |        return a + b;
        |    }
        |}
      """.stripMargin),

    "sadd_uint16_t" -> CFunction("static inline uint16_t sadd_uint16_t(uint16_t a, uint16_t b)",
      """{
        |    if (a > UINT16_MAX - b) {
        |        return UINT16_MAX;
        |    } else {
        |        return a + b;
        |    }
        |}
      """.stripMargin),

    "sadd_uint32_t" -> CFunction("static inline uint32_t sadd_uint32_t(uint32_t a, uint32_t b)",
      """{
        |    if (a > UINT32_MAX - b) {
        |        return UINT32_MAX;
        |    } else {
        |        return a + b;
        |    }
        |}
      """.stripMargin),

    "sadd_uint64_t" -> CFunction("static inline uint64_t sadd_uint64_t(uint64_t a, uint64_t b)",
      """{
        |    if (a > UINT64_MAX - b) {
        |        return UINT64_MAX;
        |    } else {
        |        return a + b;
        |    }
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ====================== Signed Subtraction ======================= */
    /* ================================================================= */

    "ssub_int8_t" -> CFunction("static inline int8_t ssub_int8_t(int8_t a, int8_t b)",
      """{
        |    int16_t c = (int16_t) a - (int16_t) b;
        |    if (c > INT8_MAX) {
        |        return INT8_MAX;
        |    } else if (c < INT8_MIN) {
        |        return INT8_MIN;
        |    } else {
        |        return (int8_t)c;
        |    }
        |}
      """.stripMargin),

    "ssub_int16_t" -> CFunction("static inline int16_t ssub_int16_t(int16_t a, int16_t b)",
      """{
        |    int32_t c = (int32_t) a - (int32_t) b;
        |    if (c > INT16_MAX) {
        |        return INT16_MAX;
        |    } else if (c < INT16_MIN) {
        |        return INT16_MIN;
        |    } else {
        |        return (int16_t)c;
        |    }
        |}
      """.stripMargin),

    "ssub_int32_t" -> CFunction("static inline int32_t ssub_int32_t(int32_t a, int32_t b)",
      """{
        |    int64_t c = (int64_t) a - (int64_t) b;
        |    if (c > INT32_MAX) {
        |        return INT32_MAX;
        |    } else if (c < INT32_MIN) {
        |        return INT32_MIN;
        |    } else {
        |        return (int32_t)c;
        |    }
        |}
      """.stripMargin),

    "ssub_int64_t" -> CFunction("static inline int64_t ssub_int64_t(int64_t a, int64_t b)",
      """{
        |    int64_t c = a - b;
        |    if ((a < 0L) && (b >= 0L) && (c >= 0L)) {
        |        return INT64_MIN;
        |    } else if ((a >= 0L) && (b < 0L) && (c < 0L)) {
        |        return INT64_MAX;
        |    } else {
        |        return c;
        |    }
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ===================== Unsigned Subtraction ====================== */
    /* ================================================================= */

    "ssub_uint8_t" -> CFunction("static inline uint8_t ssub_uint8_t(uint8_t a, uint8_t b)",
      """{
        |    if (a < b) {
        |        return 0;
        |    } else {
        |        return a - b;
        |    }
        |}
      """.stripMargin),

    "ssub_uint16_t" -> CFunction("static inline uint16_t ssub_uint16_t(uint16_t a, uint16_t b)",
      """{
        |    if (a < b) {
        |        return 0;
        |    } else {
        |        return a - b;
        |    }
        |}
      """.stripMargin),

    "ssub_uint32_t" -> CFunction("static inline uint32_t ssub_uint32_t(uint32_t a, uint32_t b)",
      """{
        |    if (a < b) {
        |        return 0;
        |    } else {
        |        return a - b;
        |    }
        |}
      """.stripMargin),

    "ssub_uint64_t" -> CFunction("static inline uint64_t ssub_uint64_t(uint64_t a, uint64_t b)",
      """{
        |    if (a < b) {
        |        return 0;
        |    } else {
        |        return a - b;
        |    }
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ==================== Signed Multiplication ====================== */
    /* ================================================================= */

    "mul_wide_s64" -> CFunction("static void mul_wide_s64(int64_t in0, int64_t in1, uint64_t *ptrOutBitsHi, uint64_t *ptrOutBitsLo)",
      """{
        |    uint64_t absIn0;
        |    uint64_t absIn1;
        |    int32_t negativeProduct;
        |    uint64_t in0Hi;
        |    uint64_t in0Lo;
        |    uint64_t productHiHi;
        |    uint64_t productHiLo;
        |    uint64_t productLoHi;
        |    if (in0 < 0L) {
        |        absIn0 = (uint64_t)-in0;
        |    } else {
        |        absIn0 = (uint64_t)in0;
        |    }
        |
        |    if (in1 < 0L) {
        |        absIn1 = (uint64_t)-in1;
        |    } else {
        |        absIn1 = (uint64_t)in1;
        |    }
        |
        |    negativeProduct = !((in0 == 0L) || ((in1 == 0L) || ((in0 > 0L) == (in1 > 0L))));
        |    in0Hi = absIn0 >> 32UL;
        |    in0Lo = absIn0 & 4294967295UL;
        |    absIn0 = absIn1 >> 32UL;
        |    absIn1 &= 4294967295UL;
        |    productHiHi = in0Hi * absIn0;
        |    productHiLo = in0Hi * absIn1;
        |    productLoHi = in0Lo * absIn0;
        |    absIn0 = in0Lo * absIn1;
        |    absIn1 = 0UL;
        |    in0Hi = absIn0 + (productLoHi << 32UL);
        |    if (in0Hi < absIn0) {
        |        absIn1 = 1UL;
        |    }
        |
        |    absIn0 = in0Hi;
        |    in0Hi += productHiLo << 32UL;
        |    if (in0Hi < absIn0) {
        |        absIn1++;
        |    }
        |
        |    absIn0 = ((absIn1 + productHiHi) + (productLoHi >> 32UL)) + (productHiLo >>
        |                                                                 32UL);
        |    if (negativeProduct) {
        |        absIn0 = ~absIn0;
        |        in0Hi = ~in0Hi;
        |        in0Hi++;
        |        if (in0Hi == 0UL) {
        |            absIn0++;
        |        }
        |    }
        |
        |    *ptrOutBitsHi = absIn0;
        |    *ptrOutBitsLo = in0Hi;
        |}
      """.stripMargin),

    "smul_int8_t" -> CFunction("static inline int8_t smul_int8_t(int8_t a, int8_t b)",
      """{
        |    int16_t i0;
        |    i0 = (int16_t) a * b;
        |    if (i0 > INT8_MAX) {
        |        i0 = INT8_MAX;
        |    } else {
        |        if (i0 < INT8_MIN) {
        |            i0 = INT8_MIN;
        |        }
        |    }
        |    return (int8_t)i0;
        |}
      """.stripMargin),

    "smul_int16_t" -> CFunction("static inline int16_t smul_int16_t(int16_t a, int16_t b)",
      """{
        |    int32_t i0;
        |    i0 = (int32_t) a * b;
        |    if (i0 > INT16_MAX) {
        |        i0 = INT16_MAX;
        |    } else {
        |        if (i0 < INT16_MIN) {
        |            i0 = INT16_MIN;
        |        }
        |    }
        |    return (int16_t)i0;
        |}
      """.stripMargin),

    "smul_int32_t" -> CFunction("static inline int32_t smul_int32_t(int32_t a, int32_t b)",
      """{
        |    int64_t i0;
        |    i0 = (int64_t) a * b;
        |    if (i0 > INT32_MAX) {
        |        i0 = INT32_MAX;
        |    } else {
        |        if (i0 < INT32_MIN) {
        |            i0 = INT32_MIN;
        |        }
        |    }
        |    return (int32_t)i0;
        |}
      """.stripMargin),

    "smul_int64_t" -> CFunction("static inline int64_t smul_int64_t(int64_t a, int64_t b)",
      """{
        |    int64_t result;
        |    uint64_t u64_clo;
        |    uint64_t u64_chi;
        |    mul_wide_s64(a, b, &u64_chi, &u64_clo);
        |    if (((int64_t)u64_chi > 0L) || ((u64_chi == 0UL) && (u64_clo >= 9223372036854775808UL))) {
        |        result = INT64_MAX;
        |    } else if (((int64_t)u64_chi < -1L) || (((int64_t)u64_chi == -1L) && (u64_clo < 9223372036854775808UL))) {
        |        result = INT64_MIN;
        |    } else {
        |        result = (int64_t)u64_clo;
        |    }
        |    return result;
        |}
      """.stripMargin),

    /* ================================================================= */
    /* =================== Unsigned Multiplication ===================== */
    /* ================================================================= */

    "mul_wide_u64" -> CFunction("static void mul_wide_u64(uint64_t in0, uint64_t in1, uint64_t *ptrOutBitsHi, uint64_t *ptrOutBitsLo)",
      """{
        |    uint64_t in0Hi;
        |    uint64_t in0Lo;
        |    uint64_t in1Hi;
        |    uint64_t in1Lo;
        |    uint64_t productHiHi;
        |    uint64_t productHiLo;
        |    uint64_t productLoHi;
        |    in0Hi = in0 >> 32UL;
        |    in0Lo = in0 & 4294967295UL;
        |    in1Hi = in1 >> 32UL;
        |    in1Lo = in1 & 4294967295UL;
        |    productHiHi = in0Hi * in1Hi;
        |    productHiLo = in0Hi * in1Lo;
        |    productLoHi = in0Lo * in1Hi;
        |    in0Hi = in0Lo * in1Lo;
        |    in1Hi = 0UL;
        |    in0Lo = in0Hi + (productLoHi << 32UL);
        |    if (in0Lo < in0Hi) {
        |        in1Hi = 1UL;
        |    }
        |
        |    in0Hi = in0Lo;
        |    in0Lo += productHiLo << 32UL;
        |    if (in0Lo < in0Hi) {
        |        in1Hi++;
        |    }
        |
        |    in0Hi = ((in1Hi + productHiHi) + (productLoHi >> 32UL)) + (productHiLo >> 32UL);
        |    *ptrOutBitsHi = in0Hi;
        |    *ptrOutBitsLo = in0Lo;
        |}
      """.stripMargin),

    "smul_uint8_t" -> CFunction("static inline uint8_t smul_uint8_t(uint8_t A, uint8_t B)",
      """{
        |    uint16_t u0;
        |    u0 = (uint16_t)A * B;
        |    if (u0 > UINT8_MAX) {
        |        u0 = UINT8_MAX;
        |    }
        |    return (uint8_t)u0;
        |}
      """.stripMargin),

    "smul_uint16_t" -> CFunction("static inline uint16_t smul_uint16_t(uint16_t A, uint16_t B)",
      """{
        |    uint32_t u0;
        |    u0 = (uint32_t)A * B;
        |    if (u0 > UINT16_MAX) {
        |        u0 = UINT16_MAX;
        |    }
        |    return (uint16_t)u0;
        |}
      """.stripMargin),

    "smul_uint32_t" -> CFunction("static inline uint32_t smul_uint32_t(uint32_t A, uint32_t B)",
      """{
        |    uint64_t u0;
        |    u0 = (uint64_t)A * B;
        |    if (u0 > UINT32_MAX) {
        |        u0 = UINT32_MAX;
        |    }
        |    return (uint32_t)u0;
        |}
      """.stripMargin),

    "smul_uint64_t" -> CFunction("static inline uint64_t smul_uint64_t(uint64_t a, uint64_t b)",
      """{
        |    uint64_t result;
        |    uint64_t u64_chi;
        |    mul_wide_u64(a, b, &u64_chi, &result);
        |    if (u64_chi) {
        |        result = UINT64_MAX;
        |    }
        |    return result;
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ====================== Unsigned Division ======================== */
    /* ================================================================= */


    "sdiv_uint8_t" -> CFunction("static inline uint8_t sdiv_uint8_t(uint8_t a, uint8_t b)",
      """{
        |    uint8_t x;
        |    int32_t i0;
        |    uint8_t b_x;
        |    if (b == 0) {
        |        if (a == 0) {
        |            x = 0;
        |        } else {
        |            x = UINT8_MAX;
        |        }
        |    } else {
        |        i0 = b;
        |        x = (uint8_t)((uint32_t)a / i0);
        |        b_x = (uint8_t)((uint32_t)a - (uint8_t)((uint32_t)x * b));
        |        if ((b_x > 0) && (b_x >= (int32_t)((uint32_t)b >> 1) + (b & 1))) {
        |            x++;
        |        }
        |    }
        |    return x;
        |}
      """.stripMargin),

    "sdiv_uint16_t" -> CFunction("static inline uint16_t sdiv_uint16_t(uint16_t A, uint16_t B)",
      """{
        |    uint16_t x;
        |    int32_t i0;
        |    uint16_t b_x;
        |    if (B == 0) {
        |        if (A == 0) {
        |            x = 0;
        |        } else {
        |            x = UINT16_MAX;
        |        }
        |    } else {
        |        i0 = B;
        |        x = (uint16_t)((uint32_t)A / i0);
        |        b_x = (uint16_t)((uint32_t)A - (uint16_t)((uint32_t)x *
        |                                                  B));
        |        if ((b_x > 0) && (b_x >= (int32_t)((uint32_t)B >> 1) + (B & 1))) {
        |            x++;
        |        }
        |    }
        |
        |    return x;
        |}
      """.stripMargin),

    "sdiv_uint32_t" -> CFunction("static inline uint32_t sdiv_uint32_t(uint32_t a, uint32_t b)",
      """{
        |    uint32_t x;
        |    uint32_t b_x;
        |    if (b == 0U) {
        |        if (a == 0U) {
        |            x = 0U;
        |        } else {
        |            x = UINT32_MAX;
        |        }
        |    } else {
        |        x = a / b;
        |        b_x = a - x * b;
        |        if ((b_x > 0U) && (b_x >= (b >> 1U) + (b & 1U))) {
        |            x++;
        |        }
        |    }
        |    return x;
        |}
      """.stripMargin),

    "sdiv_uint64_t" -> CFunction("static inline uint64_t sdiv_uint64_t(uint64_t a, uint64_t b)",
      """{
        |    uint64_t x;
        |    uint64_t b_x;
        |    if (b == 0UL) {
        |        if (a == 0UL) {
        |            x = 0UL;
        |        } else {
        |            x = UINT64_MAX;
        |        }
        |    } else {
        |        if (b == 0UL) {
        |            x = UINT64_MAX;
        |        } else {
        |            x = a / b;
        |        }
        |
        |        b_x = a - x * b;
        |        if ((b_x > 0UL) && (b_x >= (b >> 1UL) + (b & 1UL))) {
        |            x++;
        |        }
        |    }
        |    return x;
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ======================= Signed Division ========================= */
    /* ================================================================= */

    "sdiv_int8_t" -> CFunction("static inline int8_t sdiv_int8_t(int8_t a, int8_t b)",
      """{
        |    int8_t x;
        |    int i0;
        |    uint8_t y;
        |    uint8_t b_y;
        |    uint8_t q;
        |    if (b == 0) {
        |        if (a == 0) {
        |            x = 0;
        |        } else if (a < 0) {
        |            x = INT8_MIN;
        |        } else {
        |            x = INT8_MAX;
        |        }
        |    } else if (b == 1) {
        |        x = a;
        |    } else if (b == -1) {
        |        i0 = -a;
        |        if (i0 > 127) {
        |            i0 = 127;
        |        }
        |
        |        x = (int8_t)i0;
        |    } else {
        |        if (a >= 0) {
        |            y = (uint8_t)a;
        |        } else if (a == -128) {
        |            y = 128;
        |        } else {
        |            y = (uint8_t)-a;
        |        }
        |
        |        if (b >= 0) {
        |            b_y = (uint8_t)b;
        |        } else if (b == -128) {
        |            b_y = 128;
        |        } else {
        |            b_y = (uint8_t)-b;
        |        }
        |
        |        i0 = b_y;
        |        if ((uint32_t)i0 == 0U) {
        |            q = UINT8_MAX;
        |        } else {
        |            q = (uint8_t)((uint32_t)y / i0);
        |        }
        |
        |        y = (uint8_t)((uint32_t)y - (uint8_t)((uint32_t)q * b_y));
        |        if ((y > 0) && (y >= (int)((uint32_t)b_y >> 1) + (b_y & 1))) {
        |            q++;
        |        }
        |
        |        x = (int8_t)q;
        |        if ((a < 0) != (b < 0)) {
        |            x = (int8_t)-(int8_t)q;
        |        }
        |    }
        |    return x;
        |}
      """.stripMargin),

    "sdiv_int16_t" -> CFunction("static inline int16_t sdiv_int16_t(int16_t A, int16_t B) ",
      """{
        |    int16_t x;
        |    int32_t i0;
        |    uint16_t y, b_y, q;
        |    if (B == 0) {
        |        if (A == 0) {
        |            x = 0;
        |        } else if (A < 0) {
        |            x = INT16_MIN;
        |        } else {
        |            x = INT16_MAX;
        |        }
        |    } else if (B == 1) {
        |        x = A;
        |    } else if (B == -1) {
        |        i0 = -A;
        |        if (i0 > 32767) {
        |            i0 = 32767;
        |        }
        |
        |        x = (int16_t)i0;
        |    } else {
        |        if (A >= 0) {
        |            y = (uint16_t)A;
        |        } else if (A == -32768) {
        |            y = 32768;
        |        } else {
        |            y = (uint16_t)-A;
        |        }
        |
        |        if (B >= 0) {
        |            b_y = (uint16_t)B;
        |        } else if (B == -32768) {
        |            b_y = 32768;
        |        } else {
        |            b_y = (uint16_t)-B;
        |        }
        |
        |        i0 = b_y;
        |        if ((uint32_t)i0 == 0U) {
        |            q = UINT16_MAX;
        |        } else {
        |            q = (uint16_t)((uint32_t)y / i0);
        |        }
        |
        |        y = (uint16_t)((uint32_t)y - (uint16_t)((uint32_t)q * b_y));
        |        if ((y > 0) && (y >= (int)((uint32_t)b_y >> 1) + (b_y & 1))) {
        |            q++;
        |        }
        |
        |        x = (int16_t)q;
        |        if ((A < 0) != (B < 0)) {
        |            x = (int16_t) - (int16_t)q;
        |        }
        |    }
        |
        |    return x;
        |}
      """.stripMargin),

    "sdiv_int32_t" -> CFunction("static inline int32_t sdiv_int32_t(int32_t a, int32_t b)",
      """{
        |    uint32_t UINT31_MAX = 1 + (uint32_t)INT32_MAX;
        |    int32_t x;
        |    uint32_t y, b_y, q;
        |    if (b == 0) {
        |        if (a == 0) {
        |            x = 0;
        |        } else if (a < 0) {
        |            x = INT32_MIN;
        |        } else {
        |            x = INT32_MAX;
        |        }
        |    } else if (b == 1) {
        |        x = a;
        |    } else if (b == -1) {
        |        if (a <= INT32_MIN) {
        |            x = INT32_MAX;
        |        } else {
        |            x = -a;
        |        }
        |    } else {
        |        if (a >= 0) {
        |            y = (uint32_t)a;
        |        } else if (a == INT32_MIN) {
        |            y = UINT31_MAX;
        |        } else {
        |            y = (uint32_t)-a;
        |        }
        |
        |        if (b >= 0) {
        |            b_y = (uint32_t)b;
        |        } else if (b == INT32_MIN) {
        |            b_y = UINT31_MAX;
        |        } else {
        |            b_y = (uint32_t)-b;
        |        }
        |
        |        if (b_y == 0U) {
        |            q = UINT32_MAX;
        |        } else {
        |            q = y / b_y;
        |        }
        |
        |        y -= q * b_y;
        |        if ((y > 0U) && (y >= (b_y >> 1U) + (b_y & 1U))) {
        |            q++;
        |        }
        |
        |        x = (int32_t) q;
        |        if ((a < 0) != (b < 0)) {
        |            x = -(int32_t)q;
        |        }
        |    }
        |    return x;
        |}
      """.stripMargin),

    "sdiv_int64_t" -> CFunction("static inline int64_t sdiv_int64_t(int64_t a, int64_t b)",
      """{
        |    uint64_t UINT63_MAX = 1 + (uint64_t)INT64_MAX;
        |    int64_t x;
        |    uint64_t y, b_y, q;
        |    if (b == 0L) {
        |        if (a == 0L) {
        |            x = 0L;
        |        } else if (a < 0L) {
        |            x = INT64_MIN;
        |        } else {
        |            x = INT64_MAX;
        |        }
        |    } else if (b == 1L) {
        |        x = a;
        |    } else if (b == -1L) {
        |        if (a <= INT64_MIN) {
        |            x = INT64_MAX;
        |        } else {
        |            x = -a;
        |        }
        |    } else {
        |        if (a >= 0L) {
        |            y = (uint64_t) a;
        |        } else if (a == INT64_MIN) {
        |            y = UINT63_MAX;
        |        } else {
        |            y = (uint64_t) - a;
        |        }
        |
        |        if (b >= 0L) {
        |            b_y = (uint64_t) b;
        |        } else if (b == INT64_MIN) {
        |            b_y = UINT63_MAX;
        |        } else {
        |            b_y = (uint64_t) - b;
        |        }
        |
        |        if (b_y == 0UL) {
        |            q = UINT64_MAX;
        |        } else {
        |            q = y / b_y;
        |        }
        |
        |        y -= q * b_y;
        |        if ((y > 0UL) && (y >= (b_y >> 1UL) + (b_y & 1UL))) {
        |            q++;
        |        }
        |
        |        x = (int64_t)q;
        |        if ((a < 0L) != (b < 0L)) {
        |            x = -x;
        |        }
        |    }
        |    return x;
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ======================== Unsigned Power ========================= */
    /* ================================================================= */


    "spow_uint8_t" -> CFunction("static inline uint8_t spow_uint8_t(uint8_t A, uint8_t B)",
      """{
        |    uint8_t x;
        |    uint8_t ak;
        |    uint8_t bku;
        |    int32_t exitg1;
        |    int32_t i0;
        |    ak = A;
        |    x = 1;
        |    bku = B;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1) != 0) {
        |            i0 = (int32_t)((uint32_t)ak * x);
        |            if ((unsigned int)i0 > 255U) {
        |                i0 = 255;
        |            }
        |
        |            x = (uint8_t)i0;
        |        }
        |
        |        bku = (uint8_t)((int32_t)bku >> 1);
        |        if (bku == 0) {
        |            exitg1 = 1;
        |        } else {
        |            i0 = (int32_t)((uint32_t)ak * ak);
        |            if ((uint32_t)i0 > 255U) {
        |                i0 = 255;
        |            }
        |
        |            ak = (uint8_t)i0;
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    "spow_uint16_t" -> CFunction("static inline uint16_t spow_uint16_t(uint16_t A, uint16_t B)",
      """{
        |    uint16_t x;
        |    uint16_t ak;
        |    uint16_t bku;
        |    int32_t exitg1;
        |    uint32_t u0;
        |    ak = A;
        |    x = 1;
        |    bku = B;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1) != 0) {
        |            u0 = (uint32_t)ak * x;
        |            if (u0 > 65535U) {
        |                u0 = 65535U;
        |            }
        |
        |            x = (uint16_t)u0;
        |        }
        |
        |        bku = (uint16_t)((uint32_t)bku >> 1);
        |        if (bku == 0) {
        |            exitg1 = 1;
        |        } else {
        |            u0 = (uint32_t)ak * ak;
        |            if (u0 > 65535U) {
        |                u0 = 65535U;
        |            }
        |
        |            ak = (uint16_t)u0;
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    "spow_uint32_t" -> CFunction("static inline uint32_t spow_uint32_t(uint32_t a, uint32_t b)",
      """{
        |    uint32_t x;
        |    uint32_t ak;
        |    uint32_t bku;
        |    int32_t exitg1;
        |    uint64_t u0;
        |    ak = a;
        |    x = 1U;
        |    bku = b;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1U) != 0U) {
        |            u0 = (uint64_t)ak * x;
        |            if (u0 > 4294967295UL) {
        |                u0 = 4294967295UL;
        |            }
        |
        |            x = (uint32_t)u0;
        |        }
        |
        |        bku >>= 1U;
        |        if ((int)bku == 0) {
        |            exitg1 = 1;
        |        } else {
        |            u0 = (uint64_t)ak * ak;
        |            if (u0 > 4294967295UL) {
        |                u0 = 4294967295UL;
        |            }
        |
        |            ak = (uint32_t)u0;
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    "spow_uint64_t" -> CFunction("static inline uint64_t spow_uint64_t(uint64_t a, uint64_t b)",
      """{
        |    uint64_t x;
        |    uint64_t ak;
        |    uint64_t bku;
        |    int32_t exitg1;
        |    ak = a;
        |    x = 1UL;
        |    bku = b;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1UL) != 0UL) {
        |            x = smul_uint64_t(ak, x);
        |        }
        |
        |        bku >>= 1UL;
        |        if (bku == 0UL) {
        |            exitg1 = 1;
        |        } else {
        |            ak = smul_uint64_t(ak, ak);
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    /* ================================================================= */
    /* ========================= Signed Power ========================== */
    /* ================================================================= */


    "spow_int8_t" -> CFunction("static inline int8_t spow_int8_t(int8_t a, int8_t b)",
      """{
        |    int8_t x;
        |    int8_t ak;
        |    int8_t i0;
        |    uint8_t bku;
        |    int32_t exitg1;
        |    int32_t i1;
        |    ak = a;
        |    x = 1;
        |    i0 = b;
        |    if (i0 < 0) {
        |        i0 = 0;
        |    }
        |
        |    bku = (uint8_t)i0;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1) != 0) {
        |            i1 = ak * x;
        |            if (i1 > 127) {
        |                i1 = 127;
        |            } else {
        |                if (i1 < -128) {
        |                    i1 = -128;
        |                }
        |            }
        |
        |            x = (int8_t)i1;
        |        }
        |
        |        bku = (uint8_t)((uint32_t)bku >> 1);
        |        if (bku == 0) {
        |            exitg1 = 1;
        |        } else {
        |            i1 = ak * ak;
        |            if (i1 > 127) {
        |                i1 = 127;
        |            } else {
        |                if (i1 < -128) {
        |                    i1 = -128;
        |                }
        |            }
        |
        |            ak = (int8_t)i1;
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    "spow_int16_t" -> CFunction("static inline int16_t spow_int16_t(int16_t A, int16_t B)",
      """{
        |    int16_t x;
        |    int16_t ak;
        |    int16_t i0;
        |    uint16_t bku;
        |    int32_t exitg1;
        |    int32_t i1;
        |    ak = A;
        |    x = 1;
        |    i0 = B;
        |    if (i0 < 0) {
        |        i0 = 0;
        |    }
        |
        |    bku = (uint16_t)i0;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1) != 0) {
        |            i1 = ak * x;
        |            if (i1 > 32767) {
        |                i1 = 32767;
        |            } else {
        |                if (i1 < -32768) {
        |                    i1 = -32768;
        |                }
        |            }
        |
        |            x = (int16_t)i1;
        |        }
        |
        |        bku = (uint16_t)((unsigned int)bku >> 1);
        |        if (bku == 0) {
        |            exitg1 = 1;
        |        } else {
        |            i1 = ak * ak;
        |            if (i1 > 32767) {
        |                i1 = 32767;
        |            } else {
        |                if (i1 < -32768) {
        |                    i1 = -32768;
        |                }
        |            }
        |
        |            ak = (int16_t)i1;
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    "spow_int32_t" -> CFunction("static inline int32_t spow_int32_t(int32_t A, int32_t B)",
      """{
        |    int32_t x;
        |    int32_t ak;
        |    int32_t i0;
        |    uint32_t bku;
        |    int32_t exitg1;
        |    int64_t i1;
        |    ak = A;
        |    x = 1;
        |    i0 = B;
        |    if (i0 < 0) {
        |        i0 = 0;
        |    }
        |
        |    bku = (uint32_t)i0;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1U) != 0U) {
        |            i1 = (long)ak * x;
        |            if (i1 > INT32_MAX) {
        |                i1 = INT32_MAX;
        |            } else {
        |                if (i1 < INT32_MIN) {
        |                    i1 = INT32_MIN;
        |                }
        |            }
        |
        |            x = (int)i1;
        |        }
        |
        |        bku >>= 1U;
        |        if ((int)bku == 0) {
        |            exitg1 = 1;
        |        } else {
        |            i1 = (long)ak * ak;
        |            if (i1 > INT32_MAX) {
        |                i1 = INT32_MAX;
        |            } else {
        |                if (i1 < INT32_MIN) {
        |                    i1 = INT32_MIN;
        |                }
        |            }
        |
        |            ak = (int32_t)i1;
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin),

    "spow_int64_t" -> CFunction("static inline int64_t spow_int64_t(int64_t A, int64_t B)",
      """{
        |    int64_t x;
        |    int64_t ak;
        |    int64_t i0;
        |    uint64_t bku;
        |    int32_t exitg1;
        |    ak = A;
        |    x = 1L;
        |    i0 = B;
        |    if (i0 < 0L) {
        |        i0 = 0L;
        |    }
        |
        |    bku = (uint64_t)i0;
        |    do {
        |        exitg1 = 0;
        |        if ((bku & 1UL) != 0UL) {
        |            x = smul_int64_t(ak, x);
        |        }
        |
        |        bku >>= 1UL;
        |        if (bku == 0UL) {
        |            exitg1 = 1;
        |        } else {
        |            ak = smul_int64_t(ak, ak);
        |        }
        |    } while (exitg1 == 0);
        |
        |    return x;
        |}
      """.stripMargin)
  )

}
