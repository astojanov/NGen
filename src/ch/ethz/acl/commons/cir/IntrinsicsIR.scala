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

package ch.ethz.acl.commons.cir

import ch.ethz.acl.commons.cir.codegen.{CApplication, CCodegen}
import ch.ethz.acl.commons.cir.extensions.IntrinsicsHeapArrays
import ch.ethz.acl.intrinsics.MicroArchType.MicroArchType
import ch.ethz.acl.intrinsics._

import scala.reflect.SourceContext

/**
  * Intrinsics IR class, contains CIR internally and all SIMD intrinsics
  * starting from MMX all the way to AVX2 + FMA. Note that this class
  * does not include AVX512, KNC and other IRs.
  */
class IntrinsicsIR extends CIR
  with IntrinsicsArrays
  with IntrinsicsHeapArrays
  with MMX
  with SSE
  with SSE2
  with SSE3
  with SSSE3
  with SSE41
  with SSE42
  with AVX
  with AVX2
  with FMA
  with Other
{ self =>

  // =========================================================================================================================
  // Missing prefetch, needs to be fixed.
  // =========================================================================================================================

  val _MM_HINT_T0  = Const(3)
  val _MM_HINT_T1  = Const(2)
  val _MM_HINT_T2  = Const(1)
  val _MM_HINT_NTA = Const(0)

  /**
    * Fetch the line of data from memory that contains address "p" to a location in
    * the cache heirarchy specified by the locality hint "i".
    * p: char const*, i: int, pOffset: int
    */
  case class MM_PREFETCH[A[_], V:Typ, U:Integral](p: Exp[A[V]], i: Exp[Int], pOffset: Exp[U])(implicit val cont: Container[A]) extends VoidPointerIntrinsicsDef[V, U, Unit] {
    val category = List(IntrinsicsCategory.GeneralSupport)
    val intrinsicType = List()
    val performance = Map.empty[MicroArchType, Performance]
    val header = "xmmintrin.h"
  }

  def _mm_prefetch[A[_], V:Typ, U:Integral](p: Exp[A[V]], i: Exp[Int], pOffset: Exp[U])(implicit cont: Container[A]): Exp[Unit] = {
    cont.write(p)(MM_PREFETCH(p, i, pOffset)(implicitly[Typ[V]], implicitly[Integral[U]], cont))
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case iDef@MM_PREFETCH(p, i, pOffset) =>
      _mm_prefetch(iDef.cont.applyTransformer(p, f), iDef.cont.applyTransformer(i, f), iDef.cont.applyTransformer(pOffset, f))(iDef.voidType, iDef.integralType, iDef.cont)
    case Reflect(iDef@MM_PREFETCH (p, i, pOffset), u, es) =>
      reflectMirrored(Reflect(MM_PREFETCH (iDef.cont.applyTransformer(p, f), iDef.cont.applyTransformer(i, f), iDef.cont.applyTransformer(pOffset, f))(iDef.voidType, iDef.integralType, iDef.cont), mapOver(f,u), f(es)))(mtype(typ[A]), pos)
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

  // =========================================================================================================================
  // End of missing prefetch. This should go away soon.
  // =========================================================================================================================



  val codegen = new CCodegen
    with CGenMMX
    with CGenSSE
    with CGenSSE2
    with CGenSSE3
    with CGenSSSE3
    with CGenSSE41
    with CGenSSE42
    with CGenAVX
    with CGenAVX2
    with CGenFMA
    with CGenOther
  {
    val IR: self.type = self
    override def generateJNIApplication[B](
      syms: List[Sym[Any]], block: Block[B], fName: String
    ): CApplication = {
      val cApp = super.generateJNIApplication[B](syms, block, fName)
      getIntrinsicsHeaders.map(h => cApp.addSystemHeader(h))
      cApp
    }

    // =========================================================================================================================
    // Adding the missing prefetch, as well as fixes to some load and store nodes which are not offseted right
    // =========================================================================================================================

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

      case iDef@MM_PREFETCH(p, i, pOffset) =>
        headers += iDef.header
        stream.println(s"_mm_prefetch((char const*)(${quote(p) + (if(pOffset == Const(0)) "" else " + " + quote(pOffset))}), ${quote(i)});")

      case iDef@MM_LOADU_SI128(mem_addr, mem_addrOffset) =>
        headers += iDef.header
        emitValDef(sym, s"_mm_loadu_si128((__m128i const*)(${quote(mem_addr) + (if(mem_addrOffset == Const(0)) "" else " + " + quote(mem_addrOffset))}))")
      case iDef@MM_STOREU_SI128(mem_addr, a, mem_addrOffset) =>
        headers += iDef.header
        stream.println(s"_mm_storeu_si128((__m128i*)(${quote(mem_addr) + (if(mem_addrOffset == Const(0)) "" else " + " + quote(mem_addrOffset))}), ${quote(a)});")

      case iDef@MM256_LOADU_SI256(mem_addr, mem_addrOffset) =>
        headers += iDef.header
        emitValDef(sym, s"_mm256_loadu_si256((__m256i const *)(${quote(mem_addr) + (if(mem_addrOffset == Const(0)) "" else " + " + quote(mem_addrOffset))}))")
      case _ => super.emitNode(sym, rhs)
    }

    // =========================================================================================================================
    // End of fixes. This should be removed soon !!!
    // =========================================================================================================================


  }

}
