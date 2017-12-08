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
import ch.ethz.acl.intrinsics._

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
      getIntrinsicsHeaders.foreach(h => cApp.addSystemHeader(h))
      cApp
    }
  }

}
