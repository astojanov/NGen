package ch.ethz.acl.ngen.precison

import ch.ethz.acl.commons.cir.IntrinsicsIR
import ch.ethz.acl.passera.unsigned.UInt

object QVector {
  val IR = new IntrinsicsIR

  def apply(bits: Int, size: Int): QVector = bits match {
    case 16 => new QVector16 (size)
  }

}

abstract class QVector (val bits: Int, val size: Int) {

  import QVector.IR._

  //
  // Use RDRAND CPU feature to generate a hardware generated random number
  //
  def random_float_RDRAND () : Rep[Float] =
  {
    import QVector.IR.ImplicitLift._

    val i_rnd = NewArray[UInt](1)
    var ret = 0
    __whileDo(boolean_equals(readVar(ret), Const(0)), {
      ret = _rdrand32_step(i_rnd, 0)
    })
    val f_rnd = infix_cast[Float](i_rnd(0))
    f_rnd * (1.0f / 4294967296.0f)
  }

  //
  // For a given array, take the absolute value of each element
  // and return the maximum element.
  //
  def abs_max (v: Rep[Array[Float]], n0: Rep[Int]): Rep[Float] =
  {
    import QVector.IR.ImplicitLift._

    val first_bit_zero_32 = _mm256_castsi256_ps(_mm256_set1_epi32  (0x7FFFFFFF))
    val n1 = (n0 >> 5) << 5

    //
    // Initialize accumulators
    //
    var max1 = _mm256_setzero_ps()
    var max2 = _mm256_setzero_ps()
    var max3 = _mm256_setzero_ps()
    var max4 = _mm256_setzero_ps()
    //
    // Perform the main loop
    //
    forloop(0, n1, fresh[Int], 32, (i: Rep[Int]) =>
    {
      val v1 = _mm256_loadu_ps(v, i + 0)
      val v2 = _mm256_loadu_ps(v, i + 8)
      val v3 = _mm256_loadu_ps(v, i + 16)
      val v4 = _mm256_loadu_ps(v, i + 24)

      val abs1 = _mm256_and_ps(first_bit_zero_32, v1)
      val abs2 = _mm256_and_ps(first_bit_zero_32, v2)
      val abs3 = _mm256_and_ps(first_bit_zero_32, v3)
      val abs4 = _mm256_and_ps(first_bit_zero_32, v4)

      max1 = _mm256_max_ps(max1, abs1)
      max2 = _mm256_max_ps(max2, abs2)
      max3 = _mm256_max_ps(max3, abs3)
      max4 = _mm256_max_ps(max4, abs4)
    })
    //
    // Reduce the accumulators
    //
    val tmp1 = _mm256_max_ps(max1, max2)
    val tmp2 = _mm256_max_ps(max3, max4)
    val tmp3 = _mm256_max_ps(tmp1, tmp2)
    //
    // Horizontal max
    //
    val tmp4 = _mm256_castps256_ps128(tmp3)
    val tmp5 = _mm256_extractf128_ps(tmp3, 1)
    val tmp6 = _mm_max_ps(tmp4, tmp5)
    val tmp7 = _mm_shuffle_ps(tmp6, tmp6, UInt(78))
    val tmp8 = _mm_max_ps(tmp6, tmp7)
    val tmp9 = _mm_permute_ps(tmp8, 1)
    val tmp0 = _mm_max_ps(tmp8, tmp9)
    //
    // Return the result stored in the first element
    //
     var absMax = _mm_cvtss_f32(tmp0)

    forloop(n1, n0, fresh[Int], 1, (i: Rep[Int]) => {
      val va = Math.abs(v(i))
      if ( va > readVar(absMax) ) {
        absMax = va
      }
    })

    readVar(absMax)
  }

  //
  // Performs horizontal sum i.e. reduction
  //
  def reduce_sum(acc: Rep[__m256]): Rep[Float] = {
    import QVector.IR.ImplicitLift._
    val left  = _mm256_extractf128_ps(acc, 1)
    val right = _mm256_castps256_ps128(acc)
    val x128  = _mm_add_ps(left, right)
    val x64   = _mm_add_ps(x128, _mm_movehl_ps(x128, x128))
    val x32   = _mm_add_ss(x64, _mm_shuffle_ps(x64, x64, UInt(0x55)))
    _mm_cvtss_f32(x32)
  }


  val size_pad: Int

  def quantize (input  : Array[Float]): Unit
  def restore  (output : Array[Float]): Unit
}






