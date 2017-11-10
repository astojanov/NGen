package ch.ethz.acl.ngen.precison

import ch.ethz.acl.passera.unsigned.{UByte, UInt}
import com.github.dwickern.macros.NameOf.nameOf

class QVector4(s: Int) extends QVector(4, s) {

  import QVector.IR._

  val size_pad: Int = size % 128 match {
    case 0 => size
    case _ => size + 128 - (size % 128);
  }

  val values = new Array[Byte](size_pad / 2)
  var scale = 1f

  def dot_staged (
    u_short: Rep[Array[Byte]],
    v_short: Rep[Array[Byte]],
    su: Rep[Float],
    sv: Rep[Float],
    n0: Rep[Int]
  ): Rep[Float] = {

    import QVector.IR.ImplicitLift._

    val u  = u_short.asInstanceOf[Exp[Array[__m256i]]]
    val v  = v_short.asInstanceOf[Exp[Array[__m256i]]]

    val base_mask_hi = _mm256_set1_epi8(0x70.toByte)
    val base_mask_lo = _mm256_set1_epi8(0x07.toByte)
    val base_mask    = _mm256_set1_epi8(0x77.toByte)
    val one_16bit    = _mm256_set1_epi16(1.toShort)

    var acc                 = _mm256_setzero_ps();

    forloop(0, n0, fresh[Int], 128, (idx: Rep[Int]) =>
    {
      val i0 = idx >> 1
      val i1 = i0 + 32
      val i2 = i0 + 64
      //
      // Load 64 quantized values, twice, from both input vectors. The quantized values
      // are packed in pairs, each pair stored into an 8-bit chunk.
      //
      val uq_packed_8bit_1  = _mm256_loadu_si256(u, i0)
      val uq_packed_8bit_2  = _mm256_loadu_si256(u, i1)
      val vq_packed_8bit_1  = _mm256_loadu_si256(v, i0)
      val vq_packed_8bit_2  = _mm256_loadu_si256(v, i1)
      //
      // The sign bits have corresponding places, so we can use that. The signs are
      // either 1 or 0, 1 being negative, and 0 being positive. Since the dot represents
      // multiplication, the result of the signs is a simple XOR function. We do this
      // calculation now, so we can use it later, and benefit from ILP.
      //
      val q_sign_packed_xor_1 = _mm256_xor_si256(uq_packed_8bit_1, vq_packed_8bit_1)
      val q_sign_packed_xor_2 = _mm256_xor_si256(uq_packed_8bit_2, vq_packed_8bit_2)
      val q_sign_hi_1         = _mm256_or_si256(q_sign_packed_xor_1, base_mask)
      val q_sign_hi_2         = _mm256_or_si256(q_sign_packed_xor_2, base_mask)
      val q_sign_lo_1         = _mm256_slli_epi16(q_sign_hi_1, 4)
      val q_sign_lo_2         = _mm256_slli_epi16(q_sign_hi_2, 4)
      //
      // We can not process the packed chunks of 8 bytes, so we need to unpack them.
      // Extract the bases, of each element, and shift them accordingly.
      //
      val uq_base_8bit_hi_shift_1 = _mm256_and_si256(uq_packed_8bit_1, base_mask_hi)
      val uq_base_8bit_hi_shift_2 = _mm256_and_si256(uq_packed_8bit_2, base_mask_hi)
      val uq_base_8bit_lo_1       = _mm256_and_si256(uq_packed_8bit_1, base_mask_lo)
      val uq_base_8bit_lo_2       = _mm256_and_si256(uq_packed_8bit_2, base_mask_lo)
      val vq_base_8bit_hi_shift_1 = _mm256_and_si256(vq_packed_8bit_1, base_mask_hi)
      val vq_base_8bit_hi_shift_2 = _mm256_and_si256(vq_packed_8bit_2, base_mask_hi)
      val vq_base_8bit_lo_1       = _mm256_and_si256(vq_packed_8bit_1, base_mask_lo)
      val vq_base_8bit_lo_2       = _mm256_and_si256(vq_packed_8bit_2, base_mask_lo)
      val uq_base_8bit_hi_1       = _mm256_srli_epi16(uq_base_8bit_hi_shift_1, 4)
      val uq_base_8bit_hi_2       = _mm256_srli_epi16(uq_base_8bit_hi_shift_2, 4)
      val vq_base_8bit_hi_1       = _mm256_srli_epi16(vq_base_8bit_hi_shift_1, 4)
      val vq_base_8bit_hi_2       = _mm256_srli_epi16(vq_base_8bit_hi_shift_2, 4)
      //
      // Sign the bases using the XOR-ed version of the signs and prepare for use of
      // _mm256_maddubs_epi16 as the second operand
      //
      val vq_8bit_hi_1 = _mm256_sign_epi8(vq_base_8bit_hi_1, q_sign_hi_1)
      val vq_8bit_hi_2 = _mm256_sign_epi8(vq_base_8bit_hi_2, q_sign_hi_2)
      val vq_8bit_lo_1 = _mm256_sign_epi8(vq_base_8bit_lo_1, q_sign_lo_1)
      val vq_8bit_lo_2 = _mm256_sign_epi8(vq_base_8bit_lo_2, q_sign_lo_2)
      //
      // Finally, multiply the bases, with the signed bases, and store the result in
      // 16-bit chunks. _mm256_maddubs_epi16 horizontally adds adjecent pairs.
      //
      val q_16bit_hi_1 = _mm256_maddubs_epi16(uq_base_8bit_hi_1, vq_8bit_hi_1);
      val q_16bit_hi_2 = _mm256_maddubs_epi16(uq_base_8bit_hi_2, vq_8bit_hi_2);
      val q_16bit_lo_1 = _mm256_maddubs_epi16(uq_base_8bit_lo_1, vq_8bit_lo_1);
      val q_16bit_lo_2 = _mm256_maddubs_epi16(uq_base_8bit_lo_2, vq_8bit_lo_2);
      //
      // Perform reduction
      //
      val q_16bit_1 = _mm256_add_epi16(q_16bit_hi_1, q_16bit_lo_1);
      val q_16bit_2 = _mm256_add_epi16(q_16bit_hi_2, q_16bit_lo_2);
      val q_16bit   = _mm256_add_epi16(q_16bit_1, q_16bit_2);
      //
      // Convert the 16-bit chunks into 32-bit, by horizontal addition
      //
      val q_32bit = _mm256_madd_epi16(q_16bit, one_16bit);
      val q_ps    = _mm256_cvtepi32_ps(q_32bit);
      //
      // Accumulate
      //
      acc = _mm256_add_ps(acc, q_ps);
    })

    reduce_sum(acc) * su * sv
  }

  def indicator (x: Rep[Float]): Rep[UByte] = {
    val r_number = random_float_RDRAND()
    val cond = x >= r_number
    infix_cast[UByte](cond)
  }

    def quantize_staged (u: Rep[Array[Float]], r_imm: Rep[Array[UByte]], n0: Rep[Int]): Rep[Float] = {

      import QVector.IR.ImplicitLift._
      val r = reflectMutableSym(r_imm.asInstanceOf[Sym[Array[UByte]]])

      val hazy_first_bit_zero_32 = infix_cast[UInt](0x7FFFFFEF)
      val hazy_first_bit_one_32 = infix_cast[UInt](1) << UInt(31)

      val absMax = abs_max(u, n0)
      val scaled_rcp_max = 7.0f / absMax

      val n1 = (n0 >> 1) << 1

      forloop(0, n1, fresh[Int], 2, (i: Rep[Int]) =>
      {
        val val1 = u(i + 0)
        val val2 = u(i + 1)

        val signbit1 = infix_cast[UByte]( (hazy_first_bit_one_32 & reinterpret_cast[UInt](val1)) >> UInt(24) )
        val signbit2 = infix_cast[UByte]( (hazy_first_bit_one_32 & reinterpret_cast[UInt](val2)) >> UInt(28) )

        val signmask = signbit1 | signbit2

        val u_value_abs1 = hazy_first_bit_zero_32 & reinterpret_cast[UInt](val1)
        val u_value_abs2 = hazy_first_bit_zero_32 & reinterpret_cast[UInt](val2)

        val value_abs1 = reinterpret_cast[Float](u_value_abs1)
        val value_abs2 = reinterpret_cast[Float](u_value_abs2)

        val projected_value1 = value_abs1 * scaled_rcp_max
        val projected_value2 = value_abs2 * scaled_rcp_max

        val L1 = Math.floor (projected_value1)
        val L2 = Math.floor (projected_value2)

        val probability1 = projected_value1 - L1
        val probability2 = projected_value2 - L2

        val ind1 = indicator(probability1)
        val ind2 = indicator(probability2)

        val qval1 = ind1 + infix_cast[UByte](L1)
        val qval2 = ind2 + infix_cast[UByte](L2)

        r(i >> 1) = ((qval1 << UByte(4)) | qval2) | signmask
      })

      if (n0 % 2 > 0)
      {
        val val1 = u(n1)

        val signbit1 = infix_cast[UByte]( (hazy_first_bit_one_32 & reinterpret_cast[UInt](val1)) >> UInt(24) )
        val signmask = signbit1

        val u_value_abs1 = hazy_first_bit_zero_32 & reinterpret_cast[UInt](val1)
        val value_abs1 = reinterpret_cast[Float](u_value_abs1)
        val projected_value1 = value_abs1 * scaled_rcp_max

        val L1 = Math.floor (projected_value1)
        val probability1 = projected_value1 - L1
        val ind1 = indicator(probability1)
        val qval1 = ind1 + infix_cast[UByte](L1)

        r(n1 >> 1) = ((qval1 << UByte(4)) | UByte(0)) | signmask
      }

      absMax / 7.0f
    }


  def restore_staged (u: Rep[Array[Byte]], r_imm: Rep[Array[Float]], n0: Rep[Int]): Rep[Unit] =
  {
    // no implementation required for now
  }

  def dot(other: QVector4) : Float = {
    assert(other.size == size)
    dotNative(this.values, other.values, this.scale, other.scale, size_pad)
  }

  def quantize (input  : Array[Float]) : Unit = {
    scale = quantizeNative(input, values, size)
  }
  def restore  (output : Array[Float]) : Unit = restoreNative (values, output, size)

  @native def dotNative      (u: Array[Byte] , r: Array[Byte] , su_ss: Float, sv_ss: Float, n0: Int): Float
  @native def quantizeNative (u: Array[Float], r: Array[Byte] , n0: Int): Float
  @native def restoreNative  (u: Array[Byte] , r: Array[Float], n0: Int): Unit

  compile(quantize_staged _, this, nameOf(quantizeNative _))
  compile(restore_staged  _, this, nameOf(restoreNative  _))
  compile(dot_staged      _, this, nameOf(dotNative  _))

}
