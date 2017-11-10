package ch.ethz.acl.ngen.precison

import ch.ethz.acl.passera.unsigned.{UInt, UShort}
import com.github.dwickern.macros.NameOf._


class QVector16 (s: Int) extends QVector(16, s) {

  import QVector.IR._
  private val QVECTOR16FBLOCK = 64

  val size_pad: Int = size % QVECTOR16FBLOCK match {
    case 0 => size
    case _ => size + QVECTOR16FBLOCK - (size % QVECTOR16FBLOCK)
  }
  val values = new Array[Short](size_pad)
  //
  // Initialize to zero
  //
  for (i <- 0 until size_pad) {
    values (i) = 0
  }

  def quantize_staged(u: Rep[Array[Float]], r_imm: Rep[Array[Short]], n0: Rep[Int]): Rep[Unit] = {

    import QVector.IR.ImplicitLift._
    val r = reflectMutableSym(r_imm.asInstanceOf[Sym[Array[__m128i]]])

    val n1 = (n0 >> 5) << 5
    val n2 = (n0 >> 3) << 3

    forloop(0, n1, fresh[Int], 32, (i: Rep[Int]) => {

      val f0 = _mm256_loadu_ps(u, i + 0)
      val f1 = _mm256_loadu_ps(u, i + 8)
      val f2 = _mm256_loadu_ps(u, i + 16)
      val f3 = _mm256_loadu_ps(u, i + 24)

      val q0 = _mm256_cvtps_ph(f0, 0)
      val q1 = _mm256_cvtps_ph(f1, 0)
      val q2 = _mm256_cvtps_ph(f2, 0)
      val q3 = _mm256_cvtps_ph(f3, 0)

      _mm_storeu_si128 (r, q0, i +  0)
      _mm_storeu_si128 (r, q1, i +  8)
      _mm_storeu_si128 (r, q2, i + 16)
      _mm_storeu_si128 (r, q3, i + 24)

    })

    forloop(n1, n2, fresh[Int], 8, (i: Rep[Int]) => {
      val f0 = _mm256_loadu_ps(u, i)
      val q0 = _mm256_cvtps_ph(f0, 0)
      _mm_storeu_si128 (r, q0, i)
    })

    val remainder = n0 - n2
    if (remainder > 0)
    {
      var load_mask = _mm256_setr_epi32(-1, -1, -1, -1, -1, -1, -1, +0)
      switch (remainder)(
        _case(1) -> { load_mask = _mm256_setr_epi32(-1, +0, +0, +0, +0, +0, +0, +0) },
        _case(2) -> { load_mask = _mm256_setr_epi32(-1, -1, +0, +0, +0, +0, +0, +0) },
        _case(3) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, +0, +0, +0, +0, +0) },
        _case(4) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, -1, +0, +0, +0, +0) },
        _case(5) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, -1, -1, +0, +0, +0) },
        _case(6) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, -1, -1, -1, -1, +0) }
      )(
        _default -> {}
      )

      val f0 = _mm256_maskload_ps(u, load_mask, n2)
      val q0 = _mm256_cvtps_ph(f0, 0)
      _mm_storeu_si128(r, q0, n2)
    }
  }


  def dot_staged(u_short: Rep[Array[Short]], v_short: Rep[Array[Short]], n0: Rep[Int]): Rep[Float] = {

    import QVector.IR.ImplicitLift._

    val u = u_short.asInstanceOf[Exp[Array[__m256i]]]
    val v = v_short.asInstanceOf[Exp[Array[__m256i]]]

    var acc0 = _mm256_setzero_ps()
    var acc1 = _mm256_setzero_ps()
    var acc2 = _mm256_setzero_ps()
    var acc3 = _mm256_setzero_ps()

    forloop(0, n0, fresh[Int], 32, (i: Rep[Int]) => {

      val qu_0 = _mm256_loadu_si256 (u, i +  0)
      val qu_1 = _mm256_loadu_si256 (u, i + 16)
      val qv_0 = _mm256_loadu_si256 (v, i +  0)
      val qv_1 = _mm256_loadu_si256 (v, i + 16)

      val qu_0_lo = _mm256_castsi256_si128(qu_0)
      val qu_0_hi = _mm256_extractf128_si256(qu_0, 1)
      val qu_1_lo = _mm256_castsi256_si128(qu_1)
      val qu_1_hi = _mm256_extractf128_si256(qu_1, 1)

      val qv_0_lo = _mm256_castsi256_si128(qv_0)
      val qv_0_hi = _mm256_extractf128_si256(qv_0, 1)
      val qv_1_lo = _mm256_castsi256_si128(qv_1)
      val qv_1_hi = _mm256_extractf128_si256(qv_1, 1)

      val u_0_lo = _mm256_cvtph_ps(qu_0_lo)
      val u_0_hi = _mm256_cvtph_ps(qu_0_hi)
      val u_1_lo = _mm256_cvtph_ps(qu_1_lo)
      val u_1_hi = _mm256_cvtph_ps(qu_1_hi)

      val v_0_lo = _mm256_cvtph_ps(qv_0_lo)
      val v_0_hi = _mm256_cvtph_ps(qv_0_hi)
      val v_1_lo = _mm256_cvtph_ps(qv_1_lo)
      val v_1_hi = _mm256_cvtph_ps(qv_1_hi)

      acc0 = _mm256_fmadd_ps(v_0_lo, u_0_lo, acc0)
      acc1 = _mm256_fmadd_ps(v_0_hi, u_0_hi, acc1)
      acc2 = _mm256_fmadd_ps(v_1_lo, u_1_lo, acc2)
      acc3 = _mm256_fmadd_ps(v_1_hi, u_1_hi, acc3)
    })

    val sum0 = _mm256_add_ps(acc0, acc1)
    val sum1 = _mm256_add_ps(acc2, acc3)
    val sum2 = _mm256_add_ps(sum0, sum1)

    reduce_sum(sum2)
  }

  def restore_staged(u_short: Rep[Array[Short]], r_imm: Rep[Array[Float]], n0: Rep[Int]): Rep[Unit] = {

    import QVector.IR.ImplicitLift._
    val u = u_short.asInstanceOf[Exp[Array[__m128i]]]
    val r = reflectMutableSym(r_imm.asInstanceOf[Sym[Array[Float]]])

    val n1  = (n0 >> 5) << 5
    val n2  = (n0 >> 3) << 3

    forloop(0, n1, fresh[Int], 32, (i: Rep[Int]) => {

      val q0 = _mm_loadu_si128 (u, i +  0)
      val q1 = _mm_loadu_si128 (u, i +  8)
      val q2 = _mm_loadu_si128 (u, i + 16)
      val q3 = _mm_loadu_si128 (u, i + 24)

      val f0 = _mm256_cvtph_ps(q0)
      val f1 = _mm256_cvtph_ps(q1)
      val f2 = _mm256_cvtph_ps(q2)
      val f3 = _mm256_cvtph_ps(q3)

      _mm256_storeu_ps (r, f0, i +  0)
      _mm256_storeu_ps (r, f1, i +  8)
      _mm256_storeu_ps (r, f2, i + 16)
      _mm256_storeu_ps (r, f3, i + 24)
    })

    forloop(n1, n2, fresh[Int], 8, (i: Rep[Int]) => {
      val q0 = _mm_loadu_si128(u, i)
      val f0 = _mm256_cvtph_ps(q0)
      _mm256_storeu_ps (r, f0, i)
    })

    val remainder = n0 - n2
    if (remainder > 0)
    {
      var load_mask = _mm256_setr_epi32(-1, -1, -1, -1, -1, -1, -1, +0)
      switch (remainder)(
        _case(1) -> { load_mask = _mm256_setr_epi32(-1, +0, +0, +0, +0, +0, +0, +0) },
        _case(2) -> { load_mask = _mm256_setr_epi32(-1, -1, +0, +0, +0, +0, +0, +0) },
        _case(3) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, +0, +0, +0, +0, +0) },
        _case(4) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, -1, +0, +0, +0, +0) },
        _case(5) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, -1, -1, +0, +0, +0) },
        _case(6) -> { load_mask = _mm256_setr_epi32(-1, -1, -1, -1, -1, -1, -1, +0) }
      )(
        _default -> {}
      )

      val q0 = _mm_loadu_si128(u, n2)
      val f0 = _mm256_cvtph_ps(q0)
      _mm256_maskstore_ps(r, load_mask, f0, n2)
    }
  }

  def print(): Unit = {
    val tmp = new Array[Float](size)
    restore(tmp)
    tmp map println
  }

  def dot(other: QVector16) : Float = {
    assert(other.size == size)
    dotNative(this.values, other.values, size_pad)
  }
  def quantize (input  : Array[Float]) : Unit = quantizeNative(input, values, size)
  def restore  (output : Array[Float]) : Unit = restoreNative (values, output, size)

  @native def dotNative      (u: Array[Short], r: Array[Short], n0: Int): Float
  @native def quantizeNative (u: Array[Float], r: Array[Short], n0: Int): Unit
  @native def restoreNative  (u: Array[Short], r: Array[Float], n0: Int): Unit

  compile(quantize_staged _, this, nameOf(quantizeNative _))
  compile(restore_staged  _, this, nameOf(restoreNative  _))
  compile(dot_staged      _, this, nameOf(dotNative  _))
}
