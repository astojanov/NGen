package ch.ethz.acl.ngen.precison

import ch.ethz.acl.passera.unsigned.{UByte, UInt}
import com.github.dwickern.macros.NameOf._

class QVector8 (s: Int) extends QVector(8, s){

  import QVector.IR._

  val size_pad: Int = size % 32 match {
    case 0 => size
    case _ => size + 32 - (size % 32);
  }

  val values = new Array[Byte](size_pad)
  var scale = 1f

  for (i <- 0 until size_pad) {
    values (i) = 0
  }

  def dot_staged(
    u_short : Rep[Array[Byte]],
    v_short : Rep[Array[Byte]],
    su_ss   : Rep[Float],
    sv_ss   : Rep[Float],
    n0      : Rep[Int]
  ): Rep[Float] = {

    import QVector.IR.ImplicitLift._

    val u  = u_short.asInstanceOf[Exp[Array[__m256i]]]
    val v  = v_short.asInstanceOf[Exp[Array[__m256i]]]

    val ymm_ones_16bit = _mm256_set1_epi16(1.toShort)
    var ymm_sum        = _mm256_setzero_ps()

    forloop(0, n0, fresh[Int], 32, (i: Rep[Int]) => // 1 cache line per 5 cycle throughput. Looks OK
    {
      val qu = _mm256_loadu_si256(u, i)
      val qv = _mm256_loadu_si256(v, i)
      val qv_sgn = _mm256_sign_epi8(qv, qu)               // 1-latency 0.5-throughput
      val qu_abs = _mm256_abs_epi8(qu);                   // 1-latency
      val ymm1 = _mm256_maddubs_epi16(qu_abs, qv_sgn);    // 5-latency 1-throughput
      val ymm2 = _mm256_madd_epi16(ymm1, ymm_ones_16bit); // 5-latency 1-throughput
      val ymm3 = _mm256_cvtepi32_ps(ymm2);                // 3-latency 1-throughput
      ymm_sum = _mm256_add_ps(ymm_sum, ymm3);             // 3-latency 1-throughput
    })

    reduce_sum(ymm_sum) * su_ss * sv_ss
  }

  def quantize_staged (u: Rep[Array[Float]], r_imm: Rep[Array[Byte]], n0: Rep[Int]): Rep[Float] = {

    import QVector.IR.ImplicitLift._
    val r = reflectMutableSym(r_imm.asInstanceOf[Sym[Array[Byte]]])

    val max = abs_max(u, n0)
    val s = 127.0f / max
    forloop(0, n0, fresh[Int], 1, (i: Rep[Int]) => {
       val rnd = random_float_RDRAND()
      r(i) = infix_cast[Byte](Math.floor(s * u(i) + rnd))
    })
    max / 127.0f
  }

  def restore_staged (u: Rep[Array[Byte]], r_imm: Rep[Array[Float]], n0: Rep[Int]): Rep[Unit] =
  {
    // no implementation provided
  }

 def dot(other: QVector8) : Float = {
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


  def print (): Unit = {
    for (i <- 0 until size_pad) {
      println(values(i) * scale)
    }
  }
}
