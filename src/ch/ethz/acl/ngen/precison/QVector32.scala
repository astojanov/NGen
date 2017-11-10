package ch.ethz.acl.ngen.precison

import com.github.dwickern.macros.NameOf._


class QVector32(s: Int) extends QVector(32, s) {

  import QVector.IR._
  val values = new Array[Float](size)
  val size_pad = size

  def dot_staged(u: Rep[Array[Float]], v: Rep[Array[Float]], n0: Rep[Int]): Rep[Float] = {

    import QVector.IR.ImplicitLift._
    val n1 = (n0 >> 5) << 5

    var acc1 = _mm256_setzero_ps()
    var acc2 = _mm256_setzero_ps()
    var acc3 = _mm256_setzero_ps()
    var acc4 = _mm256_setzero_ps()

    forloop(0, n1, fresh[Int], 32, (i: Rep[Int]) =>
    {
      val v1 = _mm256_loadu_ps(v, i + 0)
      val v2 = _mm256_loadu_ps(v, i + 8)
      val v3 = _mm256_loadu_ps(v, i + 16)
      val v4 = _mm256_loadu_ps(v, i + 24)

      val u1 = _mm256_loadu_ps(u, i + 0)
      val u2 = _mm256_loadu_ps(u, i + 8)
      val u3 = _mm256_loadu_ps(u, i + 16)
      val u4 = _mm256_loadu_ps(u, i + 24)

      acc1 = _mm256_fmadd_ps(v1, u1, acc1)
      acc2 = _mm256_fmadd_ps(v2, u2, acc2)
      acc3 = _mm256_fmadd_ps(v3, u3, acc3)
      acc4 = _mm256_fmadd_ps(v4, u4, acc4)
    })

    // add the accumulators
    val tmp1 = _mm256_add_ps(acc1, acc2)
    val tmp2 = _mm256_add_ps(acc3, acc4)
    val tmp3 = _mm256_add_ps(tmp1, tmp2)

    var result = reduce_sum(tmp3)

    forloop(n1, n0, fresh[Int], 1, (i: Rep[Int]) => {
      result += u(i) * v(i)
    })

    readVar(result)
  }

  def dot(other: QVector32) : Float = {
    assert(other.size == size)
    dotNative(this.values, other.values, size)
  }
  def quantize (input  : Array[Float]) : Unit = input.copyToArray(values)
  def restore  (output : Array[Float]) : Unit = values.copyToArray(output)

  @native def dotNative      (u: Array[Float], r: Array[Float], n0: Int): Float
  compile(dot_staged      _, this, nameOf(dotNative  _))


  def print(): Unit = {
    for (i <- 0 until size) {
      println(values(i))
    }
  }
}
