package ch.ethz.acl.ngen.mmm

import ch.ethz.acl.commons.cir.IntrinsicsIR
import com.github.dwickern.macros.NameOf._

class NMMM { self =>

  @native def blocked (
    a: Array[Float],
    b: Array[Float],
    c: Array[Float],
    n: Int
  ): Unit

  val cIR = new IntrinsicsIR
  import cIR._

  def transpose(row: Seq[Exp[__m256]]): Seq[Exp[__m256]] = {
    import ImplicitLift._

    val __tt = row.grouped(2).toSeq.flatMap({
      case Seq(a, b) => Seq (
        _mm256_unpacklo_ps(a, b),
        _mm256_unpackhi_ps(a, b)
      )
    }).grouped(4).toSeq.flatMap({
      case Seq(a, b, c, d) => Seq(
        _mm256_shuffle_ps(a, c, 68),
        _mm256_shuffle_ps(a, c, 238),
        _mm256_shuffle_ps(b, d, 68),
        _mm256_shuffle_ps(b, d, 238)
      )
    })
    val zip = __tt.take(4) zip __tt.drop(4)
    val f = _mm256_permute2f128_ps _
    zip.map({ case (a, b) => f(a, b, 0x20) }) ++
    zip.map({ case (a, b) => f(a, b, 0x31) })
  }

  def staged_mmm_blocked (
    a     : Rep[Array[Float]],
    b     : Rep[Array[Float]],
    c_imm : Rep[Array[Float]],
    n     : Rep[Int]
  ): Rep[Unit] = {

    import ImplicitLift._
    val c_sym = c_imm.asInstanceOf[Sym[Array[Float]]]
    val c = reflectMutableSym(c_sym)

    forloop(0, n, fresh[Int], 8, (kk: Exp[Int]) => {
      forloop(0, n, fresh[Int], 8, (jj: Exp[Int]) => {
        //
        // Retrieve the current block of B and transpose it
        //
        val blockB = transpose((0 to 7).map { i =>
            _mm256_loadu_ps(b, (kk + i) * n + jj)
        })
        //
        // Multiply all the vectors of a of the corresponding
        // block column with the running block and store the
        // result
        //
        loop(n, (i: Exp[Int]) => {
          val rowA  = _mm256_loadu_ps(a, i * n + kk)
          val mulAB = transpose(
            blockB.map(_mm256_mul_ps(rowA, _))
          )
          def f(l: Seq[Exp[__m256]]): Exp[__m256] =
            l.size match {
              case 1 => l.head
              case s =>
                val lhs = f(l.take(s/2))
                val rhs = f(l.drop(s/2))
                _mm256_add_ps(lhs, rhs)
          }
          val rowC = _mm256_loadu_ps(c, i * n + jj)
          val accC = _mm256_add_ps(f(mulAB), rowC)
          _mm256_storeu_ps(c, accC, i * n + jj)
        })
      })
    })
  }

  compile(staged_mmm_blocked _, this, nameOf(blocked _))
}
