package ch.ethz.acl.ngen.saxpy

import ch.ethz.acl.commons.cir.IntrinsicsIR
import com.github.dwickern.macros.NameOf._

class NSaxpy {

  // Step 1: Placeholder for the SAXPY native function
  @native def apply (
    a      : Array[Float],
    b      : Array[Float],
    scalar : Float,
    n      : Int
  ): Unit

  // Step 2: DSL instance of the intrinsics
  val cIR = new IntrinsicsIR
  import cIR._

  // Step 3: Staged SAXPY function using AVX + FMA
  def saxpy_staged(
    a_imm  : Rep[Array[Float]],
    b      : Rep[Array[Float]],
    scalar : Rep[Float],
    n      : Rep[Int]
  ): Rep[Unit] = { import ImplicitLift._
    // make array `a` mutable
    val a_sym = a_imm.asInstanceOf[Sym[Array[Float]]]
    val a = reflectMutableSym(a_sym)
    // start with the computation
    val n0 = (n >> 3) << 3
    val vec_s = _mm256_set1_ps(scalar)
    forloop(0, n0, fresh[Int], 8, (i : Rep[Int]) => {
      val vec_a = _mm256_loadu_ps(a, i)
      val vec_b = _mm256_loadu_ps(b, i)
      val res = _mm256_fmadd_ps(vec_b, vec_s, vec_a)
      _mm256_storeu_ps(a, res, i)
    })
    forloop(n0, n, fresh[Int], 1, (i : Rep[Int]) => {
      a(i) = a(i) + b(i) * scalar
    })
  }

  // Step 4: generate the daxpy function,
  // compile it and link it to the JVM
  compile(saxpy_staged _, this, nameOf(apply _))
}