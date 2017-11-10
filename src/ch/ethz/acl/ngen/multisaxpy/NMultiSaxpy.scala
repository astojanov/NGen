package ch.ethz.acl.ngen.multisaxpy

import ch.ethz.acl.commons.cir.IntrinsicsIR
import ch.ethz.acl.commons.compiler.ISA
import ch.ethz.acl.commons.system.LocalSystem
import com.github.dwickern.macros.NameOf._

class NMultiSaxpy {

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


  def saxpy_AVX (
    a      : Rep[Array[Float]],
    b      : Rep[Array[Float]],
    scalar : Rep[Float],
    n      : Rep[Int]
  ): Unit = { import ImplicitLift._
    val n0 = (n >> 3) << 3
    val vec_s = _mm256_set1_ps(scalar)
    forloop(0, n0, fresh[Int], 8, (i : Rep[Int]) => {
      val vec_a = _mm256_loadu_ps(a, i)
      val vec_b = _mm256_loadu_ps(b, i)
      val res = if (LocalSystem.getISAs().contains(ISA.FMA)) {
        _mm256_fmadd_ps(vec_b, vec_s, vec_a)
      } else {
          val mul_ab = _mm256_mul_ps(vec_b, vec_s)
        _mm256_add_ps(mul_ab, vec_a)
      }
      _mm256_storeu_ps(a, res, i)
    })
    forloop(n0, n, fresh[Int], 1, (i : Rep[Int]) => {
      a(i) = a(i) + b(i) * scalar
    })
  }

  def saxpy_SSE (
    a      : Rep[Array[Float]],
    b      : Rep[Array[Float]],
    scalar : Rep[Float],
    n      : Rep[Int]
  ): Unit = { import ImplicitLift._
    val n0 = (n >> 2) << 2
    val vec_s = _mm_set1_ps(scalar)
    forloop(0, n0, fresh[Int], 4, (i : Rep[Int]) => {
      val vec_a = _mm_loadu_ps(a, i)
      val vec_b = _mm_loadu_ps(b, i)
      val res = if (LocalSystem.getISAs().contains(ISA.FMA)) {
        _mm_fmadd_ps(vec_b, vec_s, vec_a)
      } else {
        val mul_ab = _mm_mul_ps(vec_b, vec_s)
        _mm_add_ps(mul_ab, vec_a)
      }
      _mm_storeu_ps(a, res, i)
    })
    forloop(n0, n, fresh[Int], 1, (i : Rep[Int]) => {
      a(i) = a(i) + b(i) * scalar
    })
  }

  def saxpy_scalar (
    a      : Rep[Array[Float]],
    b      : Rep[Array[Float]],
    scalar : Rep[Float],
    n      : Rep[Int]
  ): Unit = { import ImplicitLift._
    forloop(0, n, fresh[Int], 1, (i : Rep[Int]) => {
      a(i) = a(i) + b(i) * scalar
    })
  }


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
    val systemISAs = LocalSystem.getISAs()
    if (systemISAs.contains(ISA.AVX)) {
      saxpy_AVX(a, b, scalar, n)
    } else if (systemISAs.contains(ISA.SSE)) {
      saxpy_SSE(a, b, scalar, n)
    } else {
      saxpy_scalar(a, b, scalar, n)
    }
  }

  // Step 4: generate the daxpy function,
  // compile it and link it to the JVM
  compile(saxpy_staged _, this, nameOf(apply _))
}
