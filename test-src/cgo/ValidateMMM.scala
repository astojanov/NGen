package cgo

import ch.ethz.acl.ngen.mmm.MMM
import org.scalatest.FunSpec

class ValidateMMM extends FunSpec {
  //
  // Random Number Generator
  //
  lazy val rng = new scala.util.Random(7919)
  //
  // The precision of the float equality
  //
  val precision = 0.1F

  //
  // Generate a random floating point array
  //
  def genRandomFloatArray(n: Int, maximum: Float = 3.0F): Array[Float] = {
    Array.fill(n) { rng.nextFloat * maximum }
  }

  //
  // Checks the element-wise equality of two matrices
  //
  def checkMatricesEqual(expectedMatrix: Array[Float], observedMatrix: Array[Float]): Boolean = {
    expectedMatrix.zip(observedMatrix).map{case (x1, x2) => (x1 - x2).abs <= precision}.reduce(_ && _)
  }

  describe("ValidateMMM.scala") {
    val n = 1024
    val nSquare = n * n

    val a = genRandomFloatArray(nSquare)
    val b = genRandomFloatArray(nSquare)
    val result = Array.fill(nSquare){0.0F}
    val resultBlocked = Array.fill(nSquare){0.0F}
    val resultNative = Array.fill(nSquare){0.0F}

    MMM.jMMM.baseline(a, b, result, n)
    MMM.jMMM.blocked(a, b, resultBlocked, n)
    MMM.nMMM.blocked(a, b, resultNative, n)

    assert(checkMatricesEqual(result, resultBlocked))
    assert(checkMatricesEqual(result, resultNative))
  }
}
