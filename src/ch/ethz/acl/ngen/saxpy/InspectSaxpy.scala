package ch.ethz.acl.ngen.saxpy

import org.scalameter.Bench
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
//
// Runs a single test of jDaxpy, warming up the JIT
// 20000 times, and then prints the compiled version
//
class InspectSaxpy extends Bench.ForkedTime {
  //
  // Random Number Generator
  //
  lazy val rng = new scala.util.Random(7919)
  //
  // Define exponential sizes 256 bytes to 16 MB
  //
  val sizePow2: Gen[Int] = Gen.single("size")(4194304)
  //
  // Generate the arguments
  //
  val scalaArguments: Gen[(Array[Float], Array[Float], Float, Int)] =
  sizePow2.map(size => (genRandomFloatArray(size), genRandomFloatArray(size), rng.nextFloat, size))
  //
  // Measure the performance of the two implementations
  //
  performance of "DAXPY" config (
    exec.minWarmupRuns -> 20000,
    exec.maxWarmupRuns -> 20000,
    exec.jvmflags -> List("-XX:+UnlockDiagnosticVMOptions", "-XX:CompileCommand=print,*JSaxpy.apply"),
    exec.independentSamples -> 1
  ) in {
    measure method "java" in {
      using(scalaArguments) in {
        case (a, b, scalar, size) => Saxpy.jSaxpy(a, b, scalar, size)
      }
    }
  }
  override def reporter: Reporter[Double] = Reporter.None
  //
  // Generate a random floating point array
  //
  def genRandomFloatArray(n: Int, maximum: Float = 3.0F): Array[Float] = {
    Array.fill(n) { rng.nextFloat * maximum }
  }

}
