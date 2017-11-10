package ch.ethz.acl.ngen.mmm

import org.scalameter.Bench
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
//
// Runs a single test of jMMM, warming up the JIT
// 20000 times, and then prints the compiled version
//
class InspectMMM extends Bench.ForkedTime {
  //
  // Random Number Generator
  //
  lazy val rng = new scala.util.Random(7919)
  //
  // Define exponential sizes 256 bytes to 16 MB
  //
  val sizesMMM: Gen[Int] = Gen.single("size")(128)
  //
  // Generate the arguments
  //
  println("prep arguments")
  val argsMMM: Gen[(Array[Float], Array[Float], Array[Float], Int)] =
    sizesMMM.map(size => (genRandomFloatArray(size * size), genRandomFloatArray(size * size), new Array[Float](size * size), size))
  //
  // Measure the performance of the two implementations
  //
  performance of "MMM" config (
    exec.minWarmupRuns -> 20000,
    exec.maxWarmupRuns -> 20000,
    exec.jvmflags -> List("-XX:+UnlockDiagnosticVMOptions", "-XX:CompileCommand=print,*JMMM.blocked"),
    exec.independentSamples -> 1
  ) in {
    measure method "java" in {
      using(argsMMM) in {
        case (a, b, c, size) => {
          MMM.jMMM.blocked(a, b, c, size)
        }
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

