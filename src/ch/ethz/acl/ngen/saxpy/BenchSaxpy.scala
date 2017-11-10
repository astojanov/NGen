package ch.ethz.acl.ngen.saxpy

import ch.ethz.acl.commons.system.LocalSystem
import ch.ethz.acl.commons.util.DebuggingConfig
import org.scalameter.api._
import org.scalameter.utils.Tree
import org.scalameter.{Bench, CurveData}

class BenchSaxpy extends Bench.ForkedTime {
  //
  // Random Number Generator
  //
  lazy val rng = new scala.util.Random(7919)
  //
  // Ignore LMS messages
  //
  DebuggingConfig.verbosity = 0
  //
  // Define exponential sizes 256 bytes to 16 MB
  //
  val sizePow2: Gen[Int] = Gen.exponential("size")(64, 4194304, 2)
  //
  // Generate the arguments
  //
  val scalaArguments: Gen[(Array[Float], Array[Float], Float, Int)] =
    sizePow2.map(size => (genRandomFloatArray(size), genRandomFloatArray(size), rng.nextFloat, size))
  //
  // Measure the performance of the two implementations
  //
  performance of "SAXPY" config (
    exec.minWarmupRuns -> 20000,
    exec.maxWarmupRuns -> 20000,
    exec.benchRuns -> 1000,
    exec.independentSamples -> 1
  ) in {
    measure method "jSaxpy (JVM implementation)" in {
      using(scalaArguments) in {
        case (a, b, scalar, size) => Saxpy.jSaxpy(a, b, scalar, size)
      }
    }
    measure method "nSaxpy (LMS generated)" in {
      using(scalaArguments) in {
        case (a, b, scalar, size) => Saxpy.nSaxpy(a, b, scalar, size)
      }
    }
  }
  //
  // Use median to aggregate the data
  //
  override def aggregator: Aggregator[Double] = Aggregator.median
  //
  // Report the results in Flops / Cycle
  //
  override def reporter: Reporter[Double] = new org.scalameter.Reporter[Double]
  {
    def report(result: CurveData[Double], persistor: Persistor) {
      //
      //  Output the name of the benchmark
      //
      println("====================================================")
      println(s"Benchmarking ${result.context.scope}")
      println("----------------------------------------------------")
      println("    Size (N) | Flops / Cycle")
      println("----------------------------------------------------")
      //
      // Convert each measurements into Flops / Cycle
      //
      for (measurement <- result.measurements) {
        val size = implicitly[Numeric[Int]].toDouble(measurement.params("size"))
        val flops = size * 2
        val cycles = measurement.value * LocalSystem.getFreq()
        println("%12.0f | %.3f".format(size, flops / cycles))
      }

      println("====================================================")
      println()
    }
    def report(result: Tree[CurveData[Double]], persistor: Persistor) = true
  }
  //
  // Generate a random floating point array
  //
  def genRandomFloatArray(n: Int, maximum: Float = 3.0F): Array[Float] = {
    Array.fill(n) { rng.nextFloat * maximum }
  }

}
