package ch.ethz.acl.ngen.mmm

import ch.ethz.acl.commons.system.LocalSystem
import ch.ethz.acl.commons.util.DebuggingConfig
import org.scalameter.{Bench, CurveData}
import org.scalameter.api._
import org.scalameter.utils.Tree
import org.scalameter.picklers.Implicits._

class BenchMMM extends Bench.ForkedTime {
  //
  // Random Number Generator
  //
  lazy val rng = new scala.util.Random(7919)
  //
  // Ignore LMS messages
  //
  DebuggingConfig.verbosity = 0
  //
  // Define test sizes for MMM 8 - 1024
  //
  val sizesMMM: Gen[Int] = Gen.enumeration("size")(8, 32, 64, 128, 192, 256, 320, 384, 448, 512, 576, 640, 704, 768, 832, 896, 960, 1024)
  //
  // Generate the arguments
  //
  val argsMMM: Gen[(Array[Float], Array[Float], Array[Float], Int)] =
    sizesMMM.map(size => (genRandomFloatArray(size * size), genRandomFloatArray(size * size), new Array[Float](size * size), size))
  //
  // Measure the performance of the two implementations
  //
  performance of "MMM" config (
    exec.minWarmupRuns -> 100,
    exec.maxWarmupRuns -> 100,
    exec.independentSamples -> 1
  ) in {
    measure method "jMMM.blocked (JVM implementation)" in {
      using(argsMMM) in {
        case (a, b, c, size) => MMM.jMMM.blocked(a, b, c, size)
      }
    }
    measure method "nMMM.blocked (LMS generated)" in {
      using(argsMMM) in {
        case (a, b, c, size) => MMM.nMMM.blocked(a, b, c, size)
      }
    }
    measure method "jMMM.baseline (JVM implementation)" in {
      using(argsMMM) in {
        case (a, b, c, size) => MMM.jMMM.baseline(a, b, c, size)
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
        val flops = size * size * size * 2
        val cycles = measurement.value * LocalSystem.getFreq()
        println("%12.0f | %.10f".format(size, flops / cycles))
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

