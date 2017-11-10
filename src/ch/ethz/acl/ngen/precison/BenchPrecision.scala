package ch.ethz.acl.ngen.precison

import ch.ethz.acl.commons.system.LocalSystem
import ch.ethz.acl.commons.util.DebuggingConfig
import ch.ethz.acl.ngen.mmm.MMM
import org.scalameter.{Bench, CurveData}
import org.scalameter.api._
import org.scalameter.utils.Tree

class BenchPrecision extends Bench.ForkedTime {
  //
  // Random Number Generator
  //
  lazy val rng = new scala.util.Random(7919)
  //
  // Ignore LMS messages
  //
  DebuggingConfig.verbosity = 0
  //
  // Generate the sizes
  //
  val sizePow2: Gen[Int] = Gen.exponential("size")(128, 4194304 * 16, 2)
  //
  // Generate the arguments
  //
  val argsVector: Gen[(Array[Float], Array[Float])] = sizePow2.map(size => (genRandomFloatArray(size), genRandomFloatArray(size)))

  //
  // Staging world
  //
  val argsQVector32: Gen[(QVector32, QVector32)] = argsVector map {
    case (a, b) => {
      val qa = new QVector32(a.length)
      val qb = new QVector32(b.length)
      qa.quantize(a)
      qb.quantize(b)
      (qa, qb)
    }
  }
  val argsQVector16: Gen[(QVector16, QVector16)] = argsVector map {
    case (a, b) => {
      val qa = new QVector16(a.length)
      val qb = new QVector16(b.length)
      qa.quantize(a)
      qb.quantize(b)
      (qa, qb)
    }
  }

  val argsQVector8: Gen[(QVector8, QVector8)] = argsVector map {
    case (a, b) => {
      val qa = new QVector8(a.length)
      val qb = new QVector8(b.length)
      qa.quantize(a)
      qb.quantize(b)
      (qa, qb)
    }
  }

  val argsQVector4: Gen[(QVector4, QVector4)] = argsVector map {
    case (a, b) => {
      val qa = new QVector4(a.length)
      val qb = new QVector4(b.length)
      qa.quantize(a)
      qb.quantize(b)
      (qa, qb)
    }
  }

  //
  // Java World
  //

  val argsJVector32: Gen[(JVector32, JVector32)] = argsQVector32 map {
    case (qa, qb) => {
      val ja = new JVector32(qa.size_pad)
      val jb = new JVector32(qb.size_pad)
      ja.values = qa.values
      jb.values = qb.values
      (ja, jb)
    }
  }
  val argsJVector16: Gen[(JVector16, JVector16)] = argsVector map {
    case (qa, qb) => {
      val ja = new JVector16(qa.size)
      val jb = new JVector16(qb.size)
      ja.quantize(qa)
      jb.quantize(qb)
      (ja, jb)
    }
  }
  val argsJVector8: Gen[(JVector8, JVector8)] = argsQVector8 map {
    case (qa, qb) => {
      val ja = new JVector8(qa.size_pad)
      val jb = new JVector8(qb.size_pad)
      ja.values = qa.values
      jb.values = qb.values
      ja.scale = qa.scale
      jb.scale = qb.scale
      (ja, jb)
    }
  }
  val argsJVector4: Gen[(JVector4, JVector4)] = argsQVector4 map {
    case (qa, qb) => {
      val ja = new JVector4(qa.size_pad)
      val jb = new JVector4(qb.size_pad)
      ja.values = qa.values
      jb.values = qb.values
      ja.scale = qa.scale
      jb.scale = qb.scale
      (ja, jb)
    }
  }

  //
  // Measure the performance of the two implementations
  //
  performance of "DotProduct" config (
    exec.minWarmupRuns -> 100,
    exec.maxWarmupRuns -> 100,
    exec.independentSamples -> 1
  ) in {
    measure method "JVector32" in {
      using(argsJVector32) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "JVector16" in {
      using(argsJVector16) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "JVector8" in {
      using(argsJVector8) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "JVector4" in {
      using(argsJVector4) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "QVector32" in {
      using(argsQVector32) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "QVector16" in {
      using(argsQVector16) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "QVector8" in {
      using(argsQVector8) in {
        case (a, b) => a.dot(b)
      }
    }
    measure method "QVector4" in {
      using(argsQVector4) in {
        case (a, b) => a.dot(b)
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
      println("======================================================================")
      println(s"Benchmarking ${result.context.scope}")
      println("----------------------------------------------------------------------")
      println("    Size (N) |               Cycles |  Flops / Cycle | Bandwidth MB/s")
      println("----------------------------------------------------------------------")
      //
      // Convert each measurements into Flops / Cycle
      //
      for (measurement <- result.measurements) {
        val size = implicitly[Numeric[Int]].toDouble(measurement.params("size"))
        val bytes = result.context.scope.toString match {
          case "DotProduct.QVector4"  => 0.5 * size * 2.0
          case "DotProduct.QVector8"  => 1.0 * size * 2.0
          case "DotProduct.QVector16" => 2.0 * size * 2.0
          case "DotProduct.QVector32" => 4.0 * size * 2.0
          case "DotProduct.JVector4"  => 0.5 * size * 2.0
          case "DotProduct.JVector8"  => 1.0 * size * 2.0
          case "DotProduct.JVector16" => 2.0 * size * 2.0
          case "DotProduct.JVector32" => 4.0 * size * 2.0
          case _ => 0
        }
        val mbytes = bytes / 1024.0 / 1024.0
        val mb_per_sec = mbytes / measurement.value * 1000
        val cycles = measurement.value * LocalSystem.getFreq()
        val flops = size * 2
        println("%12.0f | %20.0f |  %4.10f | %10.3f".format(size, cycles, flops / cycles, mb_per_sec))
      }
      println("======================================================================")
      println()
    }
    def report(result: Tree[CurveData[Double]], persistor: Persistor) = true
  }
  //
  // Generate a random floating point array
  //
  def genRandomFloatArray(n: Int, maximum: Float = 3.0F): Array[Float] = {

//    println("Generating vector of size: ", n)

    Array.fill(n) { rng.nextFloat * maximum }
  }

}
