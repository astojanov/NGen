package cgo

import ch.ethz.acl.ngen.multisaxpy.MultiSaxpy
import org.scalatest.FunSpec

class ExecuteMultiSaxpy extends FunSpec {

  describe("ExecuteMultiSaxpy") {

    lazy val rng = new scala.util.Random(7919)

    val a = Array.fill(13) { rng.nextFloat * 10 }
    val a1 = a.clone()
    val a2 = a.clone()
    val b = Array.fill(13) { rng.nextFloat * 10 }
    val scalar = rng.nextFloat * 10

    MultiSaxpy.nSaxpy(a1, b, scalar, b.length)
    MultiSaxpy.jSaxpy(a2, b, scalar, b.length)

    println("====================================================")
    println(s"Executing : Multi-architectre SAXPY")
    println("----------------------------------------------------")
    println("Input")
    println("A = [ " + a.map(t => "%.3f".format(t)).mkString(" ") + " ]")
    println("B = [ " + b.map(t => "%.3f".format(t)).mkString(" ") + " ]")
    println("y = [ " + scalar + " ]")
    println("----------------------------------------------------")
    println("Output: A = A + By")
    println("LMS Generated : [ " + a1.map(t => "%.3f".format(t)).mkString(" ") + " ]")
    println("JVM Computed  : [ " + a2.map(t => "%.3f".format(t)).mkString(" ") + " ]")
    println("====================================================")
  }

}
