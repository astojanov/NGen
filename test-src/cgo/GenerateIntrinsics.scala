package cgo

import ch.ethz.acl.intrinsics.GenerateIntrinsicsDSLs
import org.scalatest.FunSpec

class GenerateIntrinsics extends FunSpec {
  describe("GenerateIntrinsics") {
    val generator = new GenerateIntrinsicsDSLs
    generator.generate("Generated_SIMD_Intrinsics")
  }

}
