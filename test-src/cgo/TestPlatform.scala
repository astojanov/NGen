package cgo

import ch.ethz.acl.commons.system.LocalSystem
import org.scalatest.FunSpec

class TestPlatform extends FunSpec {
  describe("TestPlaform") {
    LocalSystem.detect()
  }
}
