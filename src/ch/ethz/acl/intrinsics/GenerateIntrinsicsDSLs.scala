package ch.ethz.acl.intrinsics

import java.io.File

class GenerateIntrinsicsDSLs {

  def generate (relativePath: String): Unit = {

    val zgen = new ch.ethz.acl.intrinsics.IntrinsicsGenerator {
      override val rootPath = new File(".").getAbsolutePath
      override val srcPath = rootPath + "/" + relativePath + "/"
      override val resourcePath = rootPath + "/resources/com.intel.intrinsics/"
      override val xmlFile = resourcePath + "data-3.3.16.xml"

      val srcDirectory = new File(srcPath)
      if (!srcDirectory.exists()) srcDirectory.mkdirs()

      val statDirectory = new File(rootPath + "/stats/")
      if (!statDirectory.exists()) statDirectory.mkdirs()
    }

    zgen.generate()
  }

}
