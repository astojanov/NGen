package ch.ethz.acl.commons.system

import java.io.{File, PrintWriter, StringWriter}
import java.nio.file.Paths
import java.util

import ch.ethz.acl.commons.compiler.ArchType.ArchType
import ch.ethz.acl.commons.compiler.ISA.ISA
import ch.ethz.acl.commons.compiler._
import ch.ethz.acl.commons.compiler.OSType.OSType
import ch.ethz.acl.commons.util.{Debugging, Utilities}
import org.apache.commons.io.FileUtils
import org.hyperic.sigar.{Sigar, SigarLoader}
import model.CPUID
import org.hyperic.jni.ArchName

object LocalSystem extends Debugging {

  private var systemDetected = false

  private var systemOS: OSType         = OSType.UNKNOWN
  private var jdkHome : Option[String] = None

  private var cpuFreq   : Int    = -1
  private var cpuModel  : String = "UNKNOWN"
  private var cpuVendor : String = "UNKNOWN"
  private var cpuBrand  : String = "UNKNOWN"
  private var cpuArch   : String = "UNKNOWN"

  private var cpuFamily : Int = -1
  private var cpuModelN : Int = -1
  private var cpuISAs         = List.empty[ISA]
  private var cpuMicroArch    = UArch.UNKNOWN

  private var gccCompiler  : Option[String]   = None
  private var gccVersion   : Option[String]   = None
  private var iccCompiler  : Option[String]   = None
  private var iccVersion   : Option[String]   = None
  private var llvmCompiler : Option[String]   = None
  private var llvmVersion  : Option[String]   = None
  private var javacCompiler: Option[String]   = None
  private var javacVersion : Option[String]   = None
  private var jvmArch      : Option[ArchType] = None

  private var processEnv : Option[Map[String, String]] = None

  private def detectGCC (): Unit = {
    val gcc = if (systemOS == OSType.WINDOWS) {
      Utilities.findExec("gcc.exe")
    } else {
      Utilities.findExec("gcc")
    }
    if (gcc != null) {
      val (_, v1, v2) = (new Executable).execute(gcc.getAbsolutePath + " -v")
      gccCompiler = Some(gcc.getAbsolutePath)
      gccVersion = Some((v1 ::: v2).mkString(", "))
    }
  }

  private def detectLLVM (): Unit = {
    val llvm = if (systemOS == OSType.WINDOWS) {
      Utilities.findExec("clang.exe")
    } else {
      Utilities.findExec("clang")
    }
    if (llvm != null) {
      val (_, v1, v2) = (new Executable).execute(llvm.getAbsolutePath + " -v")
      llvmCompiler = Some(llvm.getAbsolutePath)
      llvmVersion = Some((v1 ::: v2).mkString(", "))
    }
  }

  private def detectICC (): Unit = {
    val icc = if (systemOS == OSType.WINDOWS) {
      Utilities.findExec("icl.exe")
    } else {
      Utilities.findExec("icc")
    }
    if (icc != null) {
      val (_, v1, v2) = (new Executable).execute(icc.getAbsolutePath + " -v")
      iccCompiler = Some(icc.getAbsolutePath)
      iccVersion = Some((v1 ::: v2).mkString(", "))
    }
  }


  private def detectJavaC (): Unit = {
    val javaHome = Paths.get(System.getProperty("java.home")).getParent.resolve("bin").toString
    val javac = if (systemOS == OSType.WINDOWS) {
      Utilities.findExec("javac.exe", javaHome)
    } else {
      Utilities.findExec("javac", javaHome)
    }
    if (javac != null) {
      val (_, v1, v2) = (new Executable).execute(javac.getAbsolutePath + " -version")
      javacCompiler = Some(javac.getAbsolutePath)
      javacVersion = Some((v1 ::: v2).mkString(", "))
      jdkHome = Some(new File(System.getProperty("java.home") + File.separatorChar + ".." + File.separatorChar).getAbsolutePath)
    }
  }

  private def detectOS () : Unit = {
    systemOS = Utilities.getOS()
  }

  def checkSigar (): Unit = {

    val libraryPath  = System.getProperty("java.library.path")
    val sigarLibrary = getClass.getResource(s"/org.hyperic.sigar/") // .toExternalForm
    val tmpFile = FileUtils.getTempDirectory
    val sigarPath = tmpFile.getAbsolutePath + File.separator + "sigar" + System.currentTimeMillis
    val sigarFile = new File(sigarPath)
    FileUtils.copyDirectory(new File(sigarLibrary.toURI), sigarFile)

    val path = libraryPath + File.pathSeparator + sigarPath
    System.setProperty("java.library.path", path)

    {
      val sigar = new Sigar()
      val cpuInto = sigar.getCpuInfoList()(0)

      cpuModel   = cpuInto.getModel
      cpuVendor  = cpuInto.getVendor
      cpuFreq    = cpuInto.getMhz * 1000

      if (ArchName.is64()) {
        jvmArch = Some(ArchType.x86_64)
      }

//      val env = sigar.getProcEnv("$$")
//      import scala.collection.JavaConverters._
//      env.asScala map println
      sigar.close()
    }

    System.setProperty("java.library.path", libraryPath)
    System.gc()
  }

  def detectCPU (): Unit = {

    val cpuidLibrary = getClass.getResource(s"/com.intel.intrinsics/") // .toExternalForm
    val tmpFile = FileUtils.getTempDirectory
    val cpuidPath = tmpFile.getAbsolutePath + File.separator + "cpuid" + System.currentTimeMillis
    val cpuidFile = new File(cpuidPath)
    FileUtils.copyDirectory(new File(cpuidLibrary.toURI), cpuidFile)

    {
      CPUID.load(cpuidPath)
      cpuBrand = CPUID.getBrandName

      cpuFamily    = CPUID.getCPUFamily
      cpuModelN    = CPUID.getCPUModel
      cpuMicroArch = MicroArchMapping.getIntelMircoArch(cpuFamily, cpuModelN)

      // Check for existence of FMA
      if (CPUID.isSupported(CPUID.FEATURE_MMX)) cpuISAs = ISA.MMX :: cpuISAs

      // Check for existence of SSE
      if (CPUID.isSupported(CPUID.FEATURE_SSE))    cpuISAs = ISA.SSE   :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_SSE2))   cpuISAs = ISA.SSE2  :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_SSE3))   cpuISAs = ISA.SSE3  :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_SSSE3))  cpuISAs = ISA.SSSE3 :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_SSE4_1)) cpuISAs = ISA.SSE41 :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_SSE4_2)) cpuISAs = ISA.SSE42 :: cpuISAs

      // Check for existence of AVX
      if (CPUID.isSupported(CPUID.FEATURE_AVX))    cpuISAs = ISA.AVX   :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_AVX2))   cpuISAs = ISA.AVX2  :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_AVX512)) cpuISAs = ISA.AVX512  :: cpuISAs

      // Check existence of
      if (CPUID.isSupported(CPUID.FEATURE_RDRAND)) cpuISAs = ISA.RDRAND :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_RDSEED)) cpuISAs = ISA.RDSEED :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_RDTSCP)) cpuISAs = ISA.RDTSCP :: cpuISAs

      // Check for the rest of the smaller ISAs
      if (CPUID.isSupported(CPUID.FEATURE_ADX))       cpuISAs = ISA.ADX       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_AES))       cpuISAs = ISA.AES       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_BMI1))      cpuISAs = ISA.BMI1      :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_BMI2))      cpuISAs = ISA.BMI2      :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_FMA))       cpuISAs = ISA.FMA       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_FP16C))     cpuISAs = ISA.FP16C     :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_FSGSBASE))  cpuISAs = ISA.FSGSBASE  :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_FXSR))      cpuISAs = ISA.FXSR      :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_INVPCID))   cpuISAs = ISA.INVPCID   :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_LZCNT))     cpuISAs = ISA.LZCNT     :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_MPX))       cpuISAs = ISA.MPX       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_PCLMULQDQ)) cpuISAs = ISA.PCLMULQDQ :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_POPCNT))    cpuISAs = ISA.POPCNT    :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_RTM))       cpuISAs = ISA.RTM       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_SHA))       cpuISAs = ISA.SHA       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_TSC))       cpuISAs = ISA.TSC       :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_VPMADD52))  cpuISAs = ISA.VPMADD52  :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_XSAVE))     cpuISAs = ISA.XSAVE     :: cpuISAs
      if (CPUID.isSupported(CPUID.FEATURE_XSAVEOPT))  cpuISAs = ISA.XSAVEOPT  :: cpuISAs

      cpuISAs = cpuISAs.reverse

    }
    System.gc()

  }

  def detect (): Unit = if (!systemDetected)
  {
    detectOS()
    detectJavaC()
    checkSigar()
    detectCPU()
    detectICC()
    detectGCC()
    detectLLVM()

    val runtime = Runtime.getRuntime()

    val report =
      s"""
         |
         |===================================================
         |Running system detection
         |---------------------------------------------------
         |CPU Model   : ${cpuModel}
         |CPU Vendor  : ${cpuVendor}
         |CPU Freq    : ${cpuFreq}
         |CPU Brand   : ${cpuBrand}
         |CPU Arch    : ${cpuArch}
         |
         |CPU Family  : ${cpuFamily}
         |CPU Model   : ${cpuModelN}
         |CPU uArch   : ${cpuMicroArch}
         |CPU ISAs    : ${cpuISAs.mkString(", ")}
         |
         |Compilers:
         |---------------------------------------------------
         |ICC         : ${iccCompiler.getOrElse("UNKOWN")}  \t ${iccVersion.getOrElse("")}
         |GCC         : ${gccCompiler.getOrElse("UNKOWN")}  \t ${gccVersion.getOrElse("")}
         |LLCM        : ${llvmCompiler.getOrElse("UNKOWN")} \t ${llvmVersion.getOrElse("")}
         |
         |JVM
         |---------------------------------------------------
         |JVM Arch    : ${jvmArch.getOrElse("UNKOWN")}
         |JRE Home    : ${System.getProperty("java.home")}
         |JDK Home    : ${jdkHome}
         |VM Name     : ${System.getProperty("java.vm.name")} ${System.getProperty("java.vm.vendor")}
         |VM Version  : ${System.getProperty("java.vm.version")}
         |JRE Type    : ${if (javacCompiler.isEmpty) "JRE" else "JDK"}
         |JRE Vendor  : ${System.getProperty("java.vendor")} ${System.getProperty("java.vendor.url")}
         |JRE Version : ${System.getProperty("java.version")}
         |
         |Free  mem   : ${Utilities.humanReadableByteCount(runtime.freeMemory())}
         |Max   mem   : ${Utilities.humanReadableByteCount(runtime.maxMemory())}
         |Total mem   : ${Utilities.humanReadableByteCount(runtime.totalMemory())}
         |===================================================
       """.stripMargin

    printDebug3(report)

    systemDetected = true
  }

  def getOS () = {
    detect()
    systemOS
  }

  def getCompiler (): Option[AbstractCompiler] = {
    detect()
    iccCompiler match {
      case Some(_) => Some(new ICC(iccCompiler, getOS()))
      case _ => gccCompiler match {
        case Some(_) => Some(new GCC(gccCompiler, getOS(), getUArch()))
        case _ => llvmCompiler match {
          case Some(_) => Some(new LLVM(llvmCompiler, getOS(), getUArch()))
          case _ => None
        }
      }
    }
  }

  def getFreq(): Int = {
    detect()
    cpuFreq
  }

  def getJDKHome: Option[String] = {
    detect()
    jdkHome
  }

  def getUArch (): UArch.UArch = {
    detect()
    cpuMicroArch
  }

  def getISAs () : List[ISA] = {
    detect()
    cpuISAs
  }

  def getJVMArch () : Option[ArchType] = {
    detect()
    jvmArch
  }

}