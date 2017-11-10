/**
  *      ___    ______ __       ______ ____   __  ___ __  ___ ____   _   __ _____
  *     /   |  / ____// /      / ____// __ \ /  |/  //  |/  // __ \ / | / // ___/
  *    / /| | / /    / /      / /    / / / // /|_/ // /|_/ // / / //  |/ / \__ \
  *   / ___ |/ /___ / /___   / /___ / /_/ // /  / // /  / // /_/ // /|  / ___/ /
  *  /_/  |_|\____//_____/   \____/ \____//_/  /_//_/  /_/ \____//_/ |_/ /____/
  *
  *  Advanced Computing Laboratory
  *  Department of Computer Science
  *  ETH Zurich, Switzerland
  *
  *  Copyright (C) 2017 Alen Stojanov (astojanov@inf.ethz.ch)
  *
  *  This program is free software: you can redistribute it and/or modify
  *  it under the terms of the GNU General Public License as published by
  *  the Free Software Foundation, either version 3 of the License, or
  *  (at your option) any later version.
  *
  *  This program is distributed in the hope that it will be useful,
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  *  GNU General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program. If not, see http://www.gnu.org/licenses/.
  */

package ch.ethz.acl.commons.compiler

import ch.ethz.acl.commons.compiler.ISA.{ISA, _}

object UArch extends Enumeration {

  type UArch = Value

  val UNKNOWN = Value

  // ================================================================================================================
  // Intel Specific micro architectures
  // ================================================================================================================

  // Original Intel i386 CPU.
  val i386 = Value

  // Intel i486 CPU. (No scheduling is implemented for this chip.)
  val i486 = Value

  // Intel Pentium CPU with no MMX support.
  val i586 = Value
  val Pentium = Value

  // Intel Lakemont MCU, based on Intel Pentium CPU.
  val Lakemont = Value

  // Intel Pentium MMX CPU, based on Pentium core with MMX instruction set support.
  val Pentium_MMX = Value

  // Intel Pentium Pro CPU.
  val PentiumPro = Value

  // When used with -march, the Pentium Pro instruction set is used, so the code runs on
  // all i686 family chips. When used with -mtune, it has the same meaning as ‘generic’.
  val i686 = Value

  // Intel Pentium II CPU, based on Pentium Pro core with MMX instruction set support.
  val Pentium2 = Value

  // Intel Pentium III CPU, based on Pentium Pro core with MMX and SSE instruction set support.
  val Pentium3 = Value
  val Pentium3m = Value

  // Intel Pentium M; low-power version of Intel Pentium III CPU with MMX, SSE and SSE2
  // instruction set support. Used by Centrino notebooks.
  val Pentium_M = Value

  // Intel Pentium 4 CPU with MMX, SSE and SSE2 instruction set support.
  val Pentium4 = Value
  val Pentium4m = Value

  // Improved version of Intel Pentium 4 CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
  val Prescott = Value

  // Improved version of Intel Pentium 4 CPU with 64-bit extensions, MMX, SSE, SSE2 and SSE3
  // instruction set support.
  val Nocona = Value

  // Intel Core 2 CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set support.
  val Core2 = Value

  val CoreI7 = Value

  val CoreI7_AVX = Value

  val Core_AVX_I = Value

  val Core_AVX2 = Value

  val Atom = Value

  // Intel Nehalem CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2 and
  // POPCNT instruction set support.
  val Nehalem = Value

  // Intel Westmere CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
  // POPCNT, AES and PCLMUL instruction set support.
  val Westmere = Value

  // Intel Sandy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
  // POPCNT, AVX, AES and PCLMUL instruction set support.
  val SandyBridge = Value

  // Intel Ivy Bridge CPU with 64-bit extensions, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
  // POPCNT, AVX, AES, PCLMUL, FSGSBASE, RDRND and F16C instruction set support.
  val IvyBridge = Value

  // Intel Haswell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
  // POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2 and F16C instruction set support.
  val Haswell = Value

  // Intel Broadwell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
  // POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX and PREFETCHW
  // instruction set support.
  val Broadwell = Value

  // Intel Skylake CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2,
  // POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED, ADCX, PREFETCHW,
  // CLFLUSHOPT, XSAVEC and XSAVES instruction set support.
  val Skylake = Value

  // TODO: Double check for proper flags in GCC
  val Kabylake = Value

  // Intel Bonnell CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3 and SSSE3 instruction
  // set support.
  val Bonnell = Value

  // Intel Silvermont CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1,
  // SSE4.2, POPCNT, AES, PCLMUL and RDRND instruction set support.
  val Silvermont = Value

  // Intel Knight’s Landing CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3,
  // SSE4.1, SSE4.2, POPCNT, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C,
  // RDSEED, ADCX, PREFETCHW, AVX512F, AVX512PF, AVX512ER and AVX512CD instruction set support.
  val KNL = Value

  // Intel Skylake Server CPU with 64-bit extensions, MOVBE, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1,
  // SSE4.2, POPCNT, PKU, AVX, AVX2, AES, PCLMUL, FSGSBASE, RDRND, FMA, BMI, BMI2, F16C, RDSEED,
  // ADCX, PREFETCHW, CLFLUSHOPT, XSAVEC, XSAVES, AVX512F, AVX512VL, AVX512BW, AVX512DQ and
  // AVX512CD instruction set support.
  val Skylake_AVX512 = Value


  // ================================================================================================================
  // AMD Specific micro architectures
  // ================================================================================================================

  // TODO: Double check for proper flags in GCC
  val K5 = Value

  // AMD K6 CPU with MMX instruction set support.
  val K6 = Value

  // Improved versions of AMD K6 CPU with MMX and 3DNow! instruction set support.
  val K6_2 = Value
  val K6_3 = Value

  // AMD Athlon CPU with MMX, 3dNOW!, enhanced 3DNow! and SSE prefetch instructions support.
  val Athlon = Value
  val Athlon_Tbird = Value

  // Improved AMD Athlon CPU with MMX, 3DNow!, enhanced 3DNow! and full SSE instruction set support.
  val Athlon_4 = Value
  val Athlon_XP = Value
  val Athlon_MP = Value

  // Processors based on the AMD K8 core with x86-64 instruction set support, including the AMD
  // Opteron, Athlon 64, and Athlon 64 FX processors. (This supersets MMX, SSE, SSE2, 3DNow!,
  // enhanced 3DNow! and 64-bit instruction set extensions.)
  val K8 = Value
  val Opteron = Value
  val Athlon64 = Value
  val Athlon_FX = Value

  // Improved versions of AMD K8 cores with SSE3 instruction set support.
  val K8_SSE3 = Value
  val Opteron_SSE3 = Value
  val Athlon64_SSE3 = Value

  // CPUs based on AMD Family 10h cores with x86-64 instruction set support. (This supersets MMX,
  // SSE, SSE2, SSE3, SSE4A, 3DNow!, enhanced 3DNow!, ABM and 64-bit instruction set extensions.)
  val Amdfam10 = Value
  val Barcelona = Value


  // CPUs based on AMD Family 15h cores with x86-64 instruction set support. (This supersets FMA4,
  // AVX, XOP, LWP, AES, PCL_MUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM
  // and 64-bit instruction set extensions.)
  val Bdver1 = Value

  // AMD Family 15h core based CPUs with x86-64 instruction set support. (This supersets BMI,
  // TBM, F16C, FMA, FMA4, AVX, XOP, LWP, AES, PCL_MUL, CX16, MMX, SSE, SSE2, SSE3, SSE4A,
  // SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.)
  val Bdver2 = Value

  // AMD Family 15h core based CPUs with x86-64 instruction set support. (This supersets BMI,
  // TBM, F16C, FMA, FMA4, FSGSBASE, AVX, XOP, LWP, AES, PCL_MUL, CX16, MMX, SSE, SSE2, SSE3,
  // SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.
  val Bdver3 = Value

  // AMD Family 15h core based CPUs with x86-64 instruction set support. (This supersets BMI,
  // BMI2, TBM, F16C, FMA, FMA4, FSGSBASE, AVX, AVX2, XOP, LWP, AES, PCL_MUL, CX16, MOVBE,
  // MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.
  val Bdver4 = Value

  // AMD Family 17h core based CPUs with x86-64 instruction set support. (This supersets BMI,
  // BMI2, F16C, FMA, FSGSBASE, AVX, AVX2, ADCX, RDSEED, MWAITX, SHA, CLZERO, AES, PCL_MUL,
  // CX16, MOVBE, MMX, SSE, SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM, XSAVEC, XSAVES,
  // CLFLUSHOPT, POPCNT, and 64-bit instruction set extensions.
  val Znver1 = Value

  // CPUs based on AMD Family 14h cores with x86-64 instruction set support. (This supersets
  // MMX, SSE, SSE2, SSE3, SSSE3, SSE4A, CX16, ABM and 64-bit instruction set extensions.)
  val Btver1 = Value

  // CPUs based on AMD Family 16h cores with x86-64 instruction set support. This includes
  // MOVBE, F16C, BMI, AVX, PCL_MUL, AES, SSE4.2, SSE4.1, CX16, ABM, SSE4A, SSSE3, SSE3,
  // SSE2, SSE, MMX and 64-bit instruction set extensions.
  val Btver2 = Value

  // AMD Geode embedded processor with MMX and 3DNow! instruction set support.
  val Geode = Value


  // ================================================================================================================
  // IDT Specific micro architectures
  // ================================================================================================================

  // IDT WinChip C6 CPU, dealt in same way as i486 with additional MMX instruction set support.
  val Winchip_C6 = Value

  // IDT WinChip 2 CPU, dealt in same way as i486 with additional MMX and 3DNow! instruction set support.
  val Winchip2 = Value

  // ================================================================================================================
  // VIA Specific micro architectures
  // ================================================================================================================

  // VIA C3 CPU with MMX and 3DNow! instruction set support.
  // (No scheduling is implemented for this chip.)
  val C3 = Value

  // VIA C3-2 (Nehemiah/C5XL) CPU with MMX and SSE instruction set support.
  // (No scheduling is implemented for this chip.)
  val C3_2 = Value

  // VIA C7 (Esther) CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
  // (No scheduling is implemented for this chip.)
  val C7 = Value

  // VIA Eden Samuel 2 CPU with MMX and 3DNow! instruction set support.
  // (No scheduling is implemented for this chip.)
  val Samuel_2 = Value

  // VIA Eden Nehemiah CPU with MMX and SSE instruction set support.
  // (No scheduling is implemented for this chip.)
  val Nehemiah = Value

  // VIA Eden Esther CPU with MMX, SSE, SSE2 and SSE3 instruction set support.
  // (No scheduling is implemented for this chip.)
  val Esther = Value

  // VIA Eden X2 CPU with x86-64, MMX, SSE, SSE2 and SSE3 instruction set support.
  // (No scheduling is implemented for this chip.)
  val Eden_X2 = Value

  // VIA Eden X4 CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX and
  // AVX2 instruction set support. (No scheduling is implemented for this chip.)
  val Eden_X4 = Value

  // Generic VIA Nano CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set
  // support. (No scheduling is implemented for this chip.)
  val Nano = Value

  // VIA Nano 1xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set
  // support. (No scheduling is implemented for this chip.)
  val Nano_1000 = Value

  // VIA Nano 2xxx CPU with x86-64, MMX, SSE, SSE2, SSE3 and SSSE3 instruction set
  // support. (No scheduling is implemented for this chip.)
  val Nano_2000 = Value

  // VIA Nano 3xxx CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1 instruction
  // set support. (No scheduling is implemented for this chip.)
  val Nano_3000 = Value

  // VIA Nano Dual Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1 instruction
  // set support. (No scheduling is implemented for this chip.)
  val Nano_X2 = Value

  // VIA Nano Quad Core CPU with x86-64, MMX, SSE, SSE2, SSE3, SSSE3 and SSE4.1 instruction
  // set support. (No scheduling is implemented for this chip.)
  val Nano_X4 = Value



  // ================================================================================================================
  // ARM Specific micro architectures
  // ================================================================================================================

  val ARMV2 = Value
  val ARMV2A = Value
  val ARMV3 = Value
  val ARMV3M = Value
  val ARMV4 = Value
  val ARMV4T = Value
  val ARMV5 = Value
  val ARMV5T = Value
  val ARMV5E = Value
  val ARMV5TE = Value
  val ARMV6 = Value
  val ARMV6J = Value
  val ARMV6T2 = Value
  val ARMV6Z = Value
  val ARMV6ZK = Value
  val ARMV6_M = Value
  val ARMV7 = Value
  val ARMV7_A = Value
  val ARMV7_R = Value
  val ARMV7_M = Value
  val ARMV7E_M = Value
  val ARMV7VE = Value
  val ARMV8_A = Value
  val ARMV8_A_CRC = Value
  val IWMMXT = Value
  val IWMMXT2 = Value
  val EP9312 = Value


  class UArchValue(d: Value) {

    def convertToString () : String = d match {
      case ARMV8_A_CRC => "armv8-a+crc"
      case _ => d.toString().toLowerCase.replace("_", "-")
    }

    def getISAs () : List[ISA] = d match {
      case Nehalem        => List(MMX, SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, FXSR, POPCNT, TSC)
      // Penryn case Core2          => List(MMX, SSE, SSE2, SSE3, SSSE3, SSE41, FXSR, TSC, XSAVE, XSAVEOPT)
      case SandyBridge    => List(MMX, SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, AVX, AES, FXSR, PCLMULQDQ, POPCNT, TSC, XSAVE, XSAVEOPT)
      case IvyBridge      => List(MMX, SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, AVX, RDRAND, AES, FP16C, FSGSBASE, FXSR, PCLMULQDQ, POPCNT, TSC, XSAVE, XSAVEOPT)
      case Haswell        => List(MMX, SSE, SSE2, SSE3, SSSE3, SSE41, SSE42, AVX, AVX2, RDRAND, AES, BMI1, BMI2, FMA, FP16C, FSGSBASE, FXSR, INVPCID, PCLMULQDQ, POPCNT, TSC, XSAVE, XSAVEOPT)
      case _              => List.empty[ISA]
    }

    def bestISA (): ISA = d match {
      case Atom           => SSSE3
      // Penryn case Core2          => SSE41
      case Nehalem        => SSE42
      case SandyBridge    => AVX
      case IvyBridge      => AVX
      case Haswell        => AVX2
      case Skylake        => AVX2
      case Skylake_AVX512 => AVX512
    }
  }

  implicit def value2MicroArchValue (d: Value): UArchValue = new UArchValue(d)

  def fromString(f: String): UArch = f.trim.toLowerCase match {
    case "armv8-a+crc" => ARMV8_A_CRC
    case fstr => UArch.values.find(x => x.convertToString().equals(fstr)) match {
      case Some(flag) => flag
      case _ => UNKNOWN
    }
  }
}
