package ch.ethz.acl.commons.system

import ch.ethz.acl.commons.compiler.UArch
import ch.ethz.acl.commons.compiler.UArch._


object MicroArchMapping {

  def getAMDMircoArch(family: Int, model: Int): UArch = {
    family match
    {
      case 0x05 => model match
      {
        case 0x00  |
             0x01  |
             0x02 =>
          UArch.K5
        case 0x06  |
             0x07  |
             0x08  |
             0x0D =>
          UArch.K6
        case 0x0A =>
          UArch.Geode
      }
    }
  }

  def getIntelMircoArch(family: Int, model: Int): UArch =
  {
    family match {
      case 0x05 => model match
      {
        case 0x01  | // Pentium (60, 66)
             0x02  | // Pentium (75, 90, 100, 120, 133, 150, 166, 200)
             0x03  | // Pentium OverDrive for Intel486-based systems
             0x04 => // Pentium MMX
          UArch.Pentium  // TODO: DOUBLE-CHECK !!!

        case 0x09 =>
          UArch.Lakemont // TODO: DOUBLE-CHECK !!!
      }
      case 0x06 => model match
      {
        case 0x01 =>   // Pentium Pro
          UArch.PentiumPro

        case 0x03  | // Pentium II (Klamath) and Pentium II Overdrive
             0x05  | // Pentium II (Deschutes, Tonga), Pentium II Celeron (Covington), Pentium II Xeon (Drake)
             0x06 => // Pentium II (Dixon), Pentium II Celeron (Mendocino)
          UArch.Pentium2 // TODO: DOUBLE-CHECK !!!

        case 0x07  | // Pentium III (Katmai), Pentium III Xeon (Tanner)
             0x08  | // Pentium III (Coppermine), Pentium II Celeron (Coppermine-128), Pentium III Xeon (Cascades)
             0x0A  | // Pentium III Xeon (Cascades-2MB)
             0x0B => // Pentium III (Tualatin), Pentium III Celeron (Tualatin-256)
          UArch.Pentium3 // TODO: DOUBLE-CHECK !!!

        case 0x09  | // Pentium M (Banias), Pentium M Celeron (Banias-0, Banias-512)
             0x0D  | // Pentium M (Dothan), Pentium M Celeron (Dothan-512, Dothan-1024)
             0x15 => // Intel 80579 (Tolapai)
          UArch.Pentium_M // TODO: DOUBLE-CHECK !!!

        case 0x0E => // Core Solo/Duo (Yonah), Pentium Dual-Core T2xxx (Yonah), Celeron M (Yonah-512, Yonah-1024), Dual-Core Xeon (Sossaman)
          UArch.Pentium3m // TODO: DOUBLE-CHECK !!! !!!

        case 0x0F  | // Core 2 Duo (Conroe, Conroe-2M, Merom), Core 2 Quad (Tigerton), Xeon (Woodcrest, Clovertown, Kentsfield)
             0x16 => // Celeron (Conroe-L, Merom-L), Core 2 Duo (Merom)
          UArch.Core2 // TODO: DOUBLE-CHECK !!! !!!

        case 0x17  | // Core 2 Duo (Penryn-3M), Core 2 Quad (Yorkfield), Core 2 Extreme (Yorkfield), Xeon (Harpertown), Pentium Dual-Core (Penryn)
             0x1D => // Xeon (Dunnington)
          UArch.Core2 // TODO: Those are penryn CPUs

        case 0x1A  | // Core iX (Bloomfield), Xeon (Gainestown)
             0x1E  | // Core iX (Lynnfield, Clarksfield)
             0x1F  | // Core iX (Havendale)
             0x2E  | // Xeon (Beckton)
             0x25  | // Core iX (Clarkdale)
             0x2C  | // Core iX (Gulftown), Xeon (Gulftown)
             0x2F => // Xeon (Eagleton)
          UArch.Nehalem

        case 0x2A  | // Core iX (Sandy Bridge)
             0x2D => // Core iX (Sandy Bridge-E), Xeon (Sandy Bridge EP/EX)
          UArch.SandyBridge

        case 0x3A  | // Core iX (Ivy Bridge)
             0x3E => // Ivy Bridge-E
          UArch.IvyBridge

        case 0x3C  |
             0x3F  | // Haswell-E
             0x45  | // Haswell ULT
             0x46 => // Haswell with eDRAM
          UArch.Haswell

        case 0x3D  | // Broadwell-U
             0x47  | // Broadwell-H
             0x4F  | // Broadwell-E
             0x56 => // Broadwell-DE
          UArch.Broadwell

        case 0x4E  | // Skylake-U/Y
             0x55  | // Skylake Server (SKX)
             0x5E => // Skylake-H/S
          UArch.Skylake

        case 0x8E  | // Kaby Lake U/Y
             0x9E => // Kaby Lake H/S
          UArch.Kabylake

        case 0x1C  | // Diamondville, Silverthorne, Pineview
             0x26 => // Tunnel Creek
          UArch.Bonnell

//        case 0x27  | // Medfield
//             0x35  | // Cloverview
//             0x36 => // Cedarview, Centerton
//          MicroArchType.Saltwell


        case 0x36 =>
          UArch.Atom

        case 0x37  |
             0x4A  |
             0x4D  |
             0x5A => // Moorefield
          UArch.Silvermont

//        case 0x4C  | // Braswell
//             0x5D => // Denverton
//          MicroArchType.Airmont
        case 0x57 =>
          UArch.KNL

//        case 0x85 =>
//          MicroArchType.KNL_MILL

      }

      case 0x0F => model match
      {
        case 0x00  | // Pentium 4 Xeon (Foster)
             0x01  | // Pentium 4 Celeron (Willamette-128), Pentium 4 Xeon (Foster, Foster MP)
             0x02 => // Pentium 4 (Northwood), Pentium 4 EE (Gallatin), Pentium 4 Celeron (Northwood-128, Northwood-256), Pentium 4 Xeon (Gallatin DP, Prestonia)
          UArch.Pentium4
        case 0x03  | // Pentium 4 (Prescott), Pentium 4 Xeon (Nocona)
             0x04  | // Pentium 4 (Prescott-2M), Pentium 4 EE (Prescott-2M), Pentium D (Smithfield), Celeron D (Prescott-256), Pentium 4 Xeon (Cranford, Irwindale, Paxville)
             0x06 => // Pentium 4 (Cedar Mill), Pentium D EE (Presler), Celeron D (Cedar Mill), Pentium 4 Xeon (Dempsey, Tulsa)
          UArch.Prescott
      }
    }
  }
}