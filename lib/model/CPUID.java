package model;

import java.io.File;
import java.util.Locale;

public class CPUID {

    private static boolean loaded = false;
    static int cpuid_0x1_edx;
    static int cpuid_0x1_ecx;
    static int cpuid_0x7_ebx;
    static int cpuid_0x80000001_ecx;
    static int cpuid_0x80000001_edx;

    public static final String FEATURE_ADX = "ADX";
    public static final String FEATURE_AES = "AES";
    public static final String FEATURE_AVX = "AVX";
    public static final String FEATURE_AVX2 = "AVX2";
    public static final String FEATURE_AVX512 = "AVX-512";
    public static final String FEATURE_AVX512_CDI = "AVX-512.CDI";
    public static final String FEATURE_AVX512_ERI = "AVX-512.ERI";
    public static final String FEATURE_AVX512_PFI = "AVX-512.PFI";
    public static final String FEATURE_BMI1 = "BMI1";
    public static final String FEATURE_BMI2 = "BMI2";
    public static final String FEATURE_FMA = "FMA";
    public static final String FEATURE_FP16C = "FP16C";
    public static final String FEATURE_FSGSBASE = "FSGSBASE";
    public static final String FEATURE_FXSR = "FXSR";
    public static final String FEATURE_INVPCID = "INVPCID";
    public static final String FEATURE_LZCNT = "LZCNT";
    public static final String FEATURE_MMX = "MMX";
    public static final String FEATURE_MPX = "MPX";
    public static final String FEATURE_PCLMULQDQ = "PCLMULQDQ";
    public static final String FEATURE_POPCNT = "POPCNT";
    public static final String FEATURE_RDRAND = "RDRAND";
    public static final String FEATURE_RDSEED = "RDSEED";
    public static final String FEATURE_RDTSCP = "RDTSCP";
    public static final String FEATURE_RTM = "RTM";
    public static final String FEATURE_SHA = "SHA";
    public static final String FEATURE_SSE = "SSE";
    public static final String FEATURE_SSE2 = "SSE2";
    public static final String FEATURE_SSE3 = "SSE3";
    public static final String FEATURE_SSSE3 = "SSSE3";
    public static final String FEATURE_SSE4_1 = "SSE4.1";
    public static final String FEATURE_SSE4_2 = "SSE4.2";
    public static final String FEATURE_TSC = "TSC";
    public static final String FEATURE_VPMADD52 = "VPMADD52";
    public static final String FEATURE_XSAVE = "XSAVE";
    public static final String FEATURE_XSAVEOPT = "XSAVEOPT";

    private static void loadLibraries (String prefix) {
        String os = getOs();
        String arch = getArch(os);
        String lib = "cpuid_" + os;
        String libname = prefix;

        if (os.equals("windows")) {
            libname += File.separator + lib + "_" + arch + ".dll";
        } else if (os.equals("linux")) {
            libname += File.separator + "lib" + lib + "_" + arch + ".so";
        } else {
            libname += File.separator + "lib" + lib + ".jnilib";
        }

        try {
            System.load(libname);
            loaded = true;
        } catch (UnsatisfiedLinkError var4) {
            loaded = false;
            System.out.println(var4.getMessage());
        }
    }

    public CPUID() {
    }

    private static String getOs() {
        String osname = System.getProperty("os.name");
        String os;
        if(osname == null) {
            os = "linux";
        } else {
            osname = osname.toLowerCase(Locale.US);
            if(!osname.startsWith("mac") && !osname.startsWith("darwin") && !osname.startsWith("os x")) {
                if(osname.startsWith("windows")) {
                    os = "windows";
                } else {
                    os = "linux";
                }
            } else {
                os = "mac";
            }
        }

        return os;
    }

    private static String getArch(String os) {
        String arch = System.getProperty("os.arch");
        if(arch == null) {
            arch = "i386";
        } else {
            arch = arch.toLowerCase(Locale.US);
            if("amd64".equals(arch)) {
                arch = "x86_64";
            } else if("x86".equals(arch)) {
                arch = "i386";
            }
        }

        return arch;
    }

    static native int[] cpuid(int var0, int var1);


    private static String int32ToString(int n)
    {
        String result = "";

        result += (char)(n & 0xFF);
        result += (char)((n >> 8) & 0xFF);
        result += (char)((n >> 16) & 0xFF);
        result += (char)((n >> 24) & 0xFF);

        return result;
    }

    public static String getBrandName ()
    {
        String brandName = "";
        for (int i = 0; i <= 2; i += 1)
        {
            int[] info = cpuid(i + 0x80000002, 0);
            brandName += int32ToString(info[0]);
            brandName += int32ToString(info[1]);
            brandName += int32ToString(info[2]);
            brandName += int32ToString(info[3]);
        }
        return brandName.trim();
    }


    public static boolean load(String prefix) {
        loadLibraries(prefix);
        if (loaded) {
            int[] cpuid_0x1 = cpuid(1, 0);
            int[] cpuid_0x7 = cpuid(7, 0);
            int[] cpuid_0x80000001 = cpuid(-2147483647, 0);
            cpuid_0x1_edx = cpuid_0x1[3];
            cpuid_0x1_ecx = cpuid_0x1[2];
            cpuid_0x7_ebx = cpuid_0x7[1];
            cpuid_0x80000001_ecx = cpuid_0x80000001[2];
            cpuid_0x80000001_ecx = cpuid_0x80000001[3];
            return true;
        } else {
            return false;
        }
    }

    public static boolean isSupported(String feature) {
        boolean supported = false;
        if(feature.equals("ADX")) {
            supported = (cpuid_0x7_ebx & 524288) != 0;
        } else if(feature.equals("AES")) {
            supported = (cpuid_0x1_ecx & 33554432) != 0;
        } else if(feature.equals("AVX")) {
            supported = (cpuid_0x1_ecx & 268435456) != 0;
        } else if(feature.equals("AVX2")) {
            supported = (cpuid_0x7_ebx & 32) != 0;
        } else if(feature.equals("AVX-512")) {
            supported = (cpuid_0x7_ebx & 65536) != 0;
        } else if(feature.equals("AVX-512.CDI")) {
            supported = (cpuid_0x7_ebx & 268435456) != 0;
        } else if(feature.equals("AVX-512.ERI")) {
            supported = (cpuid_0x7_ebx & 134217728) != 0;
        } else if(feature.equals("AVX-512.PFI")) {
            supported = (cpuid_0x7_ebx & 67108864) != 0;
        } else if(feature.equals("BMI1")) {
            supported = (cpuid_0x7_ebx & 8) != 0;
        } else if(feature.equals("BMI2")) {
            supported = (cpuid_0x7_ebx & 256) != 0;
        } else if(feature.equals("FMA")) {
            supported = (cpuid_0x1_ecx & 4096) != 0;
        } else if(feature.equals("FP16C")) {
            supported = (cpuid_0x1_ecx & 536870912) != 0;
        } else if(feature.equals("FSGSBASE")) {
            supported = (cpuid_0x7_ebx & 1) != 0;
        } else if(feature.equals("FXSR")) {
            supported = (cpuid_0x1_edx & 16777216) != 0;
        } else if(feature.equals("INVPCID")) {
            supported = (cpuid_0x7_ebx & 1024) != 0;
        } else if(feature.equals("LZCNT")) {
            supported = (cpuid_0x80000001_ecx & 32) != 0;
        } else if(feature.equals("MMX")) {
            supported = (cpuid_0x1_edx & 8388608) != 0;
        } else if(feature.equals("MPX")) {
            supported = (cpuid_0x7_ebx & 16384) != 0;
        } else if(feature.equals("PCLMULQDQ")) {
            supported = (cpuid_0x1_ecx & 2) != 0;
        } else if(feature.equals("POPCNT")) {
            supported = (cpuid_0x1_ecx & 8388608) != 0;
        } else if(feature.equals("RDRAND")) {
            supported = (cpuid_0x1_ecx & 1073741824) != 0;
        } else if(feature.equals("RDSEED")) {
            supported = (cpuid_0x7_ebx & 262144) != 0;
        } else if(feature.equals("RDTSCP")) {
            supported = (cpuid_0x80000001_edx & 134217728) != 0;
        } else if(feature.equals("RTM")) {
            supported = (cpuid_0x7_ebx & 2048) != 0;
        } else if(feature.equals("SHA")) {
            supported = (cpuid_0x7_ebx & 536870912) != 0;
        } else if(feature.equals("SSE")) {
            supported = (cpuid_0x1_edx & 33554432) != 0;
        } else if(feature.equals("SSE2")) {
            supported = (cpuid_0x1_edx & 67108864) != 0;
        } else if(feature.equals("SSE3")) {
            supported = (cpuid_0x1_ecx & 1) != 0;
        } else if(feature.equals("SSSE3")) {
            supported = (cpuid_0x1_ecx & 512) != 0;
        } else if(feature.equals("SSE4.1")) {
            supported = (cpuid_0x1_ecx & 524288) != 0;
        } else if(feature.equals("SSE4.2")) {
            supported = (cpuid_0x1_ecx & 1048576) != 0;
        } else if(feature.equals("TSC")) {
            supported = (cpuid_0x1_edx & 16) != 0;
        } else if(feature.equals("VPMADD52")) {
            supported = (cpuid_0x7_ebx & 2097152) != 0;
        } else if(feature.equals("XSAVE")) {
            supported = (cpuid_0x1_ecx & 67108864) != 0;
        } else if(feature.equals("XSAVEOPT")) {
            supported = (cpuid_0x1_ecx & 134217728) != 0;
        } else {
            supported = true;
        }
        return supported;
    }

    public static int getCPUFamily()
    {
        int[] info = cpuid(1, 0);
        int lo = (info[0] >>  8) & 0xF;
        int hi = (info[0] >> 20) & 0xFF;
        return (hi << 4) | lo;
    }

    public static int getCPUModel()
    {
        int[] info = cpuid(1, 0);
        int lo = (info[0] >>  4) & 0xF;
        int hi = (info[0] >> 16) & 0xF;

        return (hi << 4) | lo;
    }

    public static int getCPUExtendedModel()
    {
        int[] info = cpuid(1, 0);
        return (info[0] >> 16) & 0xf;
    }

    public static int getCPUType()
    {
        int[] info = cpuid(1, 0);
        return (info[0] >> 12) & 0xf;
    }

    public static int getCPUExtendedFamily()
    {
        int[] info = cpuid(1, 0);
        return (info[0] >> 20) & 0xff;
    }

    public static int getCPUStepping()
    {
        int[] info = cpuid(1, 0);
        return info[0] & 0xf;
    }

//    static int getEDXCPUFlags()
//    {
//        int[] info = cpuid(1, 0);
//        return info[3];
//    }
//
//    static int getECXCPUFlags()
//    {
//        CPUIDResult c = doCPUID(1);
//        return c.ECX;
//    }
//
//    static int getExtendedEBXCPUFlags()
//    {
//        CPUIDResult c = doCPUID(0x80000001);
//        return c.EBX;
//    }
//
//    static int getExtendedECXCPUFlags()
//    {
//        CPUIDResult c = doCPUID(0x80000001);
//        return c.ECX;
//    }
//
//    /** @since 0.8.7 */
//    static int getExtendedEDXCPUFlags()
//    {
//        CPUIDResult c = doCPUID(0x80000001);
//        return c.EDX;
//    }

}
