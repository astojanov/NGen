Artifact description
====================

Submission and reviewing guidelines and methodology:
<http://cTuning.org/ae/submission-20160509.html>

Abstract
--------

To reproduce the results presented in our work, we provide an artifact
that consist of two parts:

-   `lms-intrinsics` a precompiled `jar` library that includes all
    Intel-based SIMD intrinsics functions, implemented as Scala eDSLs in
    LMS.

-   `NGen` runtime implemented in Scala and Java, that enables the use
    of `lms-intrinsics` in the JVM and includes the experiments
    discussed in our work.

The SIMD based eDSLs follow the modular design of the LMS framework and
are implemented as an external LMS library, separated from the JVM
runtime. This allows a stand-alone use of `lms-intrinsics`, enabling LMS
to generate `x86` vectorized code outside the context of the JVM. The
JVM runtime (`NGen`) demonstrates the use of the `lms-intrinsics` by
providing the compiler pipeline to generate, compile, link and execute
the LMS-generated SIMD code and has a strong dependency on this library.

The experiments included in the artifact come in the form of
microbenchmarks. While the most convenient deployment for this artifact
would have been a Docker image through Collective Knowledge, we decided
to eliminate the overhead imposed by the containers and provided a bare
metal deployment that aims at providing as precise results possible for
our tests. To achieve that, we use SBT (Simple Build Tool) to build and
execute our experiments.

Description
-----------

### Check-list (artifact meta information)

-   **Algorithm:** Using SIMD intrinsics in the JVM. Experiments include
    dot-product on quantized arrays, BLAS routines: SAXPY and
    Matrix-Matrix-Multiplication.
-   **Compilation:** `lms-intrinsics` is a precompiled library, compiled
    with Scala 2.11 and is available as a `jar` bundle, accessible
    through Maven. `NGen` requires Scala 2.11 and Java 1.8 for
    compilation. Both `NGen` and `lms-intrinsics` generate `C` code that
    is compiled with `GCC`, `ICC` or `LLVM`.
-   **Transformations:** To make SIMD instructions available in the JVM,
    `NGen` uses LMS as a staging framework. The user writes vectorized
    code as eDSL in Scala and `NGen` stages the code through multiple
    compile phases before execution.
-   **Binary:** `lms-intrinsics` is a `jar` bundle. `NGen` includes
    binaries for SBT v0.13.6, as well as small library for `CPUID`
    inspection and Sigar v1.6.5\_01 (System Information Gatherer And
    Reporter <https://github.com/hyperic/sigar>) binaries. `NGen` has
    various dependencies on precompiled libraries that include BridJ,
    Apache Commons, ScalaMeter, Scala Virtualized, LMS and finally
    `lms-intrinsics`. SBT automatically pulls all dependencies and their
    corresponding versions.
-   **Data set:** Our experiments operate with random data, requiring no
    data set.
-   **Run-time environment:** `lms-intrinsics` can run on any JVM that
    supports LMS and any operating system supported by the same JVM.
    Similarly, `NGen` could work in any JVM that supports LMS,
    reflection and native code invocation, however our focus has been on
    the HotSpot JVM only, supporting Windows, Linux and Mac OS X. Our
    results are most conveniently replicated on a Unix environment.
-   **Hardware:** The `NGen` and `lms-intrinsics` generated code can run
    on any `x86` and `x86-64` architecture that supports at least one
    subset of the Intel intrinsics functions. We recommend a Haswell
    machine for verifying the results presented in the paper to obtain
    comparable results.
-   **Run-time state:** We perform our tests using warm cache scenario,
    warming the code and data cache many times before measurements
    begin. We advise that the replication of our experiments to be done
    with minimal interference of other applications running on the
    system, having technologies for frequency scaling and resource
    sharing disabled.
-   **Output:** `NGen` generates performance profile of each algorithm
    presented in this paper.
-   **Experiment workflow:** We use SBT not only to compile the code,
    but also to run the experiments.
-   **Experiment customization:** Customisation is certainly possible
    and can be easily achieved by implementing any vectorized code as a
    Scala eDSL.
-   **Publicly available:** Yes

### How delivered

The precompiled SIMD eDSLs library, as well as our JVM runtime,
including the supporting experiments are publicly available through
GitHub, on the following links:

-   <https://github.com/ivtoskov/lms-intrinsics>
-   <https://github.com/astojanov/NGen>

Note that `lms-intrinsics` is also available through Maven, and can be
used through SBT directly:

```scala
libraryDependencies += "ch.ethz.acl" %% "lms-intrinsics" % "0.0.3-SNAPSHOT"
```

### Hardware dependencies

`lms-intrinsics` as well as `NGen` are able to generate `C` code that
can run on `x86` and `x86-64` architecture supporting Intel ISAs.
However, the full set of our experiments require at least a Haswell
machine. Namely:

-   SAXPY and MMM algorithms are implemented using `AVX` and `FMA` ISAs,
    and therefore require at least a Haswell enabled process. Broadwell,
    Skylake, Kaby Lake or later would also work.
-   The dot product of the quantized arrays relies on `AVX2`, and `FMA`
    flags, but also uses the hardware random number generator, requiring
    the `RDRAND` ISA, as well `FP16C` to deal with half-precision
    floats.

We recommend disabling Intel Turbo Boost and Hyper-Threading
technologies to avoid the effects of frequency scaling and resource
sharing on the measurements. Note that these technologies can be easily
disabled in the BIOS settings of the machines that have BIOS firmware.
Many Apple-based machines, such as the MacBook or others, do not have a
user accessible BIOS firmware, and could only disable Turbo Boost using
external kernel modules such as Turbo Boost Switcher
(<https://github.com/rugarciap/Turbo-Boost-Switcher>).

### Software dependencies

`lms-intrinsics` is a self-contained precompiled library and all of its
software dependencies are handled automatically through Maven tools such
as SBT. To build and run `NGen`, the following dependencies must be met:

-   `Git` client, used by SBT to resolve dependencies.
-   Java Development Kit (JDK) 1.8 or later.
-   `C` compiler such as `GCC`, `ICC` or `LLVM`.

After installing the dependencies, it is quite important to have the
binary executables available in the `$PATH`. This way the SBT tool will
be able to process all compilation phases as well as to execute the
experiments. Make sure that the following commands work on your
terminal:

```sh
git --version 
gcc --version 
java -version 
javac -version
```

It is also important to ensure that the installed JVM has architecture
that `GCC` can compile to. This is particularly important for Windows
users: 32-bit `MinGW` port of `GCC` will fail to compile code for 64-bit
JVM.

Installation
------------

The artifact can be cloned from the GitHub repository:

git clone https://github.com/astojanov/NGen

The artifact already includes a precompiled version of SBT. Therefore,
to start the SBT console, we run:

```sh
cd ngen

# For Unix users:
./bin/sbt/bin/sbt

# For Windows users
bin\sbt\bin\sbt.bat
```

Once started, we can compile the code using:

```sh
> compile
```

Once invoked, SBT will automatically pull `lms-intrinsics` as well as
all other dependencies and start the compilation.

Experiment workflow
-------------------

Once SBT compiles the code, we can proceed with evaluating our
experiments. We do this through the SBT console. To inspect the testing
machine through `NGen` runtime we use:

```sh
> test-only cgo.TestPlatform
```

The runtime will be able to inspect the CPU, identify available ISAs and
compilers and inspect the current JDK. If the test platform is
successfully identified, we can continue with the experiments.

#### Generating SIMD eDSLs.

The `lms-intrinsics` bundle includes the automatic generator of SIMD
eDSLs, invoked by:

```sh
> test-only cgo.GenerateIntrinsics
```

The Scala eDSLs (coupled with statistics) will be generated in
`Generated_SIMD_Intrinsics` folder.

#### Explicit vectorization in the JVM.

To run the experiments depicted in our work, we use:

```sh
> test-only cgo.TestSaxpy
> test-only cgo.TestMMM
> test-only cgo.TestPrecision
```

In the case of SAXPY algorithm, if the testing machine is not Haswell
based, we provided an architecture independent implementation of SAXPY:

```sh
> test-only cgo.TestMultiSaxpy
```

Each result shows the size of our microbenchmarks, and the obtained
performance in flops/cycle.

Evaluation and expected result
------------------------------

In the evaluation of the experiment workflow, we expect LMS to produce
correct vectorized code using `lms-intrinsics`. Furthermore, we expect
our performance results to depict a consistent behaviour to the results
shown in this work, outperforming the JVM on the microarchitectures that
support our experiments. Finally, we expect the automatic generation of
eDSLs to be easily adjustable to subsequent updates on the Intel
Intrinsics specifications.

Experiment customization
------------------------

There are many opportunities for customization. We can use `NGen` to
easily develop vectorized code, and we can use ScalaMeter to adjust the
current benchmarks.

#### Developing SIMD code.

`NSaxpy.scala` class, available in `src/ch/ethz/acl/ngen/saxpy/`,
provides detailed guidelines for the usage of SIMD in Scala. Following
the comments in the file, as well as the structural flow of the program,
one can easily modify the skeleton to perform other type of vectorized
computations.

#### Customizing Benchmarks.

Each performance experiment, uses ScalaMeter and is implemented as a
Scala class. The Matrix-Matrix-Multiplication includes `BenchMMM.scala`
located in `src/ch/ethz/acl/ngen/mmm/`. The implementaton allows changes
to various aspects of the benchmarks, including the size and the values
of the input data, warm up times, different JVM invocations, etc.
