// ========================================================
// Project meta information
// ========================================================

name := "NGen"

version := "0.1"

organization := "ch.ethz.acl"

organizationName := "Advanced Computing Laboratory, Department of Computer Science, ETH Zurich"

organizationHomepage := Some(url("https://acl.inf.ethz.ch/"))

description := "NGen represents a CGO artifact, a demonstration for the paper"

// ===============================================================================================
// Set up sources and resources folders for the SBT build
// ===============================================================================================

scalaSource                  in Compile <<= baseDirectory(_ / "src")

scalaSource                  in Test    <<= baseDirectory(_ / "test-src")

unmanagedSourceDirectories   in Compile <+= baseDirectory(_ / "lib" )

resourceDirectory            in Compile <<= baseDirectory(_ / "conf")

resourceDirectory            in Test    <<= baseDirectory(_ / "conf")

unmanagedResourceDirectories in Compile <+= baseDirectory(_ / "resources" )

unmanagedResourceDirectories in Test    <+= baseDirectory(_ / "resources" )


// ===============================================================================================
// = LMS & Scala Virtualized Settings / Dependencies
// ===============================================================================================

scalaOrganization   := "org.scala-lang.virtualized"

scalaVersion        := virtScala

scalacOptions       += "-Yvirtualize"

libraryDependencies += virtLib("org.scala-lang.virtualized" % "scala-library" % virtScala)

libraryDependencies += virtLib("org.scala-lang.virtualized" % "scala-compiler" % virtScala)

libraryDependencies += virtLib("org.scala-lang" % "scala-actors" % virtScala)

libraryDependencies += virtLib("org.scalatest" % "scalatest_2.11" % "2.2.2")

dependencyOverrides += "org.scalatest" %% "scalatest" % "2.2.2"


// ========================================================
// = ch.ethz.acl.commons Dependencies
// ========================================================

libraryDependencies += "javax.mail" % "mail" % "1.4"                          // Send Mail for reports

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"         // For MathUtilities

libraryDependencies += "net.java.dev.jna" % "jna" % "3.4.0"                   // for fftw3 package

libraryDependencies += "com.github.abrarsyed.jastyle" % "jAstyle" % "1.2"     // C Code indentation

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "3.3.0.201403021825-r" // Java Git

libraryDependencies += ("com.nativelibs4java" % "bridj" % "0.6.1").exclude("com.google.android.tools", "dx")

libraryDependencies += virtLib("com.typesafe" % "config" % "1.2.1")           // Read the config files

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"         // Various libraries

libraryDependencies += "org.knowhowlab.osgi" % "sigar" % "1.6.5_01"           // Sigar system inspection

libraryDependencies += "com.github.dwickern" %% "scala-nameof" % "1.0.3" % "provided"


// ========================================================
// = LMS Intrinsics Dependencies
// ========================================================

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "ch.ethz.acl" %% "scala-unsigned" % "0.1-SNAPSHOT"

libraryDependencies += "ch.ethz.acl" %% "lms-intrinsics" % "0.0.5-SNAPSHOT"

// ========================================================
// = NGen Settings and Dependencies
// ========================================================

//
// Make sure that each time the binary is started, the JVM is forked.
// Also, make sure that the tests are executed sequentially.
//
fork := true

parallelExecution in Test := false

//
// Use ScalaMeter as a testing framework
//
testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"





