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

import java.io._

import ch.ethz.acl.commons.system.LocalSystem
import ch.ethz.acl.passera.unsigned.{UByte, UInt, ULong, UShort}
import ch.ethz.acl.commons.util.Debugging
import org.bridj._

import scala.collection.mutable.HashMap

object CompileVM extends Debugging {

  protected var funcToLib = new HashMap[String, (String, NativeLibrary, DynamicFunction[Any])]()

  /**
   * compileFiles compiles each file in the list specified by files, using compiler flags, include and library paths
   * and other compiler settings available in the make argument. Each file is compiled as an object file
   * using the fPIC option. Compiled object files are then stored into a dynamic library, which is latter loaded into
   * the JVM. Once the function is loaded, BridJ will match a single function corresponding to the "staged" symbol,
   * having a return type defined by B, and having input values defined in the manifests instances in mList
   *
   * @param files List of source files that are supposed to be compiled and linked into the VM
   * @param make Make class that compiles each source files, and creates a shared library ready for the VM
   * @param funcName The function name that is about to be linked to the VM
   * @param mList Specifies the datatype of the input arguments of the "staged" function
   * @tparam B Specifies the return type of the "staged" function
   * @return Returns a BridJ DynamicFunction which is able to invoke the native C "staged" function
   */
  def compileFilesBridJ[B:Manifest](files: List[File], make: Make, funcName: String = "staged")(implicit mList: List[Manifest[Any]]) =
  {
    printDebug3("Creating the library file ...")
    val libFile = make.makeSharedLib(files)
    val libFileName: String = libFile.getAbsolutePath

    printDebug3("Link the library to bridJ ... " + libFileName );
    val library = BridJ.getNativeLibrary(libFileName)

    printDebug3("Create dynamic function ...");
    val func = createBridJDynamicFunction[B](library, funcName)
    synchronized {
      funcToLib.update(libFileName, (libFileName, library, func.asInstanceOf[DynamicFunction[Any]]))
    }
    (func, libFileName)
  }

  /**
   * createBridJDynamicFunction is called when the library is successfully loaded into the JVM. Its purpose is to provide
   * a Scala / Java version of the function that will be invoked in the JVM code later. The function is being matched by
   * its symbol pointer, reference by the name of the function, as well as it types. Scala types must be mapped into Java
   * types, such that BridJ is later able to match them into the JVM.
   *
   * @param libFile Native libray file which has been loaded into the JVM
   * @param funcName Name of the function that is about to be mapped by BridJ
   * @param mList Manifest list of all input arguments of the function
   * @tparam B Return type of the function
   * @return Dynamic Function which points to the native function specified in the libFile
   */
  private def createBridJDynamicFunction[B:Manifest](libFile: NativeLibrary, funcName: String)(implicit mList: List[Manifest[Any]]) = {
    val f = libFile.getSymbolPointer(funcName)
    val mB = mapBridJType(manifest[B])
    def mT(x: Manifest[Any]) = mapBridJType(x)
    f.asDynamicFunction[B](null, mB, mList map mT :_*)
  }

  /**
   * Unloads a single native library, referenced by the function name stored into that library
   * @param programFileName Name of the function that is about to be release from the JVM
   */
  def unloadProgram(programFileName: String) = funcToLib.get(programFileName) match {
    case Some((libFileName,libFile,_)) => synchronized {
      funcToLib.remove(programFileName)
      BridJ.releaseLibrary(libFileName)
    }
    case None => printDebug0("Library could not be released: " + programFileName)
  }

  /**
   * When the CompileVM is being reset, all previously loaded libraries are being released.
   */
  def reset = {
    printDebug3("Releasing all libraries attached by BridJ")
    synchronized {
      funcToLib.foreach(m => BridJ.releaseLibrary(m._2._1))
      funcToLib = new HashMap()
    }
  }

  /**
   * Map scala type into BridJ compatible type. Note that in most cases, a single scala type is being mapped into the
   * corresponding Java type, as BridJ mostly deals with Java types. However, whenever an Array[T] is being passed into
   * the Manifest, the type is being mapped into a Pointer[T].
   *
   * @param mA  Manifest of the type that is about to be mapped
   * @tparam A  Scala type that is supposed to be mapped into BridJ type
   * @return    BridJ type that corresponds the Scala type
   */
  def mapBridJType[A](mA: Manifest[A]) : java.lang.reflect.Type = if ( mA.erasure.isArray ) {
    val T = mapBridJType(mA.typeArguments(0))
    Pointer.pointerType(T)
  } else mA match {
    case _ if mA <:< manifest[Unit]    => java.lang.Void.TYPE
    case _ if mA <:< manifest[Double]  => java.lang.Double.TYPE
    case _ if mA <:< manifest[Float]   => java.lang.Float.TYPE
    case _ if mA <:< manifest[Char]    => java.lang.Character.TYPE
    case _ if mA <:< manifest[Boolean] => java.lang.Boolean.TYPE
    case _ if mA <:< manifest[Long]    => java.lang.Long.TYPE
    case _ if mA <:< manifest[Int]     => java.lang.Integer.TYPE
    case _ if mA <:< manifest[Short]   => java.lang.Short.TYPE
    case _ if mA <:< manifest[Byte]    => java.lang.Byte.TYPE
    case _ if mA <:< manifest[ULong]   => java.lang.Long.TYPE
    case _ if mA <:< manifest[UInt]    => java.lang.Integer.TYPE
    case _ if mA <:< manifest[UShort]  => java.lang.Short.TYPE
    case _ if mA <:< manifest[UByte]   => java.lang.Byte.TYPE
  }

  def getDefaultJNIMakefile (): Make = {

    val jdkHome = LocalSystem.getJDKHome match {
      case Some(exec) => exec
      case None => throw new RuntimeException("JNI Compilation is impossible. JDK not found.")
    }

    val localCompiler = LocalSystem.getCompiler() match {
      case Some(comp) => comp
      case None => throw new RuntimeException("JNI Compilation is impossible. C compiler is not found.")
    }

    val includePath = jdkHome + File.separatorChar + "include"
    val specificOS  = new File(includePath).listFiles().toList.filter(_.isDirectory).map(_.toPath.toAbsolutePath.toString)
    val jvmArchFlag = LocalSystem.getJVMArch() match {
      case Some(ArchType.x86_64) => List(CompilerFlags.m64)
      case _ => Nil
    }

    new Make () {
      val compiler: AbstractCompiler = localCompiler
      val compilerFlags = List (CompilerFlags.w, CompilerFlags.cstd, CompilerFlags.O3, CompilerFlags.xHost) ::: jvmArchFlag
      val incPaths: List[String] = includePath :: specificOS
      val libPaths: List[String] = List.empty[String]
      val staticLibraries: List[String] = List.empty[String]
      val dynamicLibraries = List("lm")
    }
  }


  def compileFilesJNI(files: List[File], funcName: String, make: Make): Unit =
  {
    printDebug3("Creating the library file ...")
    val libFile = make.makeSharedLib(files)
    val libFileName: String = libFile.getAbsolutePath

    printDebug3("Link the library to the JVM ... " + libFileName );
    System.load(libFileName)
  }
}
