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

import ch.ethz.acl.commons.compiler.OSType.{OSType, _}
import ch.ethz.acl.commons.compiler.CompilerFlags.CompilerFlags
import ch.ethz.acl.commons.util.{Utilities, Debugging}
import Utilities._
import ch.ethz.acl.commons.util.Debugging
import com.jcraft.jsch.MAC
import org.apache.commons.io.FilenameUtils

import scala.sys.process.{Process, ProcessLogger}

object Make extends Debugging {
  def gccGenericMake() = new Make () {
    val compiler = getOS() match {
      case _ => new GCC()
    }
    val compilerFlags = List(
      CompilerFlags.O3,
      CompilerFlags.xHost,
      CompilerFlags.cstd
    )
    val incPaths  = List.empty[String]
    val libPaths  = List.empty[String]
    val dynamicLibraries = List.empty[String]
    val staticLibraries = List.empty[String]
  }
}

abstract class Make extends Executable {

  class MakeException(msg: String) extends Exception(compiler.getCompilerName() + " error: " + msg)

  val compiler      : AbstractCompiler
  val compilerFlags : List[CompilerFlags]

  val incPaths         : List[String]
  val libPaths         : List[String]
  val dynamicLibraries : List[String]
  val staticLibraries  : List[String]

  protected lazy val compilerFile: File = new File(compiler.getCompilerExec())

  def execOpts (objFileNames: List[String], outputFile: String): List[String] = {
    val (iPaths, lPaths, dLibs, sLibs) = getAllLibs(); import ch.ethz.acl.commons.compiler.CompilerFlags._
    (compilerFlags map getFlag) ::: iPaths ::: lPaths ::: List(getFlag(o), outputFile) ::: objFileNames ::: dLibs ::: sLibs
  }

  def sharedLibraryOpts (objFileNames: List[String], outputFile: String): List[String] = {
    val (iPaths, lPaths, dLibs, sLibs) = getAllLibs(); import ch.ethz.acl.commons.compiler.CompilerFlags._
    iPaths ::: lPaths ::: List(getFlag(shared), getFlag(o), outputFile) ::: objFileNames ::: dLibs ::: sLibs
  }

  def objectFileOpts(fPIC: Boolean = false): List[String] = {
    val cFlags = fPIC match {
      case false  => compilerFlags map getFlag
      case true => (compilerFlags :+ CompilerFlags.fPIC) map getFlag
    }
    val iPaths = incPaths.map (inc => getFlag(CompilerFlags.I) + inc).toList
    cFlags ::: iPaths ::: (List(CompilerFlags.c, CompilerFlags.o) map getFlag)
  }

  def makeSharedLib (files: List[File]): File = {
    val objFiles = files.map(file => compileObjectFile(file, true)).toList
    val ext = getOS () match {
      case OSType.WINDOWS => "dll"
      case OSType.MAC => "dylib"
      case OSType.LINUX => "so"
      case _ => throw new MakeException("Can not determine dynamic lib extension")
    }
    val libFileName = FilenameUtils.removeExtension(objFiles(0).getAbsolutePath) + "." + ext
    val paths: List[String] = objFiles.map(f => f.getAbsolutePath)
    if (compile(sharedLibraryOpts(paths, libFileName))) {
      new File(libFileName)
    } else {
      throw new MakeException("Compilation had errors")
    }
  }

  def makeExecutabe (files: List[File]): File = {
    val objFiles = files.map(file => compileObjectFile(file)).toList
    val ext = getOS () match {
      case OSType.WINDOWS => ".exe"
      case OSType.MAC => ""
      case OSType.LINUX => ""
      case _ => throw new MakeException("Can not determine executable extension")
    }
    val execFileName = FilenameUtils.removeExtension(objFiles(0).getAbsolutePath) + ext
    val paths: List[String] = objFiles.map(f => f.getAbsolutePath)
    if (compile(execOpts(paths, execFileName))) {
      new File(execFileName)
    } else {
      throw new MakeException("Compilation had errors")
    }
  }

  def compileObjectFile(file: File, fPIC: Boolean = false): File = {
    val objectFilePath = FilenameUtils.removeExtension(file.getAbsolutePath) + ".o"
    val command = objectFileOpts (fPIC) ::: List(objectFilePath, file.getAbsolutePath)
    if (compile(command)) {
      new File(objectFilePath)
    } else {
      throw new MakeException("Compilation had errors")
    }
  }

  private def compile(params: List[String]): Boolean = {
    val c = compilerFile.getAbsolutePath
    execute((c :: params).toSeq) match {
      case (0, o, e) => (o:::e) map printDebug3; true
      case (_, o, e) => (o:::e) map printDebug3; false
    }
  }

  private def getAllLibs () = {
    val iPaths = incPaths.map  (inc => getFlag(CompilerFlags.I) + inc).toList
    val lPaths = libPaths.map  (lib => getFlag(CompilerFlags.L) + lib).toList
    val dLibs  = dynamicLibraries.map(lib => compiler.toOpts(List(lib))).toList
    val sLibs  = staticLibraries.map (lib => lib.trim).toList
    (iPaths, lPaths, dLibs, sLibs)
  }

  private def getFlag (flag: CompilerFlags): String = compiler.toOpts(compiler.getFlag(flag))
}
