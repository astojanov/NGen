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
package ch.ethz.acl.commons.util

import java.io._

import ch.ethz.acl.commons.compiler.OSType.{OSType, _}
import ch.ethz.acl.commons.compiler._
import com.github.abrarsyed.jastyle.ASFormatter
import com.github.abrarsyed.jastyle.constants.SourceMode
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

object Utilities {

  def findExec(executableName: String, systemPath: String = System.getenv("PATH")): File = {
    val pathDirs = systemPath.split(File.pathSeparator).reverse
    var fullyQualifiedExecutable:File = null
    pathDirs.foreach ( pathDir => {
      val file = new File(pathDir, executableName)
      if (file.isFile()) {
        fullyQualifiedExecutable = file
      }
    })
    fullyQualifiedExecutable
  }

  def indent (code: String): String = {
    val in  = new StringReader(code)
    val out = new StringWriter()
    val formatter = new ASFormatter ()
    formatter.setSourceStyle(SourceMode.C)
    formatter.setPreprocessorIndent(true)
    formatter.format(in, out)
    out.flush()
    out.toString
  }

  def getTempFile (fileName: String = "staged") : java.io.File = {
    java.io.File.createTempFile(fileName, ".c")
  }

  def dumpCode(code: String, name: String): java.io.File = {
    val file = getTempFile(name)
    val stream = new java.io.PrintWriter(file)
    stream.println(code); stream.close(); file
  }

  def getOS (str: String = System.getProperty("os.name")): OSType = {
    val os = str.toLowerCase
    if (os.contains("win")) WINDOWS else
    if (os.contains("mac") || os.contains("darwin")) MAC else
    if (os.contains("nix") || os.contains("nux") || os.contains("aix")) LINUX
    else UNKNOWN
  }

  def getCurrentGitCommit (): String = {
    val gitPath = new File(".").getAbsolutePath + File.separator + ".git"
    val gitDir = new File(gitPath)
    if ( gitDir.exists() ) {
      val builder = new FileRepositoryBuilder()
      val repository = builder.setGitDir(gitDir).readEnvironment().findGitDir().build()
      repository.resolve("HEAD").getName
    } else {
      "Not a Git version."
    }
  }

  def manifestSimple[T](tp: Manifest[T]): String = {
    val name = tp match {
      case _ if tp <:< manifest[Double]  => "Double"
      case _ if tp <:< manifest[Float]   => "Float"
      case _ if tp <:< manifest[Char]    => "Char"
      case _ if tp <:< manifest[Boolean] => "Boolean"
      case _ if tp <:< manifest[Long]    => "Long"
      case _ if tp <:< manifest[Int]     => "Int"
      case _ if tp <:< manifest[Short]   => "Short"
      case _ if tp <:< manifest[Byte]    => "Byte"
      case _ => tp.erasure.getSimpleName
    }
    tp.typeArguments.isEmpty match {
      case true => name
      case _ => name + "[" + tp.typeArguments.map(t => manifestSimple(t)).mkString(", ") + "]"
    }
  }

  def getTimeStamp() : String = {
    val today = java.util.Calendar.getInstance.getTime
    new java.text.SimpleDateFormat("dd.MM.yyyy HH:mm:ss").format(today)
  }

  def humanReadableByteCount(bytes: Long, si: Boolean = false): String = {
    val unit = if(si) 1000 else 1024
    if (bytes < unit) bytes + " B"
    val exp = (Math.log(bytes) / Math.log(unit)).toInt
    val pre = (if(si) "kMGTPE" else "KMGTPE").charAt(exp-1) + (if(si) "" else "i")
    "%.1f %sB".format(bytes / Math.pow(unit, exp), pre)
  }

  ///////////////////////////////////////////////////////////////
  /////////         THIS MUST BE REMOVED SOON           /////////
  ///////////////////////////////////////////////////////////////

  def getALocalCompiler (): AbstractCompiler = {
    val icc = new ICC()
    if (icc.existLocally()) {
      icc
    } else getOS() match {
      case _ => new GCC()
    }
  }

  def getStandardCompilationFlags () = List(
    CompilerFlags.O3,
    CompilerFlags.xHost,
    CompilerFlags.cstd
  )

  val genericMakefile = new Make () {
    val compiler = getALocalCompiler()
    val compilerFlags = getStandardCompilationFlags()
    val incPaths  = List.empty[String]
    val libPaths  = List.empty[String]
    val dynamicLibraries = List.empty[String]
    val staticLibraries = List.empty[String]
  }

  ///////////////////////////////////////////////////////////////
}
