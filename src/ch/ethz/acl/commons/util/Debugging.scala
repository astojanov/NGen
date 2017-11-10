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

import java.io.PrintStream

import com.typesafe.config.ConfigFactory

import scala.reflect.runtime.{universe => ru}

object DebuggingConfig {
  private val config = ConfigFactory.load("ch.ethz.acl.settings")
  /**
   * Enable or disable debugging globally
   */
  val debug     = config.getBoolean("settings.debugging")
  /**
   * verbosity defines the wordines of the output:
   * 0 - required debugging for error reporting
   * 1 - high-level illustration of the execution process
   * 2 - output inside transformations (for example IR rewrites, etc)
   * 3 - output everything, including informative and auxilary computations
   */
  var verbosity = config.getInt    ("settings.verbosity")
}

object StaticDebugging {

  var stream: PrintStream = System.out

  def setStream (s: PrintStream) = stream = s
  def getStream () = stream

  private def getTime() : String = {
    val today = java.util.Calendar.getInstance.getTime
    new java.text.SimpleDateFormat("HH:mm:ss").format(today)
  }

  private def getClassName (c: Any) : String = {
    try {
      val mirror = ru.runtimeMirror(c.getClass.getClassLoader)
      val instance = mirror.reflect(c)
      val baseClasses = instance.symbol.toType.erasure.baseClasses
      val classType   = baseClasses.dropWhile(_.toString.contains("anonymous class"))
      classType(0).toString
    } catch {
      case _: Throwable => c.getClass.getSimpleName
    }
  }

  def printDebugGeneric[T] (v: Int, s: T, c: Any, debug: Boolean): Unit = if (debug) {
    if (v <= DebuggingConfig.verbosity)
      getStream().println("[" + v + "][" + getTime() + " " + getClassName(c) + "] " + s.toString)
  }
}

trait Debugging {

  protected var debug: Boolean = DebuggingConfig.debug

  private def printDebug[T] (v: Int, s: T) = StaticDebugging.printDebugGeneric(v, s, this, debug)

  protected def printDebug0[T] (s: T) = printDebug(0, s)
  protected def printDebug1[T] (s: T) = printDebug(1, s)
  protected def printDebug2[T] (s: T) = printDebug(2, s)
  protected def printDebug3[T] (s: T) = printDebug(3, s)


  def setDebugging     (d: Boolean) = debug = d
  def enableDebugging  () = debug = true
  def disableDebugging () = debug = false
}
