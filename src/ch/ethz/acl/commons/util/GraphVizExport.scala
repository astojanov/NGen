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

import java.io.PrintWriter

import scala.lms.common.BaseExp
import scala.lms.internal.GraphTraversal

trait GraphVizExport extends GraphTraversal with Debugging {

  val IR: BaseExp
  import IR._

  protected var defColors = Map.empty[Def[Any], String]

  def setDefColors(colors: Map[Def[Any], String]) = {
    defColors = colors
  }

  private var colorIdx = 0
  private val palette = List(
    "ff1a00", "cc0000", "ff7400", "008c00", "006e2e", "4096ee",
    "ff0084", "b02b2c", "d15600", "c79810", "73880a", "6bba70",
    "3f4c6b", "356aa0", "d01f3c", "cdeb8b", "c3d9ff"
  )

  def getFreshColor(): String = { val c = palette(colorIdx); colorIdx = (colorIdx + 1) % palette.size; "\"#" + c + "\"" }
  def getTextColor(c1: String): String = {
    val c = c1.replace("#", "").replace("\"", "")
    val r = Integer.parseInt(c.substring(0, 2), 16)
    val g = Integer.parseInt(c.substring(2, 4), 16)
    val b = Integer.parseInt(c.substring(4, 6), 16)
    val a = 1 - ( 0.299 * r + 0.587 * g + 0.114 * b) / 255.0
    "\"#" + (if (a < 0.5) "000000" else "ffffff") + "\""
  }

  protected def quote(x: Any) = "\""+x+"\""
  protected def toNumericString[A](value: A)(implicit n: Numeric[A]) =  n.toFloat(value).formatted("%.16f").toString
  protected def toGraphVizSaveString(x: String): String = x.replace("<", "\\<").replace(">", "\\>")
  protected def symToStruct(sym: Sym[Any]): String = "x" + sym.id.toString()

  case class MRecord(fillcolor: String, fontcolor: String, fontsize: Double, landscape: Boolean) {
    var mrecord = ""
    var sym: Sym[Any] = null
    def printStream()(implicit stream: PrintWriter): Unit = {
      stream.print(symToStruct(sym) + " [")
      landscape match {
        case true  => stream.print(" label=" + quote(mrecord))
        case false => stream.print(" label=" + quote("{ " + mrecord + "}"))
      }
      stream.print(" style=filled")
      stream.print(" fontcolor=" + fontcolor);
      stream.print(" color=\"#c6c6c6\"");
      stream.print(" fillcolor=" + fillcolor)
      stream.print(" fontsize=" + fontsize.formatted("%.1f"))
      stream.print(" shape=" + "Mrecord")
      stream.println(" ]")
    }
  }

  def exportBlock[A, B](syms: List[Sym[A]], block: Block[B], fileName: String): Unit = {
    exportExp[B](syms, block.res, fileName)
  }

  def exportExp[B](syms: List[Sym[Any]], f: Exp[B], fileName: String) : Unit
}
