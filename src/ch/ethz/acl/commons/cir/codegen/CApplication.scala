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

package ch.ethz.acl.commons.cir.codegen

import ch.ethz.acl.commons.util.Utilities
import com.sun.org.apache.bcel.internal.classfile.LineNumber

class CApplication (name: String) {

  private var preamble             = ""
  private var generatedCode        = ""
  private var systemHeaders        = List.empty[String]
  private var systemLibraries      = List.empty[String]
  private var registeredStructures = List.empty[CStructure]


  def addSystemHeader(headerName: String) = {
    if (!systemHeaders.contains(headerName)) {
      systemHeaders :+= headerName
    }
  }

  def addSystemLibrary(libraryName: String) = {
    if (!systemLibraries.contains(libraryName)) {
      systemLibraries :+= libraryName
    }
  }

  def addStructure (structure: CStructure): Unit = {
    registeredStructures.find(s => s.getName.equals(structure.getName)) match {
      case Some(x) => assert(x == structure)
      case None => registeredStructures :+=  structure
    }
  }

  def setPreamble(text: String) = preamble = text
  def setGeneratedCode(code: String) = generatedCode = code

  def generateHeaders () = {
    val headers = systemHeaders.filterNot(h => h.trim().equals(""))
    headers.map(h => s"#include <$h>").mkString("\n")
  }

  def generateStructures () = {
    val deps = registeredStructures map {
      case CFunction(header, body) => {
        header.trim + "\n" + body.trim + "\n"
      }
      case CEnum(name, body) => {
        "typedef enum " + body + name + ";"
      }
      case CStruct(name, body) => {
        "typedef struct " + body + name + ";"
      }
      case CTypeDef(name, body) => {
        "typedef struct " + body + name + ";"
      }
      case CDefinition(name, body) => {
        name + " = " + body + ";"
      }
    }
    deps.mkString("\n\n")
  }


  def generateSingleFile (lineNumbers1: Boolean = false): String = {
    val lineNumbers = false
    val singleFileBundle =
      s"""
        |$preamble
        |${generateHeaders()}
        |
        |${generateStructures()}
        |
        |$generatedCode
        |
      """.stripMargin

    val strFile = Utilities.indent(singleFileBundle).trim

    if (lineNumbers) {
      val lines = strFile.lines.toArray

      val digits = lines.size.toString.size
      val formatString = s"%${digits}d"

      val nbLines = (1 to lines.size)map(i => {
        formatString.format(i) + " | " + lines(i-1)
      })
      nbLines.mkString("\n")
    } else {
      strFile
    }
  }

}
