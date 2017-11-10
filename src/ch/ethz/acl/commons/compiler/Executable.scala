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

import ch.ethz.acl.commons.util.Debugging
import scala.sys.process.{Process, ProcessLogger}

class Executable extends Debugging {

  def execute (command: String): (Int, List[String], List[String]) = execute(command.split(" "))

  protected def execute(cmd: Seq[String]): (Int, List[String], List[String]) = {
    val command = cmd.map(x=>x.trim()).filter(x=>x!="")
    printDebug3(command.mkString(" "))
    val qb = Process(command)
    var out = List[String]()
    var err = List[String]()
    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)
    (exit, out.reverse, err.reverse)
  }

}
