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

import scala.lms.internal.{Effects, NestedBlockTraversal}

trait DSLUtils extends Effects with Debugging { self =>

  def rewriteConst(s: String) = {
    """Const\((\d*)\)""".r.replaceAllIn(s, """$1""")
  }

  def printBlock[T](block: Block[T], printManifest: Boolean = false) = new NestedBlockTraversal {
    val IR: self.type = self
    var indent = 0;
    override def traverseStm(stm: Stm): Unit = stm match {
      case _ => {
        val str = " " * { indent+=3; indent }
        super.traverseStm(stm)
        printDebug3(str + stm.toString())
        if (printManifest) stm match {
          case TP(s, d) => {
            s :: syms(d) foreach { sym => {
              printDebug3(str + " " + sym + " -> " + sym.tp)
            }}
          }
        }
        indent -= 3
      }
    }
  }.traverseBlock(block)

  def findDefinitionByDeps(opSyms: Set[Sym[Any]], e: Def[Any] = null): List[Stm] = {
    var res = List.empty[Stm]
    globalDefs.foreach( stm => stm match {
      case TP(s, _) => {
        val symSet = syms(stm).toSet - s
        if (symSet.subsetOf(opSyms) && opSyms.subsetOf(symSet)) {
          res = stm :: res
        }
      }
      case _ =>
    })
    res.reverse
  }

  def findDefinitionByDeps(syms: List[Sym[Any]]): List[Stm] = findDefinitionByDeps(syms.toSet)
  def funcToBlock[T:Typ](out: Exp[T]) = reifyEffects[T](out)
}
