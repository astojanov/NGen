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
  *                2015 Tiark Rompf   (tiark@purdue.edu)
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

package ch.ethz.acl.commons.traversals

import scala.collection.mutable
import scala.lms.internal.{Effects, Expressions, NestedBlockTraversal}
import scala.lms.util.GraphUtil

/**
 * The NodePreference trait enforces definition ranks, to ensure that some
 * instructions / definitions appear earlier in the code, and other appear
 * latter. In a nutshell, the getRank function returns an integer, which
 * is then used in the sorting of the definition traversal, when DSL nodes
 * (or IR definitions) have the same order of dependency. The NodePreference
 * does not violate definition dependency, however, it ensures that nodes
 * having same rank will likely be close in the generated code.
 *
 * The definition / node rank is determined with integer values. Use:
 * maxRank, minRank and midRank to define ranks of your DSL definitions.
 */
trait NodePreference extends NestedBlockTraversal {

  val IR: Expressions with Effects
  import IR._

  def minRank () = Int.MinValue
  def maxRank () = Int.MaxValue
  def midRank () = 0

  def getRank(s: Stm): Int = s match {
    case _ => midRank()
  }

  override def getStronglySortedSchedule2 (
    scope: List[Stm],
    level: List[Stm],
    result: Any
  ): (List[Stm], List[Sym[Any]]) = {
    (level, Nil) // override -- we don't have recursive dependencies ...
  }


  override def traverseStm(stm: Stm) = stm match {
    case _ => super.traverseStm(stm)
  }

  override def getSchedule(scope: List[Stm])
    (result: Any, sort: Boolean = true): List[Stm] =
  {
    val scopeCache = new mutable.HashMap[Sym[Any],Stm]
    for (stm <- scope; s <- stm.lhs)
      scopeCache(s) = stm

    def deps(st: List[Sym[Any]]): List[Stm] = {
      st flatMap (scopeCache.get(_).toList)
    }

    val xx = GraphUtil.stronglyConnectedComponents[Stm](
      deps(syms(result)), t => deps(syms(t.rhs))
    )

    if (sort) xx.foreach { x => if (x.length > 1) {
      printerr("warning: recursive schedule for result " + result + ": " + x)
      (new Exception) printStackTrace
    }}

    var remstms = xx.flatten.reverse.sortBy(getRank)
    val remsyms = new mutable.HashSet[Sym[Any]]
    val temp = new mutable.ListBuffer[Stm]

    for (r <- remstms) remsyms ++= r.lhs

    while (remstms.nonEmpty) {
      val idx = remstms.indexWhere(st => !syms(st.rhs)
        .exists(remsyms contains _))
      val (h,t) = remstms.splitAt(idx)
      temp += t.head
      remsyms --= syms(t.head.lhs)
      remstms = h ++ t.tail
    }

    val yy = temp.result
    yy
  }

}
