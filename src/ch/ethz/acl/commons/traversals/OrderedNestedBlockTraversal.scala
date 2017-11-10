/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
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

import scala.lms.internal.NestedBlockTraversal

trait OrderedNestedBlockTraversal extends NestedBlockTraversal {

  import IR._

  override def getStronglySortedSchedule2(scope: List[Stm], level: List[Stm], result: Any): (List[Stm], List[Sym[Any]]) = {


    import scala.collection.mutable
    import scala.lms.util.GraphUtil

    val scopeCache = new mutable.HashMap[Sym[Any],Stm]
    for (stm <- scope; s <- stm.lhs)
      scopeCache(s) = stm

    //TR: wip!

    def deps(st: List[Sym[Any]]): List[Stm] = //st flatMap (scopeCache.get(_).toList)
    {
      val l1 = st flatMap (scopeCache.get(_).toList) distinct; // need distinc??
      /*val l2 = scope.filter(d => (st intersect d.lhs).nonEmpty) sortBy(_.lhs.intersec(st).map(_.id).min)
      if (l1 != l2) {
        println("l1: " + l1)
        println("l2: " + l2)
      }*/
      l1
    }

    val fixed = new mutable.HashMap[Any,List[Sym[Any]]]
    def allSyms(r: Any) = fixed.getOrElse(r, syms(r) ++ softSyms(r))


    val inner = scope diff level // TODO: restrict to things referenced by functions (not ifs) ?

    var recursive: List[Sym[Any]] = Nil

    var xx = GraphUtil.stronglyConnectedComponents[Stm](deps(allSyms(result)), t => deps(allSyms(t.rhs)))
    xx.foreach { xs =>
      if (xs.length > 1 && (xs intersect level).nonEmpty) {
        printdbg("warning: recursive schedule for result " + result + ": " + xs)

        // find things residing on top level
        val fs = (xs intersect level) flatMap (_.lhs)

        recursive = fs ::: recursive

        // eliminate all outward dependencies
        // CAVEAT: this *only* works for lambdas
        // problematic if sym is used both in a lambda and an if branch (may lead to NPE)
        // TODO: restrict 'inner' to functions
        // CAVEAT: even for lambdas, this works *only* if the initialization happens before the first call
        // TODO: can we check that somehow? -- maybe insert a dep from the call
        (inner intersect xs) foreach {
          case stm if allSyms(stm.rhs) exists (fs contains _) =>
            fixed(stm.rhs) = allSyms(stm.rhs) filterNot (fs contains _)
            printdbg("fixing deps of " + stm.rhs + " to " + fixed(stm.rhs))
          case _ =>
        }

        // also remove direct inner deps (without inner stms): x1 = Lambda { x2 => Block(x3) }
        (level intersect xs) foreach {
          case stm if allSyms(blocks(stm.rhs)) exists (fs contains _) =>
            fixed(stm.rhs) = allSyms(stm.rhs) filterNot (fs contains _)
            printdbg("fixing deps of " + stm.rhs + " to " + fixed(stm.rhs))
          case _ =>
        }
      }
    }
    xx = GraphUtil.stronglyConnectedComponents[Stm](deps(allSyms(result) ++ allSyms(recursive)), t => deps(allSyms(t.rhs)))
    xx.foreach { xs =>
      if (xs.length > 1 && (xs intersect level).nonEmpty) {
        // see test5-schedfun. since we're only returning level scope (not inner)
        // we're still fine if the order for strictly inner stms is not quite right
        // but we need to ensure that levelScope's order is correct.
        printerr("error: recursive schedule did not go away for result " + result + ": " + xs)
      }
    }
    val xxf = xx.flatten.reverse
    (xxf filter (level contains _), recursive)
  }

}
