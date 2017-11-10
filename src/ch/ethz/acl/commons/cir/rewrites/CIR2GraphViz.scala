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
  *  Copyright (C) 2013 Alen Stojanov (astojanov@inf.ethz.ch)
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

package ch.ethz.acl.commons.cir.rewrites

import java.io.PrintWriter

import ch.ethz.acl.commons.cir.CIR

import scala.collection.mutable
import scala.lms.internal.{GraphTraversal, NestedBlockTraversal}

trait CIR2GraphViz extends GraphTraversal  {  self =>

  val IR: CIR
  import IR._

  private var constCount = 0
  var landscape = true
  var title: String = ""

  var adds = -1
  var muls = -1

  def quote(x: Any) = "\""+x+"\""

  var noSym: IR.Sym[Any] = null // fresh[Any]
  // val noSym = Sym(Int.MaxValue)

  var constants: List[Double] = List.empty[Double]
  var nodeShape: mutable.HashMap[Sym[Any], String] = null
  var arrayDeps: List[(Sym[Any], Sym[Any], String)] = null

  var disableArrayUpdateDeps = false
  var printConstants = true

  def setDisableArrayUpdateDeps(x: Boolean) = {
    disableArrayUpdateDeps = x
  }

  def toNumericString[A](value: A)(implicit numeric: Numeric[A]) : String = {
    numeric.toFloat(value).formatted("%.16f").toString
  }

  def toGraphVizSaveString(x: String): String = {
    x.replace("<", "\\<").replace(">", "\\>")
  }

  def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Reflect(Comment(_), _, _ ) =>
    case _ => {

      stream.print(symToStruct(sym) + " [")

      val (sign, fillcolor, fontcolor, fontsize, shape) = rhs match {
        case NumericPlus(_, _)   => ("+", "\"#7b7b7b\"", "black", 14.0, "Mrecord")
        // case FloatPlus(_, _)   => ("+", "\"#7b7b7b\"", "black", 14.0, "Mrecord")
        case NumericMinus(_, _)  => ("-", "\"#a7a7a7\"", "black", 14.0, "Mrecord")
        case NumericTimes(_, _)  => ("*", "\"#b90000\"", "white", 14.0, "Mrecord")
        case FloatTimes(_, _)  => ("*", "\"#b90000\"", "white", 14.0, "Mrecord")
        case NumericDivide(_, _) => ("/", "\"#a9d9e7\"", "black", 14.0, "Mrecord")
        case ArrayApply(Sym(x), Const(i)) => ("x" + x + "[" + i + "]", "\"#ffffff\"", "black", 14.0, "Mrecord")
        case Reflect(ArrayApply(Sym(x), Const(i)), _, _) => ("x" + x + "[" + i + "]", "\"#a7e0e7\"", "black", 14.0, "Mrecord")
        case Reflect(ArrayApply(Sym(x), Sym(i)), _, _) => ("x" + x + "[x" + i + "]", "\"#a7e0e7\"", "black", 14.0, "Mrecord")
        case Reflect(ArrayUpdate(Sym(x), Const(i), Sym(a)), _, _) => ("x" + x + "[" + i + "] = x" + a, "\"#ffffff\"", "black", 14.0, "Mrecord")
        case Reflect(ArrayUpdate(Sym(x), Sym(i), Sym(a)), _, _) => ("x" + x + "[x" + i + "] = x" + a, "\"#ffffff\"", "black", 14.0, "Mrecord")
        case Reflect(ArrayNew(Const(size)), _, _) => ("new Array" + "[" + size + "]", "\"#e8e8e8\"", "black", 14.0, "Mrecord")
        case Reflect(ForLoop(start, end, i, inc, body), _, _) => ("ForLoop[" + i.toString + " , " + end.toString + " /" + inc.toString + "/]", "\"#660033\"", "white", 24.0, "record")
        case Reify(Sym(x), _, _) => ("x" + x, "\"#000000\"", "white", 14.0, "Mrecord")
        case Reify(Const(_), _, _) => ("Unit", "\"#000000\"", "white", 14.0, "Mrecord")
        case _ => (rhs.toString(), "\"#ffffff\"", "black", 14.0, "Mrecord")
      }

      if (!landscape)
        stream.print(" label=" + quote("{ <sign> " + toGraphVizSaveString(sign) + "|<sym> x" + sym.id + "}"))
      else
        stream.print(" label=" + quote("<sign> " + toGraphVizSaveString(sign) + "|<sym> x" + sym.id))

      val constNodesList = rhs match {
        case e@NumericPlus(Const(x),Const(y))   => List((x, e.aev), (y, e.aev))
        case e@NumericPlus(_,Const(x))          => List((x, e.aev))
        case e@NumericPlus(Const(x),_)          => List((x, e.aev))
        case e@NumericMinus(Const(x),Const(y))  => List((x, e.aev), (y, e.aev))
        case e@NumericMinus(_,Const(x))         => List((x, e.aev))
        case e@NumericMinus(Const(x),_)         => List((x, e.aev))
        case e@NumericTimes(Const(x),Const(y))  => List((x, e.aev), (y, e.aev))
        case e@NumericTimes(_,Const(x))         => List((x, e.aev))
        case e@NumericTimes(Const(x),_)         => List((x, e.aev))
        case e@NumericDivide(Const(x),Const(y)) => List((x, e.aev), (y, e.aev))
        case e@NumericDivide(_,Const(x))        => List((x, e.aev))
        case e@NumericDivide(Const(x),_)        => List((x, e.aev))
        case _ => Nil
      }

      stream.print(" style=filled")
      stream.print(" fontcolor=" + fontcolor);
      stream.print(" color=\"#c6c6c6\"");
      stream.print(" fillcolor=" + fillcolor)
      stream.print(" fontsize=" + fontsize.formatted("%.1f"))
      stream.print(" shape=" + shape)
      stream.println(" ]")

      nodeShape.update(sym, "record")

      if ( constNodesList != Nil ) {
        constNodesList foreach (tmp => { val (value, aev) = tmp
          constCount += 1;
          stream.print( "const" + constCount.toString + " [")
          stream.print(" label=" + quote(toNumericString(value)(aev)))
          stream.print(" shape=oval")
          stream.print(" fontcolor=\"#c6c6c6\"");
          stream.print(" style=dashed")
          stream.print(" color=\"#c6c6c6\"");
          stream.println(" ]")
          stream.println("const" + constCount.toString + " -> " + symToStruct(sym) + ":sign [style=dashed color=\"#c6c6c6\"]")
        })
      }
    }
  }

  def symToStruct(sym: Sym[Any]): String = {
    "x" + sym.id.toString()
  }

  def printConstants (stream: PrintWriter) {
    if (constants.isEmpty)
    {
      stream.print(" \\n " + constants.size.toString + " Constants: \\n ")
      constants.foreach( c => {
        if ( c >= 0 )
          stream.print("+")
        stream.print(c.formatted("%.16f") + " \\n ")
      })
    }
  }

  def emitClusterNodes (start: Exp[Any], stream: PrintWriter) {

    val emitter = new NestedBlockTraversal {
      val IR: self.IR.type = self.IR
      import IR._

      // stack of cluster maps
      var stack: mutable.Stack[mutable.HashMap[Sym[Any], List[(Sym[Any], Def[Any])]]] = null

      def getNodeCluster (sym: Sym[Any], rhs: Def[Any]): Sym[Any]  = rhs match {
        // case Reflect(ArrayNew(Const(size)), _, _) => "cluster_" + sym.id
        case ArrayApply(s@Sym(x), Const(i)) => s
        case Reflect(ArrayUpdate(s@Sym(x), Const(i), Sym(a)), _, _) => s
        case Reflect(ArrayUpdate(s@Sym(x), Sym(i), Sym(a)), _, _) => s
        case Reflect(ArrayApply(s@Sym(x), Const(i)), _, _) => s
        case Reflect(ArrayApply(s@Sym(x), Sym(i)), _, _) => s
        case _ => noSym
      }

      def clusterNode(s: Sym[Any], d: Def[Any]) = {
        val clusterMap = stack.top
        val cluster = getNodeCluster(s, d)
        val nodes = clusterMap.getOrElse(cluster, List.empty[(Sym[Any], Def[Any])])
        clusterMap.update(cluster, (s, d) :: nodes)
      }


      def emitBlockNodes[A](block: Block[A]): Unit = {
        stack = new mutable.Stack[mutable.HashMap[Sym[Any], List[(Sym[Any], Def[Any])]]] ()
        stack.push(new mutable.HashMap[Sym[Any], List[(Sym[Any], Def[Any])]]())
        traverseBlock(block)
        printClusterNodes(stack.pop(), null)
      }

      def printClusterNodes(clusterMap: mutable.HashMap[Sym[Any], List[(Sym[Any], Def[Any])]], nestedCluster: String) = {
        for ( (cluster, nodes) <- clusterMap ) {
          if ( cluster.equals(noSym) )  {
            for ( (sym, rhs) <- nodes ) {
              emitNode(sym, rhs)(stream)
            }
          } else {
            val clusterName = nestedCluster match {
              case null => "cluster_" + cluster.id
              case _ => "cluster_" + cluster.id + "_" + nestedCluster
            }
            stream.println("subgraph " + clusterName + " {")
            stream.println("style=filled")
            for ( (sym, rhs) <- nodes ) {
              emitNode(sym, rhs)(stream)
            }
            stream.println("fillcolor=\"#e8e8e8\"");
            stream.println("color=\"#e8e8e8\"");
            stream.println("}")
            arrayDeps = (cluster, nodes.head._1, clusterName) :: arrayDeps
          }
        }
      }

      override def traverseStm(stm: Stm): Unit = stm match {
        case TP(s, d@Reflect(ForLoop(start, end, i, inc, body), _, _)) => {

          // start a cluster for the new ForLoop
          stream.println("subgraph cluster_" + s.id + " {")
          stream.println("style=\"filled, dashed\"")
          stream.println("shape=Mbox")
          stream.println("fillcolor=\"#ece5b65f\"")
          stream.println("color=black");

          // create a new element on the stack to accumulate the nodes in the ForLoop
          stack.push(new mutable.HashMap[Sym[Any], List[(Sym[Any], Def[Any])]]())
          super.traverseStm(stm)
          printClusterNodes(stack.pop(), s.id.toString)

          // emit the ForLoop node
          emitNode(s, d)(stream)

          // emit the iterator node
          stream.print(symToStruct(i) + " [")
          stream.print(" label=" + quote("x" + i.id.toString))
          stream.print(" shape=Mdiamond")
          stream.println(" ]")

          // save the shape of the iterator
          nodeShape.update(i, "diamond")

          // finish the ForLoop cluster
          stream.println("}")

        }

        case TP(s, d) => {
          super.traverseStm(stm)
          clusterNode(s, d)
          // emitNode(s, d)(stream)
        }
        case _ => super.traverseStm(stm)
      }
    }
    emitter.emitBlockNodes(Block(start))
  }

  def getDepEdge (sym:Sym[Any], dep:Sym[Any]): (String, String) = {
    var sDep = "";
    var sSym = "";
    if (nodeShape.getOrElse(dep, "box").equals("record")) {
      sDep = symToStruct(dep) + ":sym"
    } else {
      sDep = symToStruct(dep)
    }
    if (nodeShape.getOrElse(sym, "box").equals("record")) {
      sSym = symToStruct(sym) + ":sign"
    } else {
      sSym = symToStruct(sym)
    }
    (sSym, sDep)
  }
   def emitDeps(sym: Sym[Any], rhs: Def[Any], deps: List[Sym[Any]])(implicit stream: PrintWriter) = {
    val color = rhs match {
      case Reify(_, _, _) => " [color=\"#d0d0d0\"]"
      case _ =>  " [color=\"#909090\"]"
    }
    for (dep <- deps) {
      val (sSym, sDep) =  getDepEdge(sym, dep)
      stream.println(sDep + " -> " + sSym + color)
    }
  }

  def emitArrayDeps(stream: PrintWriter) = {
    arrayDeps.foreach(x => {
      val (array, clusterNode, clusterName) = x
      val (sArray, sClusterNode) = getDepEdge(array, clusterNode)
      stream.println(sArray + " -> " + sClusterNode + "[color=\"#909090\", lhead="+clusterName + "]")
    })
  }

   def emitDepGraph(start: Exp[Any], stream: PrintWriter, landscape: Boolean) {

    constCount = 0;
    this.landscape = landscape

    stream.println("digraph structs {")
    stream.println("compound=true")
    stream.println("node [shape=record];")
    if (landscape) {
      stream.println("rankdir=LR")
    }
    stream.println("ratio=0.70710678118")

    val deflist = buildScheduleForResult(start, false)

    emitClusterNodes(start, stream)


    if (!disableArrayUpdateDeps) {
      for (TP(sym, rhs) <- deflist) {
        val deps = syms(rhs)
        emitDeps(sym, rhs, deps)(stream)
      }
    } else {
      for (TP(sym, rhs) <- deflist) {
        var deps = rhs match {
          case ArrayApply(Sym(x), Const(i)) => Nil
          case ArrayApply(Sym(x), idx@Sym(i)) => List(idx)
          case Reflect(ArrayApply(Sym(x), Const(i)), _, _) => Nil
          case Reflect(ArrayApply(Sym(x), idx@Sym(i)), _, _) => List(idx)
          case Reflect(ArrayUpdate(Sym(x), Const(i), sa@Sym(a)), _, _) => List(sa)
          case Reflect(ArrayUpdate(Sym(x), idx@Sym(i), sa@Sym(a)), _, _) => List(idx, sa)
          case Reflect(Comment(_), _, _) => Nil
          case _ => syms(rhs).distinct
        }

        // Filter all comments
        deps = deps.filter( s => findDefinition(s) match {
          case Some(stm) => stm match {
            case TP(_, Reflect(Comment(_), _, _)) => false
            case _ => true
          }
          case None => true
        })

        // stream.println( "/* " + rhs.toString + "*/" )
        emitDeps(sym, rhs, deps)(stream)
        // stream.println( "/* ===================== */")
      }
      emitArrayDeps(stream)
    }

    stream.println("labelloc=\"b\"")
    stream.print("label=\"\\n" + title + "\\n" + this.adds.toString + " additions, " + this.muls + " multiplications" + " \\n ");
    if (printConstants)
      printConstants(stream)
    stream.println("\"");

    stream.println("}")
    stream.close()
  }

  def exportGraph[A, B](f: Exp[A] => Exp[B], file: String, landscape: Boolean, title: String) (implicit mA: IR.Typ[A], mB: IR.Typ[B]) : Unit = {
    this.adds = -1
    this.muls = -1
    val s = fresh[A]
    val sList = List(s).asInstanceOf[List[Sym[Any]]]
    implicit val mList = List(mA).asInstanceOf[List[Typ[Any]]]
    exportGraph[B]((sList, IR.funcToBlock(f(s))), file, landscape, title)
  }

  def exportGraph[B](block: (List[Sym[Any]], Block[B]), file: String, landscape: Boolean, title: String) (implicit mList: List[IR.Typ[Any]], mB: IR.Typ[B]) : Unit = {
    exportGraph[B](block._2.res, file, landscape, title)
  }

  def exportGraph[B](f: Exp[B], file: String, landscape: Boolean, title: String) (implicit mB: IR.Typ[B]) : Unit = {
    this.title = title
    this.noSym = Sym[Int](-1)
    val stream = new java.io.PrintWriter(new java.io.FileOutputStream(file))
    nodeShape = new mutable.HashMap[Sym[Any], String]()
    arrayDeps = List.empty[(Sym[Any], Sym[Any], String)]
    emitDepGraph(f, stream, landscape)
  }
}
