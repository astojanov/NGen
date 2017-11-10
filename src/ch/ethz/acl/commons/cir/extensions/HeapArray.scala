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

package ch.ethz.acl.commons.cir.extensions

import ch.ethz.acl.commons.cir.codegen.CUnparser
import ch.ethz.acl.commons.extensions.{CastExp, CastExpOpt}
import ch.ethz.acl.commons.types.TheTyp
import scala.collection.mutable
import scala.lms.common._
import scala.lms.internal.NestedBlockTraversal
import scala.reflect.SourceContext

trait HeapArrayBase extends BaseExp { self =>

  abstract class HeapArray[+T]

  implicit def heapArrayTyp[T:Typ]: Typ[HeapArray[T]] = {
    implicit val m: Manifest[T] = TheTyp.toManifest(self)(typ[T])
    ManifestTyp(Predef.manifest[HeapArray[T]])
  }

  class ArrayOpsCls[T:Typ](a: Rep[HeapArray[T]]){
    def alloc   (n: Rep[Int]) = heap_array_alloc[T, T](a, n)
    def apply   (n: Rep[Int])(implicit pos: SourceContext) = heap_array_apply(a, n)
    def update  (n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = heap_array_update(a,n,y)
    def free    () = heap_array_free(a)
  }

  def heap_array_new    [T:Typ]       (): Rep[HeapArray[T]]
  def heap_array_alloc  [T:Typ, U:Typ](arr: Rep[HeapArray[U]], n: Rep[Int]): Rep[Unit]
  def heap_array_free   [T]           (arr: Rep[HeapArray[T]]): Rep[Unit]
  def heap_array_apply  [T:Typ]       (arr: Rep[HeapArray[T]], n: Rep[Int]): Rep[T]
  def heap_array_update [T:Typ]       (arr: Rep[HeapArray[T]], n: Rep[Int], y: Rep[T]): Rep[Unit]
  def heap_array_swap   [T:Typ, U:Typ](src: Rep[HeapArray[T]], dst: Rep[HeapArray[U]]): Rep[Unit]
}

trait HeapArrayExp extends HeapArrayBase with BaseExp with EffectExp with CastExp {

  protected lazy val theHeapSync = reflectMutableSym(fresh(manifestTyp[Any]))

  protected class HeapArrayDef[T:Typ] extends Def[HeapArray[T]] {
    val m = manifest[T]
  }

  protected class HeapArrayDefUnit[T:Typ, U:Typ] extends Def[Unit] {
    val (mT, mU) = (manifest[T], manifest[U])
  }

  case class HeapArrayEmpty [T:Typ]() extends HeapArrayDef[T]
  case class HeapArrayFree  [T]    (arr: Exp[HeapArray[T]]) extends Def[Unit]
  case class HeapArrayApply [T:Typ](arr: Exp[HeapArray[T]], n: Exp[Int]) extends Def[T]
  case class HeapArrayUpdate[T:Typ](arr: Exp[HeapArray[T]], n: Exp[Int], y: Exp[T]) extends Def[Unit] {
    val m = manifest[T]
  }
  case class HeapArrayAlloc [T:Typ, U:Typ](a: Exp[HeapArray[U]], n: Exp[Int])                       extends HeapArrayDefUnit[T, U]
  case class HeapArraySwap  [T:Typ, U:Typ](s: Exp[HeapArray[T]], d: Exp[HeapArray[U]])              extends HeapArrayDefUnit[T, U]
  case class HeapArrayCopy  [T:Typ, U:Typ](s: Exp[HeapArray[T]], d: Exp[HeapArray[U]], l: Rep[Int]) extends HeapArrayDefUnit[T, U]
  case class HeapArrayCast  [T:Typ, U:Typ](src: Exp[HeapArray[T]]) extends Def[HeapArray[U]] {
    val (mT, mU) = (manifest[T], manifest[T])
  }

  def heap_array_new  [T:Typ](): Exp[HeapArray[T]] = {
    reflectMutable(HeapArrayEmpty[T]())
  }

  def heap_array_alloc[T:Typ, U:Typ](arr: Exp[HeapArray[U]], n: Exp[Int]): Exp[Unit] = {
    reflectWrite(theHeapSync, getWriteExp(arr))(HeapArrayAlloc[T, U](arr, n))
  }

  def heap_array_free[T](arr: Exp[HeapArray[T]]): Exp[Unit] = {
    reflectWrite(theHeapSync, arr)(HeapArrayFree(arr))
  }

  def heap_array_apply [T:Typ](x: Exp[HeapArray[T]], n: Exp[Int]): Exp[T] = {
    HeapArrayApply(x, n)
  }
  
  def heap_array_update[T:Typ](x: Exp[HeapArray[T]], n: Exp[Int], y: Exp[T]): Exp[Unit] = {
    reflectWrite(getWriteExp(x))(HeapArrayUpdate(x, n, y))
  }

  def heap_array_swap[T:Typ, U:Typ](src: Exp[HeapArray[T]], dst: Exp[HeapArray[U]]): Exp[Unit] = {
    reflectWrite(theHeapSync, getWriteExp(src), getWriteExp(dst))(HeapArraySwap[T, U](src, dst))
  }

  def heap_array_copy[T:Typ, U:Typ](src: Exp[HeapArray[T]], dst: Exp[HeapArray[U]], size: Rep[Int]): Exp[Unit] = {
    reflectWrite(theHeapSync, getWriteExp(dst))(HeapArrayCopy[T, U](src, dst, size))
  }

  def heap_array_cast[T:Typ, U:Typ](s: Exp[HeapArray[T]]): Exp[HeapArray[U]] = {
    HeapArrayCast[T, U](s)
  }

  protected def getWriteExp[T](array: Exp[HeapArray[T]]): Exp[Any] = array match {
    case Def(Reflect(HeapArrayCast(a), _, _)) => getWriteExp(a)
    case Def(HeapArrayCast(a)) => getWriteExp(a)
    case _ => array
  }

  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case HeapArrayCopy(a, b, c) => aliasSyms(b) ::: aliasSyms(c)
    case _ => super.aliasSyms(e)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case c@HeapArrayCast(a) => heap_array_cast(f(a))(mtype(c.mT), mtype(c.mU))
    case Reflect(c@HeapArrayCast (a), u, es) =>
      reflectMirrored(Reflect(HeapArrayCast(f(a))(c.mT, c.mU), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(c@HeapArrayEmpty (), _, _) => heap_array_new()(c.m)
    case Reflect(c@HeapArrayAlloc(arr, n), u, es) =>
      reflectMirrored(Reflect(HeapArrayAlloc(f(arr),f(n))(c.mT, c.mU), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(c@HeapArrayFree(arr), u, es) =>
      reflectMirrored(Reflect(HeapArrayFree(f(arr)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case Reflect(HeapArrayApply(l,r), u, es) =>
      reflectMirrored(Reflect(HeapArrayApply(f(l),f(r))(mtype(manifest[A])), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case Reflect(c@HeapArrayUpdate(l,i,r), u, es) =>
      reflectMirrored(Reflect(HeapArrayUpdate(f(l),f(i),f(r))(c.m), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(c@HeapArraySwap(src, dst), u, es) =>
      reflectMirrored(Reflect(HeapArraySwap(f(src),f(dst))(c.mT, c.mU), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case Reflect(c@HeapArrayCopy(src, dst, size), u, es) =>
      reflectMirrored(Reflect(HeapArrayCopy(f(src),f(dst), f(size))(c.mT, c.mU), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

}


trait HeapArrayExpOpt extends HeapArrayExp with CastExpOpt {

  override def heap_array_cast[T:Typ, U:Typ](s: Exp[HeapArray[T]]): Exp[HeapArray[U]] = s match {
    case _ if typ[T] <:< typ[U] && typ[U] <:< typ[T] => s.asInstanceOf[Exp[HeapArray[U]]]
    case Def(d@HeapArrayCast(arr)) => heap_array_cast(arr)(d.mT, typ[U])
    case _ => super.heap_array_cast[T, U](s)
  }

  def heap_array_realloc[T:Typ, U:Typ](arr: Exp[HeapArray[U]], n: Exp[Int]): Exp[Unit] = {
    if (context ne null) {
      val vs = arr.asInstanceOf[Sym[HeapArray[T]]]
      val rhs = context.reverse.collectFirst {
        case Def(Reflect(d@HeapArrayAlloc(`arr`, `n`), _, _)) if d.mT <:< manifest[T] && manifest[T] <:< d.mT => {
          Some(Const(()))
        }
        case Def(Reflect(d@HeapArrayFree(`arr`), _, _))    => None
        case Def(Reflect(d@HeapArraySwap(`arr`, _), _, _)) => None

//        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse({
        heap_array_free(arr)
        heap_array_alloc[T, U](arr, n)
      })
    } else {
      heap_array_free(arr)
      heap_array_alloc[T, U](arr, n)
    }
  }

  override def heap_array_apply[T:Typ](x: Exp[HeapArray[T]], n: Exp[Int]): Exp[T] = {
    if (context ne null) {
      val vs = x.asInstanceOf[Sym[HeapArray[T]]]
      val rhs = context.reverse.collectFirst {
        case Def(Reflect(HeapArrayUpdate(`x`, `n`, rhs: Exp[T]), _, _)) => Some(rhs)
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.heap_array_apply(x, n))
    } else {
      super.heap_array_apply(x,n)
    }
  }

  override def heap_array_update[T:Typ](x: Exp[HeapArray[T]], n: Exp[Int], y: Exp[T]) = {
    if (context ne null) {
      // find the last modification of array x
      // if it is an assigment at index n with the same value, just do nothing
      val vs = x.asInstanceOf[Sym[HeapArray[T]]]
      //TODO: could use calculateDependencies?

      val rhs = context.reverse.collectFirst {
        //case w @ Def(Reflect(ArrayNew(sz: Exp[T]), _, _)) if w == x => Some(Const(())) // FIXME: bounds check!
        case Def(Reflect(HeapArrayUpdate(`x`, `n`, `y`), _, _)) => Some(Const(()))
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
      }
      rhs.flatten.getOrElse(super.heap_array_update(x,n,y))
    } else {
      super.heap_array_update(x, n, y)
    }
  }
}


trait HeapArrayExpOptExtra extends HeapArrayExpOpt {

  override def heap_array_apply[T:Typ](x: Exp[HeapArray[T]], n: Exp[Int]): Exp[T] = n match {
    case Const(_) if context ne null => {
      val vs = x.asInstanceOf[Sym[HeapArray[T]]]
      val matchLastUpdate = context.reverse.dropWhile({
        case Def(Reflect(HeapArrayUpdate(`x`, idx@Const(_), rhs: Exp[T]), _, _)) => idx != n
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => false // not a simple assignment
        case _ => true
      })

      val rhs = if (matchLastUpdate.isEmpty) None else  {
        matchLastUpdate.head match {
          case Def(Reflect(HeapArrayUpdate(`x`, `n`, rhs: Exp[T]), _, _)) => Some(rhs)
          case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None // not a simple assignment
        }
      }
      rhs.getOrElse(super.heap_array_apply(x, n))

    }
    case _ => super.heap_array_apply(x, n)
  }
}

trait FreeHeapArraysTransformer extends ForwardTransformer { self =>

  val IR: BaseFatExp with HeapArrayExp
  import IR._

  private var blockToArrayMap = Map.empty[Exp[Any], mutable.HashSet[Exp[HeapArray[Any]]]]

  private lazy val traversal = new NestedBlockTraversal {

    val IR: self.IR.type = self.IR
    val blockPointerStack = new mutable.Stack[mutable.HashSet[Exp[HeapArray[Any]]]]()

    override def traverseStm(stm: Stm) = {
      stm match {
        case TP(arr, Reflect(HeapArrayEmpty(), _, _)) =>  {
          val exp = arr.asInstanceOf[Exp[HeapArray[Any]]]
          blockPointerStack.top.add(exp)
        }
        case _ => // do nothing
      }
      super.traverseStm(stm)
    }

    override def traverseBlock[A](block: Block[A]): Unit = {
      val set = mutable.HashSet.empty[Exp[HeapArray[Any]]]
      blockPointerStack.push(set)
      super.traverseBlock(block)
      val arrays = blockPointerStack.pop()
      blockToArrayMap += (getBlockResult(block) -> arrays)
    }
  }

  override def transformStm(stm: Stm) = stm match {
    case TP(s, d@Reify(block, _, _)) => {
      val arrays = blockToArrayMap(block)
      arrays foreach { arr =>
        heap_array_free(apply(arr))
      }
      self_mirror(s, d)
    }
    case _ => super.transformStm(stm)
  }


  def createFreeStms[A:Typ](block: Block[A]): Block[A] = {
    blockToArrayMap = Map.empty[Exp[Any], mutable.HashSet[Exp[HeapArray[Any]]]]
    traversal.traverseBlock(block)
    transformBlock(block)
  }
}



trait CGenHeapArray extends CUnparser { self =>

  val IR: HeapArrayExp
  import IR._

  override def remap[T](m: Typ[T]): String = m match {
    case _ if m <:< ManifestTyp(manifest[HeapArray[Any]]) => {
      remap(m.typeArguments.head) + "*"
    }
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case HeapArrayEmpty() => {
      cApp.addSystemHeader("stdlib.h")
      stream.println(remap(sym.tp.typeArguments.head) + "* " + quote(sym) + " = NULL;")
    }
    case HeapArrayFree  (arr) => {
      cApp.addSystemHeader("stdlib.h")
      stream.println("free(" + quote(arr) + ");")
    }
    case HeapArrayCast(a) => emitValDef(sym, "(" + remap(sym.tp) + ") " + quote(a))
    case HeapArrayApply (arr, n) => emitValDef(sym, quote(arr) + "[" + quote(n) + "]")
    case HeapArrayUpdate(arr, n, y) => stream.println(quote(arr) + "[" + quote(n) + "] = " + quote(y) + ";")
    case ref@HeapArrayAlloc (arr, n) => {
      cApp.addSystemHeader("stdlib.h")
      val malloc = "malloc (sizeof(" + remap(ref.mT) + ") * " + quote(n) + ");"
      stream.println(quote(arr) + " = " + "(" + remap(ref.mU.arrayTyp) + ") " + malloc)
    }
    case Reflect(c@HeapArraySwap(src, dst), u, es) => {
      val (src_t, dst_t) = (remap(c.mT.arrayTyp), remap(c.mU.arrayTyp))
      stream.println(src_t + " " + quote(sym) + " = " + quote(src) + ";")
      stream.println(quote(src) + " = (" + src_t + ")" + quote(dst) + ";")
      stream.println(quote(dst) + " = (" + dst_t + ")" + quote(sym) + ";")
    }
    case Reflect(c@HeapArrayCopy(src, dst, size), u, es) => {
      cApp.addSystemHeader("string.h")
      stream.println(src"memcpy($dst, $src, $size);")
    }
    case _ => super.emitNode(sym, rhs)

  }
}
