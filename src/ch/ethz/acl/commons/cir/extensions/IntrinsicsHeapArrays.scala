package ch.ethz.acl.commons.cir.extensions

import ch.ethz.acl.intrinsics.IntrinsicsBase

trait IntrinsicsHeapArrays extends IntrinsicsBase with HeapArrayBase {

  implicit object HeapArrayContainerExp extends Container[HeapArray]
  {
    def write[A:Typ, T:Typ](c: Exp[HeapArray[T]]*)(writeObject: Def[A]): Exp[A] = {
      reflectWrite(c.toArray:_*)(writeObject)
    }

    def read[A:Typ, T:Typ](c: Exp[HeapArray[T]]*)(readObject: Def[A]): Exp[A] = {
      toAtom(readObject)
    }

    def apply[T:Typ](c: Exp[HeapArray[T]], i: Exp[Int]): Exp[T] = {
      heap_array_apply(c, i)
    }

    def update[T:Typ](c: Exp[HeapArray[T]], i: Exp[Int], elem: Exp[T]): Exp[Unit] = {
      heap_array_update(c, i, elem)
    }

    def applyTransformer[A](x: Exp[A], f: Transformer): Exp[A] = {
      f(x)
    }

    def newInstance[T:Typ](size: Exp[Int]): Exp[HeapArray[T]] = {
      val inst = heap_array_new[T]()
      heap_array_alloc[T, T](inst, size)
      inst
    }
  }

}
