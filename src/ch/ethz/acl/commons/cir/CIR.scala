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

package ch.ethz.acl.commons.cir

import ch.ethz.acl.commons.cir.codegen.CCodegen
import ch.ethz.acl.commons.cir.extensions._
import ch.ethz.acl.commons.extensions._
import ch.ethz.acl.commons.types.{TheTyp, TypeIR}
import ch.ethz.acl.commons.util.Debugging
import ch.ethz.acl.commons.compiler.{CompileVM, Make}
import ch.ethz.acl.commons.util.{DSLUtils, Utilities}
import scala.lms.common._
import java.io._

import org.bridj.DynamicFunction

import scala.reflect.SourceContext

trait CIR extends BaseFatExp
  with TypeIR
  with CastExp
  with CommentExp
  with Assertion
  with SwitchExp
  with ExceptionOpsExp
  with ArrayOpsExpOptExtra
  with ForLoopExpOpt

  with CNumericExpOpt
  with NumericMathOpsExp
  with EqualExpBridgeOpt
  with BooleanOpsExpOpt
  with BooleanEqualOpt
  with OrderingOpsExp
  with SaturatedNumericsOpsExpOpt
  with PrimitiveOpsExt

  with IfThenElseExpOpt
  with WhileExp with WhileMirror
  with VariablesExpOpt
  with HeapArrayExpOpt

  with DSLUtils
  with Debugging
{ self =>

  val codegen: CCodegen { val IR: self.type }

  object ImplicitLift {

    // Define methods to lift variables
    def __newVar[T:Typ](init: T)(implicit pos: SourceContext): Var[T] = var_new(unit(init))
    def __newVar[T](init: Rep[T])(implicit o: Overloaded1, mT: Typ[T], pos: SourceContext): Var[T] = var_new(init)
    def __newVar[T](init: Var[T])(implicit o: Overloaded2, mT: Typ[T], pos: SourceContext): Var[T] = var_new(init)

    // Lift all numeric variables
    implicit def numericToNumericRep[T:Numeric:Typ](x: T): Rep[T] = unit(x)
  }

  /* ================================================================================================================ */
  /* BridJ Compilation                                                                                                  */
  /* ================================================================================================================ */

  def compileBridJ[A:Typ, R:Typ](f: Exp[A] => Exp[R], makefile: Make): (DynamicFunction[R], String) = {
    val arg0 = fresh[A]
    val block = reifyEffects[R](f(arg0))
    implicit val mL = List(typ[A]).asInstanceOf[List[Typ[Any]]]
    compileBridJ(List(arg0), block, makefile)
  }

  def compileBridJ[A:Typ, B:Typ, R:Typ](f: (Exp[A], Exp[B]) => Exp[R], makefile: Make): (DynamicFunction[R], String) = {
    val arg0 = fresh[A]
    val arg1 = fresh[B]
    val block = reifyEffects[R](f(arg0, arg1))
    implicit val mL = List(typ[A], typ[B]).asInstanceOf[List[Typ[Any]]]
    compileBridJ(List(arg0, arg1), block, makefile)
  }

  def compileBridJ[A:Typ, B:Typ, C:Typ, R:Typ](f: (Exp[A], Exp[B], Exp[C]) => Exp[R], makefile: Make): (DynamicFunction[R], String) = {
    val arg0 = fresh[A]
    val arg1 = fresh[B]
    val arg2 = fresh[C]
    val block = reifyEffects[R](f(arg0, arg1, arg2))
    implicit val mL = List(typ[A], typ[B], typ[C]).asInstanceOf[List[Typ[Any]]]
    compileBridJ(List(arg0, arg1, arg2), block, makefile)
  }

  def compileBridJ[B](inputs: List[Sym[Any]], block: Block[B], makefile: Make): (DynamicFunction[B], String) = {
    val cApp = codegen.generateApplication(inputs, block, "staged")
    val codeFile = Utilities.dumpCode(cApp.generateSingleFile(), "staged")
    val mA = TheTyp.toManifest(self)(block.tp)
    val mT = inputs.map(s => TheTyp.toManifest(self)(s.tp))
    CompileVM.compileFilesBridJ[B](List(codeFile), makefile)(mA, mT)
  }


  /* ================================================================================================================ */
  /* JNI Compilation                                                                                                  */
  /* ================================================================================================================ */

  var compiledJNIFunctions = Set.empty[String]

  /**
    * Compilation of a function with 1 argument, and a default JNI Makefile
    *
    * @param f        Staged function f
    * @param funName  Name of the function
    * @tparam A       Input type of arg0
    * @tparam R       Return type
    */
  def compile[A:Typ, R:Typ](f: Exp[A] => Exp[R], inst: AnyRef, funName: String): Unit = {
    val nativeName = inst.getClass.getName.replace('.', '_') + "_" + funName
    if (!compiledJNIFunctions.contains(nativeName)) {
      val arg0 = fresh[A]
      val block = reifyEffects[R](f(arg0))
      compile(List(arg0), block, nativeName, CompileVM.getDefaultJNIMakefile())
      compiledJNIFunctions += nativeName
    }
  }

  /**
    * Compilation of a function with 2 arguments, and a default JNI Makefile
    *
    * @param f        Staged function f
    * @param funName  Name of the function
    * @tparam A       Input type of arg0
    * @tparam B       Input type of arg1
    * @tparam R       Return type
    */
  def compile[A:Typ, B:Typ, R:Typ](f: (Exp[A], Exp[B]) => Exp[R], inst: AnyRef, funName: String): Unit = {
    val nativeName = inst.getClass.getName.replace('.', '_') + "_" + funName
    if (!compiledJNIFunctions.contains(nativeName)) {
      val arg0 = fresh[A]
      val arg1 = fresh[B]
      val block = reifyEffects[R](f(arg0, arg1))
      compile(List(arg0, arg1), block, nativeName, CompileVM.getDefaultJNIMakefile())
      compiledJNIFunctions += nativeName
    }
  }

  /**
    * Compilation of a function with 3 arguments, and a default JNI Makefile
    *
    * @param f        Staged function f
    * @param funName  Name of the function
    * @tparam A       Input type of arg0
    * @tparam B       Input type of arg1
    * @tparam C       Input type of arg2
    * @tparam R       Return type
    */
  def compile[A:Typ, B:Typ, C:Typ, R:Typ](f: (Exp[A], Exp[B], Exp[C]) => Exp[R], inst: AnyRef, funName: String): Unit = {
    val nativeName = inst.getClass.getName.replace('.', '_') + "_" + funName
    if (!compiledJNIFunctions.contains(nativeName)) {
      val arg0 = fresh[A]
      val arg1 = fresh[B]
      val arg2 = fresh[C]
      val block = reifyEffects[R](f(arg0, arg1, arg2))
      compile(List(arg0, arg1, arg2), block, nativeName, CompileVM.getDefaultJNIMakefile())
      compiledJNIFunctions += nativeName
    }
  }

  /**
    * Compilation of a function with 4 arguments, and a default JNI Makefile
    *
    * @param f        Staged function f
    * @param funName  Name of the function
    * @tparam A       Input type of arg0
    * @tparam B       Input type of arg1
    * @tparam C       Input type of arg2
    * @tparam D       Input type of arg3
    * @tparam R       Return type
    */
  def compile[A:Typ, B:Typ, C:Typ, D:Typ, R:Typ](f: (Exp[A], Exp[B], Exp[C], Exp[D]) => Exp[R], inst: AnyRef, funName: String): Unit = {
    val nativeName = inst.getClass.getName.replace('.', '_') + "_" + funName
    if (!compiledJNIFunctions.contains(nativeName)) {
      val arg0 = fresh[A]
      val arg1 = fresh[B]
      val arg2 = fresh[C]
      val arg3 = fresh[D]
      val block = reifyEffects[R](f(arg0, arg1, arg2, arg3))
      compile(List(arg0, arg1, arg2, arg3), block, nativeName, CompileVM.getDefaultJNIMakefile())
      compiledJNIFunctions += nativeName
    }
  }

  /**
    * Compilation of a function with 5 arguments, and a default JNI Makefile
    *
    * @param f        Staged function f
    * @param funName  Name of the function
    * @tparam A       Input type of arg0
    * @tparam B       Input type of arg1
    * @tparam C       Input type of arg2
    * @tparam D       Input type of arg3
    * @tparam E       Input type of arg4
    * @tparam R       Return type
    */
  def compile[A:Typ, B:Typ, C:Typ, D:Typ, E:Typ, R:Typ](f: (Exp[A], Exp[B], Exp[C], Exp[D], Exp[E]) => Exp[R], inst: AnyRef, funName: String): Unit = {
    val nativeName = inst.getClass.getName.replace('.', '_') + "_" + funName
    if (!compiledJNIFunctions.contains(nativeName)) {
      val arg0 = fresh[A]
      val arg1 = fresh[B]
      val arg2 = fresh[C]
      val arg3 = fresh[D]
      val arg4 = fresh[E]
      val block = reifyEffects[R](f(arg0, arg1, arg2, arg3, arg4))
      compile(List(arg0, arg1, arg2, arg3, arg4), block, nativeName, CompileVM.getDefaultJNIMakefile())
      compiledJNIFunctions += nativeName
    }
  }

  def compile[B](inputs: List[Sym[Any]], block: Block[B], funName: String, makefile: Make): Unit = {
    val cApp = codegen.generateJNIApplication(inputs, block, funName)
    val codeFile = Utilities.dumpCode(cApp.generateSingleFile(), funName)
    CompileVM.compileFilesJNI(List(codeFile), funName, makefile)
  }

  def emitBlock[B] (
    inputs: List[Sym[Any]],
    block: Block[B],
    stream: PrintWriter,
    funcName: String = "staged"
  ): Unit = {
    val app = codegen.generateApplication[B](inputs, block, funcName)
    stream.println(app.generateSingleFile())
    stream.close()
  }

  def dumpCode[B] (name: String, inputs: List[Sym[Any]], block: Block[B]): Unit = {
    emitBlock(inputs, block, new PrintWriter(name + ".c"))
  }


}
