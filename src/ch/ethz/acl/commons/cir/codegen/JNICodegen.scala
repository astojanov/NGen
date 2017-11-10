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

import java.io.{PrintWriter, StringWriter}

import ch.ethz.acl.commons.types.TheTyp
import ch.ethz.acl.passera.unsigned.{UByte, UInt, ULong, UShort}

trait JNICodegen extends CUnparser {

  import IR._

  def remapJNI[T](m: Typ[T]): String = if (m.erasure.isArray) {
    val h = m.typeArguments.head
    h match {
      case _ if h <:< TheTyp.toTyp[IR.type, Double](IR)  => "jdoubleArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Float](IR)   => "jfloatArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Char](IR)    => "jcharArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Boolean](IR) => "jbooleanArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Long](IR)    => "jlongArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Int](IR)     => "jintArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Short](IR)   => "jshortArray"
      case _ if h <:< TheTyp.toTyp[IR.type, Byte] (IR)   => "jbyteArray"
      case _ if h <:< TheTyp.toTyp[IR.type, ULong](IR)   => "jlongArray"
      case _ if h <:< TheTyp.toTyp[IR.type, UInt](IR)    => "jintArray"
      case _ if h <:< TheTyp.toTyp[IR.type, UShort](IR)  => "jshortArray"
      case _ if h <:< TheTyp.toTyp[IR.type, UByte](IR)   => "jbyteArray"
      case _ => throw new RuntimeException("JNI can only handle arrays of primitive types for now")
    }
  } else m match {
    case _ if m <:< TheTyp.toTyp[IR.type, Unit](IR)    => "void"
    case _ if m <:< TheTyp.toTyp[IR.type, Double](IR)  => "jdouble"
    case _ if m <:< TheTyp.toTyp[IR.type, Float](IR)   => "jfloat"
    case _ if m <:< TheTyp.toTyp[IR.type, Char](IR)    => "jchar"
    case _ if m <:< TheTyp.toTyp[IR.type, Boolean](IR) => "jboolean"
    case _ if m <:< TheTyp.toTyp[IR.type, Long](IR)    => "jlong"
    case _ if m <:< TheTyp.toTyp[IR.type, Int](IR)     => "jint"
    case _ if m <:< TheTyp.toTyp[IR.type, Short](IR)   => "jshort"
    case _ if m <:< TheTyp.toTyp[IR.type, Byte] (IR)   => "jbyte"
    case _ if m <:< TheTyp.toTyp[IR.type, ULong](IR)   => "jlong"
    case _ if m <:< TheTyp.toTyp[IR.type, UInt](IR)    => "jint"
    case _ if m <:< TheTyp.toTyp[IR.type, UShort](IR)  => "jshort"
    case _ if m <:< TheTyp.toTyp[IR.type, UByte](IR)   => "jbyte"
    case _ => throw new RuntimeException("JNI can only handle primitive types for now")
  }

  def quoteJNI (e: Exp[Any]): String = "jni_" + quote(e)

  def getJNIPrimitives (arg: Sym[Any]): String = {
    val typ  = remap(arg.tp)
    val sym  = quote(arg)
    val jsym = quoteJNI(arg)
    if (arg.tp.erasure.isArray) {
      s"""
        |jboolean isCopy_$sym;
        |// jsize len_$sym  = (*env)->GetArrayLength(env, $jsym);
        |$typ $sym = ($typ) (*env)->GetPrimitiveArrayCritical(env, $jsym, &isCopy_$sym);
      """.stripMargin
    } else {
      s"$typ $sym = ($typ) $jsym;"
    }
  }.trim

  def releaseJNIPrimitives (arg: Sym[Any]): String = {
    if (arg.tp.erasure.isArray) {
      s"""
         |(*env)->ReleasePrimitiveArrayCritical(env, ${ quoteJNI(arg) }, (jbyte *) ${ quote(arg) }, 0);
      """.stripMargin.trim
    } else ""
  }

  def generateJNIApplication [B] (
    syms: List[Sym[Any]], block: Block[B], fName: String
  ): CApplication = {

    cApp = new CApplication(fName)
    cApp.addSystemHeader("stdint.h")
    cApp.addSystemHeader("stdbool.h")
    cApp.addSystemHeader("jni.h")

    val stringOutput = new StringWriter()
    val stringWriter = new PrintWriter(stringOutput)

    withStream(stringWriter) {
      val returnType = remapJNI(getBlockResult(block).tp)
      val arguments = syms.map(m => remapJNI(m.tp) + " " +  quoteJNI(m)).mkString(", ")
      emitGlobalNodes (block)
      stream.println(s"JNIEXPORT $returnType JNICALL Java_$fName (JNIEnv *env, jobject obj, $arguments) {")
      stream.println((syms map getJNIPrimitives).mkString("\n"))
      emitBlock(block)
      stream.println((syms map releaseJNIPrimitives).mkString("\n"))
      val returnValue = getBlockResult(block)
      if ( !(returnValue.tp <:< typ[Unit]) )
        stream.println("return (" + remapJNI(returnValue.tp) + ") " + quote(returnValue) + ";")
      stream.println("}")
    }
    cApp.setGeneratedCode(stringOutput.toString)
    cApp
  }

}
