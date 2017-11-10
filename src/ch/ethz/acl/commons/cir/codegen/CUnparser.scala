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
  *  Copyright (C) 2017 Alen Stojanov  (astojanov@inf.ethz.ch)
  *                2013 Georg Ofenbeck (ofenbeck@inf.ethz.ch)
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

import java.io._

import ch.ethz.acl.passera.unsigned.{UByte, UInt, ULong, UShort}
import ch.ethz.acl.commons.types.TheTyp

import scala.lms.common._

trait CUnparser extends scala.lms.internal.CCodegen  {

  val IR: BaseExp
  import IR._

  protected var cApp: CApplication = null

  override def remap[A](m: Typ[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    } else if (m.erasure.isArray) {
      remap(m.typeArguments.head) + "*"
    } else m match {
      case _ if m <:< TheTyp.toTyp[IR.type, Double](IR)  => "double"
      case _ if m <:< TheTyp.toTyp[IR.type, Float](IR)   => "float"
      case _ if m <:< TheTyp.toTyp[IR.type, Char](IR)    => "char"
      case _ if m <:< TheTyp.toTyp[IR.type, Boolean](IR) => "bool"
      case _ if m <:< TheTyp.toTyp[IR.type, Long](IR)    => "int64_t"
      case _ if m <:< TheTyp.toTyp[IR.type, Int](IR)     => "int32_t"
      case _ if m <:< TheTyp.toTyp[IR.type, Short](IR)   => "int16_t"
      case _ if m <:< TheTyp.toTyp[IR.type, Byte](IR)    => "int8_t"
      case _ if m <:< TheTyp.toTyp[IR.type, ULong](IR)   => "uint64_t"
      case _ if m <:< TheTyp.toTyp[IR.type, UInt](IR)    => "uint32_t"
      case _ if m <:< TheTyp.toTyp[IR.type, UShort](IR)  => "uint16_t"
      case _ if m <:< TheTyp.toTyp[IR.type, UByte](IR)   => "uint8_t"
      case _ => super.remap(m)
    }
  }

  override def remapWithRef[A](m: Typ[A]): String = remap(m) + " "

  override def isPrimitiveType(tpe: String) : Boolean = {
    tpe match {
      case "double"   | "float"    | "char"     | "bool"    => true
      case "int64_t"  | "int32_t"  | "int16_t"  | "int8_t"  => true
      case "uint64_t" | "uint32_t" | "uint16_t" | "uint8_t" => true
      case _ => super.isPrimitiveType(tpe)
    }
  }

  def emitGlobalNodes (block: Block[Any]): Unit = {}

  override def emitSource[A, B](
    f: Exp[A] => Exp[B],
    functionName: String,
    out: PrintWriter
  )(implicit mA: Typ[A], mB: Typ[B]): List[(Sym[Any], Any)] = {
    val func = (in: List[Exp[Any]]) => f(in(0).asInstanceOf[Exp[A]])
    implicit val mList = List(mA).asInstanceOf[List[Typ[Any]]]
    emitSource(func, functionName, out)
    Nil
  }

  def generateApplication [B] (
    syms: List[Sym[Any]], block: Block[B], fName: String
  ): CApplication = {

    cApp = new CApplication(fName)
    cApp.addSystemHeader("stdint.h")
    cApp.addSystemHeader("stdbool.h")

    val stringOutput = new StringWriter()
    val stringWriter = new PrintWriter(stringOutput)

    withStream(stringWriter) {
      val returnType = remap(getBlockResult(block).tp)
      val arguments = syms.map(m => remap(m.tp) + " " +  quote(m)).mkString(", ")
      emitGlobalNodes (block)
      stream.println(s"$returnType $fName ($arguments) {")
      emitBlock(block)
      val returnValue = getBlockResult(block)
      if ( !(returnValue.tp <:< typ[Unit]) )
        stream.println("return " + quote(returnValue) + ";")
      stream.println("}")
    }
    cApp.setGeneratedCode(stringOutput.toString)
    cApp
  }

  def emitSource[B](
    f: List[Exp[Any]] => Exp[B],
    fName: String,
    out: PrintWriter,
    beautify: Boolean = true
  )(implicit mList: List[Typ[Any]], mB: Typ[B]): Unit = {
    var p = List.empty[Exp[Any]]
    mList.foreach( m => { p = p :+ fresh(m) } )
    var q = reifyBlock[B](f(p))
    val (x, y) = (p.asInstanceOf[List[IR.Sym[Any]]], q)
    val app = generateApplication(x, y, fName)
    out.println(app.generateSingleFile())
  }

  override def quote(x: Exp[Any]) : String = x match {
    case Const(c: Char) => "((char)(" + c.toInt + "))"
    case Const(s: String) => "\"" + s + "\""
    case s@Sym(n) if s.tp <:< ManifestTyp(manifest[Int]) => "i"+n
    case _ => super.quote(x)
  }
}
