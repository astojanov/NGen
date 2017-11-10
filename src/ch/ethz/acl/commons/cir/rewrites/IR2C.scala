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

package ch.ethz.acl.commons.cir.rewrites

import ch.ethz.acl.commons.types.TheTyp
import scala.collection.immutable.HashMap
import scala.lms.common.BaseExp

trait IR2C {

  val IR: BaseExp
  val C : BaseExp

  /**
    * Whenever a certain IR is being transformed into CIR code, it
    * must be transformed in a form of a function. That means that
    * the output function must have input / output arguments.
    *
    * In most cases, the input and output parameters of the IR DSL
    * will correspond to the input and output parameters of the
    * CIR DSL. However, it could happen that multiple IR arguments
    * map to a single CIR argument, or vise versa. However, since
    * CIR is a lower DSL, it is more likely that an IR argument
    * maps to several CIR arguments.
    *
    * To describe this mapping, we use the abstract class bellow,
    * which represents mapping between the IR argument to multiple
    * CIR arguments. When a function is generated in the C world,
    * we expect that the input and output arguments reside inside
    * the header of the function serialized by their order. To be
    * able to deal with this arguments, we need to know their types
    * as well as the order to serialize  and deserialize while
    * building the CIR. CFunArguments achieves this with such that:
    *
    * - the serialize method converts IR arguments into an ordered
    *   list of Manifests which represent the types of the arguments.
    *
    * - the deserialize method, gets a list of input arguments, and
    *   extracts the corresponding arguments in serialized order,
    *   performs deserialization, and returns the list of arguments
    *   that have not yet been used.
    */
  abstract class CFunArguments () {

    /**
      * Describes whether the following arguments are return
      * arguments
      *
      * @return True or false, whether this argument is return or not
      */
    def isReturn: Boolean

    /**
      * Convert the IR argument into a list of CIR manifest that
      * reflect the type of each argument.
      *
      * @return List of manifest reflecting the type of the argument
      */
    def serialize (): List[Manifest[Any]]

    /**
      * For a given list of CIR input / output arguments, obtain
      * the corresponding CIR arguments, deserialize them, and return
      * the list of unused arguments.
      *
      * @param l List of input / output arguments
      * @return  List of input / output arguments that do not correspond
      *          to the CFunArguments deserializer.
      */
    def deserialize (l: List[C.Rep[Any]]) : List[C.Rep[Any]]
  }

  /**
    * Define a mapping between IR expressions to CIR expressions.
    *
    * Depending on the definition, we assume that a certain IR expression will be
    * mapped to one or several expressions in into CIR.
    */
  var mapIRExp2CIRExp = HashMap.empty[IR.Exp[Any], () => C.Exp[Any]]

  /**
    * For each IR expression / symbol, add auxiliary mapping referenced by a string in form
    * of a metadata. The IR symbol, including its string representation of the metadata
    * index corresponds to a CIR expression / symbol.
    */
  var mapIRExp2CIRExpMeta = HashMap.empty[String, HashMap[IR.Exp[Any], () => C.Exp[Any]]]

  /**
    * When an CIR expression is being defined for a given symbol s, add it to the map (s -> exp).
    *
    * @param s   MIR symbol.
    * @param exp CIR expression that corresponds to the given symbol.
    * @tparam T  The type of the symbol.
    */
  def setCExp [T](s: IR.Exp[T], exp: () => C.Exp[T]): Unit = {
    if (!hasCExp(s))
      mapIRExp2CIRExp += (s -> exp.asInstanceOf[() => C.Exp[Any]])
  }

  /**
    * Checks whether a mapping is being defined for a given Expression
    *
    * @param s   IR expression symbol / constant.
    * @tparam T  The expression symbol / constant type.
    * @return    True or False, depending on whether the input is mapped
    */
  def hasCExp[T](s: IR.Exp[T]): Boolean = {
    mapIRExp2CIRExp.contains(s)
  }

  /**
    * For a given IR symbol / constant, return the corresponding symbol / constant in CIR. Not that the symbols
    * will be extracted from the mapIRExp2CIRExp and as result, symbol indices may not correspond.
    *
    * @param exp IR expression symbol / constant.
    * @tparam T  The expression symbol / constant type.
    * @return    Corresponding CIR symbol / constant
    */
  def getCExp[T](exp: IR.Exp[Any]): C.Exp[T] = exp match {
    case s@IR.Sym(_) => {
      if (mapIRExp2CIRExp.contains(s)) {
        val cSym = mapIRExp2CIRExp(s)
        cSym ().asInstanceOf[C.Exp[T]]
      } else {
        val cSym = C.fresh(typC(exp.tp))
        mapIRExp2CIRExp += (s -> (() => cSym))
        cSym.asInstanceOf[C.Exp[T]]
      }
    }
    case IR.Const(x) => {
      C.Const(x.asInstanceOf[T])(typC(exp.tp).asInstanceOf[C.Typ[T]])
    }
  }

  /**
    * Define a mapping between a given IR symbol / expression, and its metadata descriptor.
    *
    * @param s     Input IR symbol / expression
    * @param meta  Metadata string descriptor
    * @param exp   Mapped expression
    * @tparam T    The type of the input IR descriptor
    */
  def setCExpMeta [T](s: IR.Exp[T], meta: String, exp: () => C.Exp[T]): Unit = {
    if (!hasCExpMeta(s, meta)) {
      var metaMap = mapIRExp2CIRExpMeta.getOrElse(meta, HashMap.empty[IR.Exp[Any], () => C.Exp[Any]])
      metaMap += (s -> exp.asInstanceOf[() => C.Exp[Any]])
      mapIRExp2CIRExpMeta += (meta -> metaMap)
    }
  }

  /**
    * Check whether a mapping for a given IR expression / symbol and given metadata string
    * exists
    *
    * @param s     IR expression / symbol
    * @param meta  Metadata String expression
    * @tparam T    The output type of the corresponding CIR symbol / expression
    * @return      The corresponding CIR symbol / expression
    */
  def hasCExpMeta[T](s: IR.Exp[T], meta: String): Boolean = {
    if (mapIRExp2CIRExpMeta.contains(meta)) {
      mapIRExp2CIRExpMeta(meta).contains(s)
    } else false
  }

  /**
    * For a given IR symbol / expression, and its metadata descriptor, return the
    * corresponding CIR symbol / expression. Note that the getCExpMeta will return
    * a fresh symbol when a mapping is not defined.
    *
    * @param exp   Input IR expression / symbol
    * @param meta  Metadata descriptor
    * @tparam T    Type of the corresponding CIR symbol / expression
    * @return      Corresponding CIR symbol / expression
    */
  def getCExpMeta[T](exp: IR.Exp[Any], meta: String): C.Exp[T] = {
    Predef.assert(hasCExpMeta(exp, meta), s"Meta $meta is not defined for $exp")
    val cSym = mapIRExp2CIRExpMeta(meta)(exp)
    cSym().asInstanceOf[C.Exp[T]]
  }

  def typC[T:IR.Typ]: C.Typ[T] = {
    val m = TheTyp.toManifest[T](IR)(IR.typ[T])
    TheTyp.toTyp[C.type, T](C)(m)
  }

}
