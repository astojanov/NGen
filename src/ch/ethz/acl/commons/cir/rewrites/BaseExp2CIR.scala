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

import ch.ethz.acl.commons.cir.CIR
import ch.ethz.acl.commons.cir.extensions.{ArrayOpsExpOptExtra, CommentExp}
import ch.ethz.acl.commons.extensions.{ForLoopExp, ForLoopFatExp}
import ch.ethz.acl.commons.types.TheTyp
import ch.ethz.acl.commons.util.DSLUtils

import scala.collection.immutable.HashMap
import scala.lms.common.{BaseExp, VariablesExpOpt}

trait BaseExp2CIR extends IR2C {

  val IR: BaseExp
  val C : VariablesExpOpt

  /**
   * Define a mapping between IR variable to CIR variable.
   *
   * Depending on the definition, we assume that a certain IR variable will be
   * mapped to one variable into CIR, or a variable expression into CIR.
   */
  var mapIRExp2CIRVar = HashMap.empty[IR.Exp[Any], () => C.Var[Any]]


  /**
   * For each IR variable, add auxiliary mapping referenced by a string in form
   * of a metadata. The IR variable, including its string representation of the metadata
   * index corresponds to a CIR variable.
   */
  var mapIRExp2CIRVarMeta = HashMap.empty[String, HashMap[IR.Exp[Any], () => C.Var[Any]]]

  /**
   * Define a mapping between a given IR variable, and its metadata descriptor.
   *
   * @param s     Input IR symbol / expression
   * @param meta  Metadata string descriptor
   * @param exp   Mapped expression
   * @tparam T    The type of the input IR descriptor
   */
  def setCVarMeta [T](s: IR.Exp[T], meta: String, exp: () => C.Var[T]): Unit = {
    if (!hasCExpMeta(s, meta)) {
      var metaMap = mapIRExp2CIRVarMeta.getOrElse(meta, HashMap.empty[IR.Exp[Any], () => C.Var[Any]])
      metaMap += (s -> exp.asInstanceOf[() => C.Var[Any]])
      mapIRExp2CIRVarMeta += (meta -> metaMap)
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
  def hasCVarMeta[T](s: IR.Exp[T], meta: String): Boolean = {
    if (mapIRExp2CIRVarMeta.contains(meta)) {
      mapIRExp2CIRVarMeta(meta).contains(s)
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
  def getCVarMeta[T](exp: IR.Exp[Any], meta: String): C.Var[T] = {
    Predef.assert(hasCVarMeta(exp, meta), s"Meta $meta is not defined for $exp")
    val cSym = mapIRExp2CIRVarMeta(meta)(exp)
    cSym().asInstanceOf[C.Var[T]]
  }

}
