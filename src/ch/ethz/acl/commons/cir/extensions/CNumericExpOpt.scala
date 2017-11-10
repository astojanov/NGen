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

import scala.lms.common._
import scala.reflect.SourceContext

trait CNumericExpOpt extends NumericOpsExpOpt with PrimitiveOpsExpOpt {

  override def numeric_plus[T:Numeric:Typ](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
    // 2 constants with NumericPluses
    case (Const(c1), Def(NumericPlus(x, Const(c2))))  => numeric_plus(x, Const(implicitly[Numeric[T]].plus(c1, c2)))
    case (Const(c1), Def(NumericPlus(Const(c2), x)))  => numeric_plus(x, Const(implicitly[Numeric[T]].plus(c1, c2)))
    case (Def(NumericPlus(x, Const(c2))), Const(c1))  => numeric_plus(x, Const(implicitly[Numeric[T]].plus(c1, c2)))
    case (Def(NumericPlus(Const(c2), x)), Const(c1))  => numeric_plus(x, Const(implicitly[Numeric[T]].plus(c1, c2)))
    // 2 constants with NumericMinus
    case (Const(c1), Def(NumericMinus(x, Const(c2)))) => numeric_plus(x, Const(implicitly[Numeric[T]].minus(c1, c2)))
    case (Const(c1), Def(NumericMinus(Const(c2), x))) => numeric_minus(Const(implicitly[Numeric[T]].plus(c1, c2)), x)
    case (Def(NumericMinus(x, Const(c2))), Const(c1)) => numeric_plus(x, Const(implicitly[Numeric[T]].minus(c1, c2)))
    case (Def(NumericMinus(Const(c2), x)), Const(c1)) => numeric_minus(Const(implicitly[Numeric[T]].plus(c1, c2)), x)
    // take care of the rest
    case _ => super.numeric_plus(lhs,rhs)
  }

  override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext) : Exp[Int] = (lhs,rhs) match {
    // 2 constants with IntPluses
    case (Const(c1), Def(IntPlus(x, Const(c2))))  => int_plus(x, Const(c1 + c2))
    case (Const(c1), Def(IntPlus(Const(c2), x)))  => int_plus(x, Const(c1 + c2))
    case (Def(IntPlus(x, Const(c2))), Const(c1))  => int_plus(x, Const(c1 + c2))
    case (Def(IntPlus(Const(c2), x)), Const(c1))  => int_plus(x, Const(c1 + c2))
    // 2 constants with IntMinus
    case (Const(c1), Def(IntMinus(x, Const(c2)))) => int_plus(x, Const(c1 - c2))
    case (Const(c1), Def(IntMinus(Const(c2), x))) => int_minus(Const(c1 + c2), x)
    case (Def(IntMinus(x, Const(c2))), Const(c1)) => int_plus(x, Const(c1 - c2))
    case (Def(IntMinus(Const(c2), x)), Const(c1)) => int_minus(Const(c1 + c2), x)
    // take care of the rest
    case _ => super.int_plus(lhs,rhs)
  }
}