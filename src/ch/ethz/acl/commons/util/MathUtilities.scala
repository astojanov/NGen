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

package ch.ethz.acl.commons.util

import scala.math._

object MathUtilities {

  def Gcd[A](x: A, y: A)(implicit integral: Integral[A]) : A =
  {
    val t = scala.math.BigInt(integral.toLong(x))
    val res = t.gcd(scala.math.BigInt(integral.toLong(y)))
    x match {
      case _:Int => res.toInt.asInstanceOf[A]
      case _:Long => res.toLong.asInstanceOf[A]
      case _:Short => res.toShort.asInstanceOf[A]
    }
  }

  def NormalizeRational[A](x: A, y: A)(implicit integral: Integral[A]) : (A, A) = {
    val gcd = Gcd(x, y)
    (integral.quot(x, gcd), integral.quot(y, gcd))
  }

  def normalize_2pi_shift (xin: Double, yin: Double): (Double,Double) = {
    var (x, y) = NormalizeRational(Math.round(xin), Math.round(yin))
    if ( (x / y) < 0 ) {
      val t:Long = Math.ceil( x.toDouble / y.toDouble / (-2.0) ).toLong
      x = x + 2 * t * y
    } else {
      val t = (Math.floor( (x.toDouble - 2 * y.toDouble) / y.toDouble / 2.0 ) + 1 ).toLong;
      x = x - 2 * y * t;
    }
    val (xp, yp) = NormalizeRational(x,y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_shift (xin: Double, yin: Double): (Double,Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(2 * x - y, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_pi_over2_reflection(xin: Double, yin: Double): (Double,Double) = {
    val (x, y) = (Math.round(xin), Math.round(yin))
    val (xp, yp) = NormalizeRational(y - 2 * x, 2 * y)
    (xp.toDouble, yp.toDouble)
  }

  def normalize_trig (sign: Int, trig: String, x: Double, y: Double): (Int, String, Double, Double, Double) = {
    // normalization in 2Pi, achieving: 0 <= xn / yn <= 2
    val (xn,yn) = normalize_2pi_shift(x,y)
    if ( xn > yn ) {
      trig match {
        case "sin" => normalize_trig ( sign * (-1), "sin", xn - yn, yn )
        case "cos" => normalize_trig ( sign * (-1), "cos", xn - yn, yn )
      }
    } else if (xn == yn) {
      trig match {
        case "sin" => ( sign, "sin", xn, yn, sign * (+0.0) )
        case "cos" => ( sign, "cos", xn, yn, sign * (-1.0) )
      }
    } else {
      if ( xn > yn/2 ) {
        // normalization in Pi, achieving 0 <= xn / yn <= 1/2
        val (xp, yp) = normalize_pi_over2_shift(xn, yn)
        trig match {
          case "sin" => normalize_trig ( sign * (+1), "cos", xp, yp )
          case "cos" => normalize_trig ( sign * (-1), "sin", xp, yp )
        }
      } else if ( xn == yn / 2 ) {
        trig match {
          case "sin" => ( sign, "sin", xn, yn, sign * (+1.0) )
          case "cos" => ( sign, "cos", xn, yn, sign * (+0.0) )
        }
      } else {
        // now reflect in Pi / 2, and make sure that 0 <= xn / yn <= 1/4
        if ( xn > yn / 4 ) {
          val (xp, yp) = normalize_pi_over2_reflection(xn, yn)
          trig match {
            case "sin" => ( sign, "cos", xp, yp, Double.MaxValue )
            case "cos" => ( sign, "sin", xp, yp, Double.MaxValue )
          }
        } else if ( xn == yn / 4) {
          ( sign, "cos", 1.0, 4.0, Double.MaxValue )
        } else {
          if ( xn == 0.0 ) {
            trig match {
              case "sin" => ( sign, "sin", xn, yn, sign * (+0.0) )
              case "cos" => ( sign, "cos", xn, yn, sign * (+1.0) )
            }
          } else {
            trig match {
              case "sin" => ( sign, "sin", xn, yn, Double.MaxValue )
              case "cos" => ( sign, "cos", xn, yn, Double.MaxValue )
            }
          }
        }
      }
    }
  }

  trait TrigonometryOps [T] {
    /**
     * Performs normalization of sin and cos functions. The sin and cos function are in the form sin (rational * PI).
     * The function normalizes the value of the rational (which is x / y) to a value [0 .. PI / 4], and the result is
     * either a cos of a sin function. The function normalizes both staged and non-staged version of sin and cos.
     *
     * @param f Starting function. Either "sin" or "cos"
     * @param x Dividend component of the rational number
     * @param y Divisor component of the rational number
     * @return  Normalized sin or cos function (staged or non-staged depends on T)
     */
    private def valueSinOrCos (f: String, x: Double, y: Double) : T = {
      val (sign, trig, xn, yn, value) = normalize_trig(1, f, x, y)
      if ( !value.equals(scala.Double.MaxValue) ) {
        fromDouble(value)
      } else {
        trig match {
          case "sin" => (xn, yn) match {
            case (1.0, 6.0) => fromDouble (sign * 0.5)
            case _ => fromDouble(sign * Math.sin(xn * Math.PI/yn))
          }
          case "cos" => fromDouble ( sign * Math.cos(xn * Math.PI/yn) )
        }
      }
    }

    def SinPi(x : Double, y: Double) : T = valueSinOrCos("sin", x, y)
    def CosPi(x : Double, y: Double) : T = valueSinOrCos("cos", x, y)

    def fromDouble(x: Double)  : T
  }
//
//
}
//
//
//
//
//
