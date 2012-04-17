/*
 * Copyright 2012 Sanjin Sehic
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.saserr.sleazy
package library

import java.math.MathContext.DECIMAL128

import Number.{one, zero}
import form.Lambda

trait Math {
  this: Environment =>

  define(Symbol("+")) = Lambda.BuiltIn("number*") {
    numbers: Seq[Number] =>
      Value(numbers.foldLeft(zero) {_ + _})
  }
  define(Symbol("-")) = Lambda.BuiltIn("number+") {
    numbers: Seq[Number] =>
      Value(
        if (numbers.length === 1) zero - numbers(0)
        else numbers reduce {_ - _}
      )
  }
  define(Symbol("*")) = Lambda.BuiltIn("number*") {
    numbers: Seq[Number] =>
      Value(numbers.foldLeft(one) {_ * _})
  }
  define(Symbol("/")) = Lambda.BuiltIn("number+") {
    numbers: Seq[Number] =>
      if (numbers exists {_ === zero}) fail("division by zero")
      else if (numbers.length === 1)
        try Value(one / numbers(0))
        catch {
          case _: ArithmeticException =>
            Value(one(DECIMAL128) / numbers(0))
        }
      else
        try Value(numbers reduce {_ / _})
        catch {
          case _: ArithmeticException =>
            val head :: tail = numbers
            Value(tail.foldLeft(head(DECIMAL128)) {_ / _})
        }
  }
}
