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

import form.Lambda
import util.Check
import util.Check.Arguments

trait Comparisons {
  this: Environment.Builder =>

  define(Symbol("equal?")) = Lambda.BuiltIn("x", "y") {
    values: HList =>
      Check(Arguments(values).length =:= 2) {
        Result(values(0) === values(1))
      }
  }
  define(Symbol("eq?")) = Lambda.BuiltIn("x", "y") {
    values: HList =>
      Check(Arguments(values).length =:= 2) {
        Result(values(0) sameAs values(1))
      }
  }

  define(Symbol("boolean?")) = Lambda.BuiltIn("x") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.is[Boolean])
      }
  }
  define(Symbol("number?")) = Lambda.BuiltIn("x") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.is[Number])
      }
  }
  define(Symbol("char?")) = Lambda.BuiltIn("x") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.is[Char])
      }
  }
  define(Symbol("string?")) = Lambda.BuiltIn("x") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.is[String])
      }
  }
  define(Symbol("symbol?")) = Lambda.BuiltIn("x") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.is[Symbol])
      }
  }
  define(Symbol("list?")) = Lambda.BuiltIn("x") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.is[HList])
      }
  }

  define(Symbol("<")) = Lambda.BuiltIn("number*") {
    numbers: Seq[Number] =>
      Result(numbers zip (numbers drop 1) forall {case (a, b) => a < b})
  }
  define(Symbol("<=")) = Lambda.BuiltIn("number*") {
    numbers: Seq[Number] =>
      Result(numbers zip (numbers drop 1) forall {case (a, b) => a <= b})
  }
  define(Symbol(">")) = Lambda.BuiltIn("number*") {
    numbers: Seq[Number] =>
      Result(numbers zip (numbers drop 1) forall {case (a, b) => a > b})
  }
  define(Symbol(">=")) = Lambda.BuiltIn("number*") {
    numbers: Seq[Number] =>
      Result(numbers zip (numbers drop 1) forall {case (a, b) => a >= b})
  }
}
