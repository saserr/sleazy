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

trait Standard {
  this: Environment =>

  define('list) = Lambda.BuiltIn("value*") {
    values: HList =>
      Result(values)
  }

  define('cons) = Lambda.BuiltIn("value", "list") {
    values: HList =>
      Check(Arguments(values).length =:= 2) {
        Result(values(1).as[HList] map {values(0) +: _})
      }
  }
  define('car) = Lambda.BuiltIn("list") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        values.head.as[HList] flatMap {
          list => list.headOption or s"${Type(list)} is empty"
        }
      }
  }
  define('cdr) = Lambda.BuiltIn("list") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.as[HList] map {_ drop 1})
      }
  }

  define(Symbol("null?")) = Lambda.BuiltIn("list") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.as[HList] map {_.isEmpty})
      }
  }
  define('length) = Lambda.BuiltIn("list") {
    values: HList =>
      Check(Arguments(values).length =:= 1) {
        Result(values.head.as[HList] map {list => Number(list.length)})
      }
  }
}
