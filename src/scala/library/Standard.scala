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

trait Standard {
  this: Environment =>

  define('list) = Lambda.BuiltIn("value*") {
    values: HList =>
      Value(values)
  }

  define('cons) = Lambda.BuiltIn("value", "list") {
    values: HList =>
      Value(values(0) +: values(1).as[HList])
  }
  define('car) = Lambda.BuiltIn("list") {
    values: HList =>
      values.head.as[HList].head
  }
  define('cdr) = Lambda.BuiltIn("list") {
    values: HList =>
      Value(values.head.as[HList] drop 1)
  }

  define(Symbol("null?")) = Lambda.BuiltIn("list") {
    values: HList =>
      Value(values.head.as[HList].isEmpty)
  }
  define('length) = Lambda.BuiltIn("list") {
    values: HList =>
      Value(Number(values.head.as[HList].length))
  }
}
