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

trait Logic {
  this: Environment =>

  define('not) = Lambda.BuiltIn("boolean") {
    values: HList =>
      Value(values.head === `#f`)
  }
  define('and) = Lambda.BuiltIn("boolean*") {
    values: HList =>
      Value(values forall {_ /== `#f`})
  }
  define('or) = Lambda.BuiltIn("boolean*") {
    values: HList =>
      Value(values exists {_ /== `#f`})
  }
}
