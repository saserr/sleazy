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
package form

trait Special[+A] extends Operation[A] {

  def syntax: List[Symbol]

  override protected lazy val show = Show(this)(Special.IsShowable)
  override protected val `type` = "Special"
}

object Special {

  class Helper(syntax: String*) {
    def apply[A](f: (List[Expression[Any]], Environment) => Result[A]): Special[A] =
      new Special[A] {

        override val syntax = Helper.this.syntax.toList map {Symbol(_)}

        override def apply(operands: List[Expression[Any]], executedIn: Environment) =
          f(operands, executedIn)
      }
  }

  def apply(syntax: String*): Helper = new Helper(syntax: _*)

  implicit object IsShowable extends Show[Special[Any]] {
    override def apply(special: Special[Any]) = Show(special.syntax)
  }
}
