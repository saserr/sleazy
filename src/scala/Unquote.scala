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

trait Unquote[-A] extends (A => Expression[Any])

object Unquote {

  def apply[A](value: A)(implicit unquote: Unquote[A]): Expression[Any] = unquote(value)

  implicit def literalIsUnquotable[A](implicit literal: Literal[A]): Unquote[A] = literal.unquote

  implicit object SymbolIsUnquotable extends Unquote[Symbol] {
    override def apply(name: Symbol) = Expression[Symbol](name)
  }
}
