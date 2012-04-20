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

import Show._
import Unquote._

trait Quote[-A] extends (A => Value[Any])

object Quote {

  def apply[A](value: A)(implicit quote: Quote[A]): Value[Any] = quote(value)

  implicit def literalIsQuotable[A](implicit literal: Literal[A]): Quote[A] = literal.quote

  implicit object SymbolIsQuotable extends Quote[Symbol] {
    override def apply(name: Symbol) = Value(name)
  }
}
