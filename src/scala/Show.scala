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

trait Show[-A] extends (A => String)

object Show {

  def apply[A](value: A)(implicit show: Show[A]): String = show(value)

  implicit def seqIsShowable[A: Show]: Show[Seq[A]] = new Show[Seq[A]] {
    override def apply(seq: Seq[A]) = seq map Show[A] mkString ("(", " ", ")")
  }

  implicit object SymbolIsShowable extends Show[Symbol] {
    override def apply(value: Symbol) = value.name
  }
}
