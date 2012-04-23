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
package util

trait Check extends (() => Boolean) {
  def error: String
}

object Check {

  def apply[A](check: Check)(f: => Result[A]): Result[A] =
    if (check()) f else fail(check.error)

  class Arguments(values: Seq[Any]) {
    object length {

      def =:=(number: Int): Check = new Check {
        override def apply() = values.length === number
        override val error = s"${argumentsAre(number)} expected, but ${argumentsWere(values.length)} given"
      }

      def >=(minimum: Int): Check = new Check {
        override def apply() = values.length >= minimum
        override val error = s"at least ${argumentsAre(minimum)} expected, but ${argumentsWere(values.length)} given"
      }

      def between(minimum: Int, maximum: Int): Check = new Check {
        override def apply() = (values.length >= minimum) && (values.length <= maximum)
        override val error = s"between ${number(minimum)} and ${argumentsAre(maximum)} expected, but ${argumentsWere(values.length)} given"
      }

      private def argumentsAre(n: Int): String = arguments(n) + (if (n === 1) " is" else " are")
      private def argumentsWere(n: Int): String = arguments(n) + (if (n === 1) " was" else " were")
      private def arguments(n: Int): String = number(n) + (if (n === 1) " argument" else " arguments")
      private def number(n: Int): String =
        if (n === 0) "zero"
        else if (n === 1) "one"
        else Integer.toString(n)
    }
  }

  object Arguments {
    def apply(arguments: Seq[Any]): Arguments = new Arguments(arguments)
  }
}
