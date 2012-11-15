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

trait Evaluate[-A] extends (A => Result[Any])

object Evaluate {

  def apply[A](value: A)(implicit evaluate: Evaluate[A]): Result[Any] = evaluate(value)

  def in[A](environment: Environment)(value: A)
           (implicit evaluate: Evaluate[A]): Validation[(Environment, Value[Any])] =
    evaluate(value)(environment)

  implicit def literalIsEvaluable[A](implicit literal: Literal[A]): Evaluate[A] =
    literal.evaluate

  implicit object ExpressionsAreEvaluable extends Evaluate[List[Expression[Any]]] {
    override def apply(expressions: List[Expression[Any]]) = expressions match {
      case operation :: operands =>
        def whichHas(environment: Environment)(name: Symbol): Validation[Environment] =
          environment.whichHas(name) or s"could not find the environment where ${Show(name)} is defined"

        for {
          result <- Evaluate(operation)
          current <- State.environment.get
          definedIn = (operation.as[Symbol] map whichHas(current)) | current.right
          value <- (for {f <- result.as[Operation[Any]]; e <- definedIn} yield f(operands, e)).flatten[Value[Any]]
        } yield value
      case Nil => fail("nothing to invoke")
    }
  }

  implicit object SymbolIsEvaluable extends Evaluate[Symbol] {
    override def apply(name: Symbol) =
      State.environment.get flatMap {
        current =>
          State(current.find(name) or s"${Show(name)} is not defined")
      }
  }
}
