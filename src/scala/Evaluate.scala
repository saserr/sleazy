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

trait Evaluate[-A] extends ((A, Environment) => Result[Any])

object Evaluate {

  def in[A](environment: Environment)(value: A)(implicit evaluate: Evaluate[A]): Result[Any] =
    evaluate(value, environment)

  implicit def literalIsEvaluable[A](implicit literal: Literal[A]): Evaluate[A] =
    literal.evaluate

  implicit object ExpressionsAreEvaluable extends Evaluate[List[Expression[Any]]] {
    override def apply(expressions: List[Expression[Any]], environment: Environment) = expressions match {
      case operation :: operands =>
        for {
          result <- Evaluate.in(environment)(operation)
          value <- result.as[Operation[Any]] flatMap {_(operands, environment)}
        } yield value
      case Nil => fail("nothing to invoke")
    }
  }

  implicit object SymbolIsEvaluable extends Evaluate[Symbol] {
    override def apply(name: Symbol, environment: Environment) =
      environment.find(name) or s"${Show(name)} is not defined"
  }
}
