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

sealed trait Expression[+A] {
  def evaluate(environment: Environment): Value[A]
}

case class Constant[+A](value: A) extends Expression[A] {
  override def evaluate(environment: Environment) = Value(value)
}

case class Variable(name: Symbol) extends Expression[Any] {
  override def evaluate(environment: Environment) = environment.find(name).get
}

case class Expressions(expressions: List[Expression[Any]]) extends Expression[Any] {
  override def evaluate(environment: Environment) = expressions match {
    case operation :: operands =>
      val form = operation.evaluate(environment).as[Operation[Any]]
      form(operands, environment)
    case Nil => fail("Nothing to invoke")
  }
}
