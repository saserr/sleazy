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

import scala.language.implicitConversions
import scala.reflect.ClassTag

case class Expression[+A: Evaluate : Quote : Show](private val value: A) {

  private object evaluate {
    def in(environment: Environment): Value[Any] = Evaluate.in(environment)(value)
  }
  private lazy val quote: Value[Any] = Quote(value)
  private lazy val show: String = Show(value)

  def as[B: ClassTag](implicit t: Type[B]): B =
    if (is[B]) value.asInstanceOf[B]
    else fail(s"$show is not a ${t.name}")

  def is[B](implicit ct: ClassTag[B]): Boolean = ct.runtimeClass.isInstance(value)
}

object Expression {

  implicit def fromLiteral[A: Literal](value: A): Expression[Any] = Unquote(value)

  implicit object IsEvaluable extends Evaluate[Expression[Any]] {
    override def apply(expression: Expression[Any], environment: Environment) =
      expression.evaluate.in(environment)
  }

  implicit object IsQuotable extends Quote[Expression[Any]] {
    override def apply(expression: Expression[Any]) = expression.quote
  }

  implicit object IsShowable extends Show[Expression[Any]] {
    override def apply(expression: Expression[Any]) = expression.show
  }
}
