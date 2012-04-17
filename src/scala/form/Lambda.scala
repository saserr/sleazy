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

import scala.collection.mutable.Map

sealed trait Lambda[+A] extends Operation[A] {

  def formals: List[Symbol]
  def invoke(arguments: HList): Value[A]

  override def apply(operands: List[Expression[Any]], executedIn: Environment) = {
    val arguments = operands map {_.evaluate(executedIn)}
    invoke(arguments)
  }
}

object Lambda {

  class BuiltIn[+A](override val formals: List[Symbol])(f: HList => Value[A]) extends Lambda[A] {
    override protected lazy val show = Show(this)(Lambda.IsShowable)
    override def invoke(arguments: HList) = f(arguments)
  }

  object BuiltIn {

    class Helper(formals: String*) {

      def apply[A](f: HList => Value[A]): Value[BuiltIn[A]] =
        Value(new BuiltIn(formals.toList map {Symbol(_)})(f))

      def apply[A: Manifest, B](f: Seq[A] => Value[B]): Value[BuiltIn[B]] = apply {
        arguments: HList =>
          f(arguments map {_.as[A]})
      }
    }

    def apply(formals: String*): Helper = new Helper(formals: _*)
  }

  case class UserDefined[+A](override val formals: List[Symbol])
                            (val body: Expression[A], definedIn: Environment) extends Lambda[A] {

    override protected lazy val show = Show(this)(Lambda.IsShowable)

    override def invoke(arguments: HList) =
      body.evaluate(Environment(Map((formals zip arguments): _*), definedIn.pure[Option]))
  }

  implicit object IsShowable extends Show[Lambda[Any]] {
    override def apply(lambda: Lambda[Any]) = s"<lambda ${Show(lambda.formals)}>"
  }
}
