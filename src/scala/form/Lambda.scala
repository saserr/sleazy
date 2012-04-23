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

import util.Check
import util.Check.Arguments

sealed trait Lambda[+A] extends Operation[A] {

  def formals: List[Symbol]
  def invoke(arguments: HList): Result[A]

  override protected val `type` = "Lambda"

  override def apply(operands: List[Expression[Any]], executedIn: Environment) =
    (operands map Evaluate.in(executedIn)).sequence[Validation, Value[Any]] flatMap invoke
}

object Lambda {

  class BuiltIn[+A](override val formals: List[Symbol])(f: HList => Result[A]) extends Lambda[A] {
    override protected lazy val show = Show(this)(BuiltIn.IsShowable)
    override def invoke(arguments: HList) = f(arguments)
  }

  object BuiltIn {

    class Helper(formals: String*) {

      def apply[A](f: HList => Result[A]): BuiltIn[A] = new BuiltIn(formals.toList map {Symbol(_)})(f)

      def apply[A: Manifest : Type, B](f: Seq[A] => Result[B]): BuiltIn[B] = apply {
        arguments: HList =>
          (arguments map {_.as[A]}).sequence flatMap f
      }
    }

    def apply(formals: String*): Helper = new Helper(formals: _*)

    implicit object IsShowable extends Show[BuiltIn[Any]] {
      override def apply(lambda: BuiltIn[Any]) = s"(lambda ${Show(lambda.formals)} <built-in>)"
    }
  }

  case class UserDefined(override val formals: List[Symbol])
                        (val body: Expression[Any], definedIn: Environment) extends Lambda[Any] {

    override protected lazy val show = Show(this)(UserDefined.IsShowable)

    override def invoke(arguments: HList) =
      Check(Arguments(arguments).length =:= formals.length) {
        Evaluate.in(Environment(Map((formals zip arguments): _*), definedIn.pure[Option]))(body)
      }
  }

  object UserDefined {
    implicit object IsShowable extends Show[UserDefined] {
      override def apply(lambda: UserDefined) = s"(lambda ${Show(lambda.formals)} ${Show(lambda.body)})"
    }
  }
}
