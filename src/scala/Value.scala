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

import scalaz.Equal

case class Value[+A: Show : Type : Unquote](private val x: A) {

  private lazy val show: String = Show(x)
  private lazy val `type`: String = Type(x)
  private lazy val unquote: Expression[Any] = Unquote(x)

  def as[B: ClassTag](implicit t: Type[B]): B =
    if (is[B]) x.asInstanceOf[B]
    else fail(s"$show is not a ${t.name}")

  def is[B](implicit ct: ClassTag[B]): Boolean = ct.runtimeClass.isInstance(x)

  def sameAs(that: Value[Any]): Boolean =
    this.x match {
      case value: AnyRef if that.x.isInstanceOf[AnyRef] => value eq that.x.asInstanceOf[AnyRef]
      case _ => this.x == that.x
    }
}

object Value {

  implicit def fromLiteral[A: Literal](value: A): Value[Any] = Quote(value)

  implicit object HasType extends Type[Value[Any]] {
    override val name = "Value"
    override def apply(value: Value[Any]) = value.`type`
  }

  implicit def isEqualable[A]: Equal[Value[A]] = Equal.equalA

  implicit object IsShowable extends Show[Value[Any]] {
    override def apply(value: Value[Any]) = value.show
  }

  implicit object IsUnquotable extends Unquote[Value[Any]] {
    override def apply(value: Value[Any]) = value.unquote
  }
}
