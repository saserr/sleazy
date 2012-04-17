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

import scala.reflect.ClassTag

import scalaz.Equal

case class Value[+A: Show](private val x: A) {

  private lazy val show: String = Show(x)

  def as[B](implicit ct: ClassTag[B]): B =
    if (is[B]) x.asInstanceOf[B]
    else fail(s"$show is not a $ct")

  def is[B](implicit ct: ClassTag[B]): Boolean = ct.runtimeClass.isInstance(x)

  def sameAs(that: Value[Any]): Boolean =
    this.x match {
      case value: AnyRef if that.x.isInstanceOf[AnyRef] => value eq that.x.asInstanceOf[AnyRef]
      case _ => this.x == that.x
    }
}

object Value {

  implicit def isEqualable[A]: Equal[Value[A]] = Equal.equalA

  implicit object IsShowable extends Show[Value[Any]] {
    override def apply(value: Value[Any]) = value.show
  }
}
