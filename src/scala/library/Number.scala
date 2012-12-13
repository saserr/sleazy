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
package library

import java.math.MathContext

import scalaz.Equal

class Number(val n: BigDecimal) extends AnyVal with Ordered[Number] {

  def +(that: Number): Number = new Number(n + that.n)
  def -(that: Number): Number = new Number(n - that.n)
  def *(that: Number): Number = new Number(n * that.n)
  def /(that: Number): Number = new Number(n / that.n)

  def apply(context: MathContext): Number = new Number(n(context))

  override def compare(that: Number) = this.n compare that.n
}

object Number {

  val zero: Number = Number(0)
  val one: Number = Number(1)

  def apply(value: Long): Number = new Number(BigDecimal(value, MathContext.UNLIMITED))

  def apply(value: String): Option[Number] =
    try new Number(BigDecimal(value, MathContext.UNLIMITED)).pure[Option]
    catch {
      case _: NumberFormatException => None
    }

  implicit object HasType extends Type[Number] {
    override val name = "Number"
  }

  implicit val isEqualable: Equal[Number] = Equal.equalA

  implicit object IsShowable extends Show[Number] {
    override def apply(value: Number) = value.n.underlying().toPlainString
  }

  implicit object IsLiteral extends Literal[Number]
}
