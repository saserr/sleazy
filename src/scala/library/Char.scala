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

import scala.collection.immutable.{Map, Set}

import scalaz.Equal

class Char(val underlying: scala.Char) extends AnyVal {
  def toHex: String = Integer.toString(underlying.toInt, 16)
}

object Char {

  private val named = Map[String, Char](
    "nul"       -> new Char('\u0000'),
    "alarm"     -> new Char('\u0007'),
    "backspace" -> new Char('\u0008'),
    "tab"       -> new Char('\u0009'),
    "linefeed"  -> new Char('\u000A'),
    "newline"   -> new Char('\u000A'),
    "vtab"      -> new Char('\u000B'),
    "page"      -> new Char('\u000C'),
    "return"    -> new Char('\u000D'),
    "esc"       -> new Char('\u001B'),
    "space"     -> new Char('\u0020'),
    "delete"    -> new Char('\u007F')
  )
  private val reversed = {
    val builder = Map.newBuilder[Char, String]
    for ((name, char) <- named) builder += (char -> s"#\\$name")
    builder.result()
  }

  val names: Set[String] = named.keySet
  def withName(name: String): Option[Char] = named get name

  implicit object HasType extends Type[Char] {
    override val name = "Char"
  }

  implicit val isEqualable: Equal[Number] = Equal.equalA

  implicit object IsShowable extends Show[Char] {
    override def apply(value: Char) = reversed.get(value) | s"#\\x${value.toHex}"
  }

  implicit object IsLiteral extends Literal[Char]
}
