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
package parser

trait Parse[+A] extends (String => Option[A])

object Parse {

  def apply[A](str: String)(implicit parse: Parse[A]): Option[A] = parse(str)

  implicit object BooleanIsParsable extends Parse[Boolean] {
    override def apply(str: String) = str match {
      case "#t" | "#T" => true.pure[Option]
      case "#f" | "#F" => false.pure[Option]
      case _ => None
    }
  }

  implicit object CharIsParsable extends Parse[Char] {
    override def apply(str: String) =
      if (str.startsWith("#\\x")) {
        val codePoint: Int = Integer.parseInt(str.substring(3), 16)
        if (Character.charCount(codePoint) === 1)
          new Char(Character.toChars(codePoint)(0)).pure[Option]
        else None
      } else if (str.startsWith("#\\")) {
        if (str.length === 3) new Char(str.charAt(2)).pure[Option]
        else Char.withName(str.substring(2))
      } else None
  }

  implicit object NumberIsParsable extends Parse[Number] {
    override def apply(str: String) = Number(str)
  }
}
