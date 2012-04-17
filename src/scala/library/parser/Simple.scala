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

import scala.annotation.tailrec

import scalaz.Lens.{firstLens => first}

trait Simple extends Parser {

  override def parse(input: String) = first.get(parse(tokenize(input)))

  private def tokenize(input: String): List[String] =
    input.replace("(", " ( ").replace(")", " ) ").trim.split("\\s+").toList

  private def parse(tokens: List[String]): (Expression[Any], List[String]) =
    tokens match {
      case "(" :: rest =>
        @tailrec def iterate(expressions: List[Expression[Any]], tokens: List[String]): (Expression[Any], List[String]) =
          if (tokens.isEmpty || (tokens.head === ")"))
            (Expressions(expressions), tokens drop 1)
          else {
            val (expression, remaining) = parse(tokens)
            iterate(expressions :+ expression, remaining)
          }

        iterate(List.empty, rest)
      case ")" :: _ => fail("Unexpected )")
      case token :: rest => (datum(token), rest)
      case Nil => fail("Unexpected EOF while reading")
    }

  private def datum(token: String): Expression[Any] = token match {
    case "#t" | "#T" => Constant(true)
    case "#f" | "#F" => Constant(false)
    case _ => Number(token).map[Expression[Any]]{Constant(_)} | Variable(Symbol(token))
  }
}
