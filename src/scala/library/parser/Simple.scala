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

import scalaz.{Failure, Success}
import scalaz.Lens.{firstLens => first}

trait Simple extends Parser {

  override def parse(input: String) = parse(tokenize(input)) map first.get

  private def tokenize(input: String): List[String] =
    input.replace("(", " ( ").replace(")", " ) ").trim.split("\\s+").toList

  private def parse(tokens: List[String]): Validation[(Expression[Any], List[String])] =
    tokens match {
      case "(" :: rest =>
        @tailrec def iterate(expressions: List[Expression[Any]], tokens: List[String]): Validation[(Expression[Any], List[String])] =
          if (tokens.isEmpty || (tokens.head === ")"))
            (Expression(expressions), tokens drop 1).pure[Validation]
          else parse(tokens).validation match {
            case Success((expression, remaining)) => iterate(expressions :+ expression, remaining)
            case Failure(error) => failure(error)
          }

        iterate(List.empty, rest)
      case ")" :: _ => failure(SyntaxError("unexpected )"))
      case token :: rest => (datum(token), rest).pure[Validation]
      case Nil => failure(SyntaxError("unexpected EOF while reading"))
    }

  private def datum(token: String): Expression[Any] = {
    def literal[A: Literal : Parse : Show]: Option[Expression[A]] =
      Parse[A](token) map Expression[A]

    val result: Option[Expression[Any]] = literal[Boolean] orElse literal[Char] orElse literal[Number]
    result | Expression(Symbol(token))
  }
}
