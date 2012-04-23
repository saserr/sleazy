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

import form.{Lambda, Special}
import util.Check
import util.Check.Arguments

trait SpecialForms {
  this: Environment =>

  define('quote) = Special("quote", "<datum>") {
    (expressions, _) =>
      val datum :: _ = expressions
      Check(Arguments(expressions).length =:= 1) {
        Result(Quote(datum))
      }
  }
  define('unquote) = Special("unquote", "<datum>") {
    (expressions, environment) =>
      val datum :: _ = expressions
      Check(Arguments(expressions).length =:= 1) {
        Evaluate.in(environment)(datum) flatMap {value => Evaluate.in(environment)(Unquote(value))}
      }
  }

  define('if) = Special("if", "<test>", "<consequent>", "[<alternate>]") {
    (expressions, environment) =>
      Check(Arguments(expressions).length between (2, 3)) {
        val test :: consequent :: _ = expressions
        Evaluate.in(environment)(test) flatMap {
          value =>
            if (value === `#f`)
              ((expressions drop 2).headOption map Evaluate.in(environment)) | Result(())
            else Evaluate.in(environment)(consequent)
        }
      }
  }

  define(Symbol("set!")) = Special("set!", "<variable>", "<expression>") {
    (expressions, environment) =>
      Check(Arguments(expressions).length =:= 2) {
        val variable :: expression :: _ = expressions
        for {
          name <- variable.as[Symbol]
          value <- Evaluate.in(environment)(expression)
          _ = (environment.set(name) = value)
        } yield Value(())
      }
  }
  define('define) = Special("define", "<variable>", "<expression>") {
    (expressions, environment) =>
      Check(Arguments(expressions).length =:= 2) {
        val variable :: expression :: _ = expressions
        for {
          name <- variable.as[Symbol]
          value <- Evaluate.in(environment)(expression)
          _ = (environment.define(name) = value)
        } yield Value(())
      }
  }

  define('lambda) = Special("lambda", "<formals>", "<body>") {
    (expressions, environment) =>
      Check(Arguments(expressions).length =:= 2) {
        val formals :: body :: _ = expressions
        implicit object ExpressionsType extends Type[List[Expression[Any]]] {
          override val name = "Formals"
        }
        Result {
          for {
            values <- formals.as[List[Expression[Any]]]
            parameters <- (values map {_.as[Symbol]}).sequence
          } yield Lambda.UserDefined(parameters)(body, environment)
        }
      }
  }

  define('begin) = Special("begin", "<expression*>") {
    (expressions, environment) =>
      expressions.foldLeft[Result[Any]](Result(())) {
        (previous, expression) =>
          previous flatMap {_ => Evaluate.in(environment)(expression)}
      }
  }
}
