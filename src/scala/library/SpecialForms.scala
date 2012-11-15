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
  this: Environment.Builder =>

  define('quote) = Special("quote", "<datum>") {
    expressions =>
      val datum :: _ = expressions
      Check(Arguments(expressions).length =:= 1) {
        Result(Quote(datum))
      }
  }
  define('unquote) = Special("unquote", "<datum>") {
    expressions =>
      val datum :: _ = expressions
      Check(Arguments(expressions).length =:= 1) {
        Evaluate(datum) flatMap {value => Evaluate(Unquote(value))}
      }
  }

  define('if) = Special("if", "<test>", "<consequent>", "[<alternate>]") {
    expressions =>
      Check(Arguments(expressions).length between (2, 3)) {
        val test :: consequent :: _ = expressions
        Evaluate(test) flatMap {
          value =>
            if (value === `#f`) (expressions.lift(2) map Evaluate[Expression[Any]]) | Result(())
            else Evaluate(consequent)
        }
      }
  }

  define(Symbol("set!")) = Special("set!", "<variable>", "<expression>") {
    expressions =>
      Check(Arguments(expressions).length =:= 2) {
        val variable :: expression :: _ = expressions
        for {
          current <- State.environment.get
          value <- Evaluate(expression)
          next = variable.as[Symbol] map {name => current.set(name) = value}
          _ <- State.environment.put(next | current)
        } yield Value(())
      }
  }
  define('define) = Special("define", "<variable>", "<expression>") {
    expressions =>
      Check(Arguments(expressions).length =:= 2) {
        val variable :: expression :: _ = expressions
        for {
          current <- State.environment.get
          value <- Evaluate(expression)
          next = variable.as[Symbol] map {name => current.define(name) = value}
          _ <- State.environment.put(next | current)
        } yield Value(())
      }
  }

  define('lambda) = Special("lambda", "<formals>", "<body>") {
    expressions =>
      Check(Arguments(expressions).length =:= 2) {
        val formals :: body :: _ = expressions
        implicit object ExpressionsType extends Type[List[Expression[Any]]] {
          override val name = "Formals"
        }
        Result {
          for {
            values <- formals.as[List[Expression[Any]]]
            parameters <- (values map {_.as[Symbol]}).sequence
          } yield Lambda.UserDefined(parameters)(body)
        }
      }
  }

  define('begin) = Special("begin", "<expression*>") {
    expressions =>
      expressions.foldLeft[Result[Any]](Result(())) {
        (previous, expression) =>
          previous flatMap {_ => Evaluate(expression)}
      }
  }
}
