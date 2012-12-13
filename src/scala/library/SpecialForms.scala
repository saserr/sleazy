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

trait SpecialForms {
  this: Environment =>

  define('if) = Special("if", "<test>", "<consequent>", "[<alternate>]") {
    (expressions, environment) =>
      val test :: consequent :: _ = expressions
      if (Evaluate.in(environment)(test) === `#f`)
        ((expressions drop 2).headOption map Evaluate.in(environment)) | Value(())
      else Evaluate.in(environment)(consequent)
  }

  define(Symbol("set!")) = Special("set!", "<variable>", "<expression>") {
    (expressions, environment) =>
      val variable :: expression :: _ = expressions
      Value(environment.set(variable.as[Symbol]) = Evaluate.in(environment)(expression))
  }
  define('define) = Special("define", "<variable>", "<expression>") {
    (expressions, environment) =>
      val variable :: expression :: _ = expressions
      Value(environment.define(variable.as[Symbol]) = Evaluate.in(environment)(expression))
  }

  define('lambda) = Special("lambda", "<formals>", "<body>") {
    (expressions, environment) =>
      val formals :: body :: _ = expressions
      implicit object ExpressionsType extends Type[List[Expression[Any]]] {
        override val name = "Formals"
      }
      Value(Lambda.UserDefined(formals.as[List[Expression[Any]]] map {_.as[Symbol]})(body, environment))
  }

  define('begin) = Special("begin", "<expression*>") {
    (expressions, environment) =>
      expressions.foldLeft[Value[Any]](Value(())) {
        (_, expression) =>
          Evaluate.in(environment)(expression)
      }
  }
}
