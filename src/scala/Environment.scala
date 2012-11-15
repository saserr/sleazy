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

import scala.collection.immutable.{Map, Set}

case class Environment(variables: Map[Symbol, Value[Any]] = Map.empty, outer: Option[Environment] = None) {

  lazy val names: Set[Symbol] = variables.keys.toSet ++ ((outer map {_.names}) | Set.empty)

  def find(name: Symbol): Option[Value[Any]] =
    variables.get(name) orElse {outer flatMap {_ find name}}

  def whichHas(name: Symbol): Option[Environment] =
    if (variables contains name) this.pure[Option]
    else outer flatMap {_ whichHas name}

  object set {
    def update(name: Symbol, value: Value[Any]): Environment =
      if (variables contains name) copy(variables = variables updated (name, value))
      else copy(outer = outer map {_.set(name) = value})
  }

  object define {
    def update(name: Symbol, value: Value[Any]): Environment =
      copy(variables = variables updated (name, value))
  }
}

object Environment {
  class Builder {

    private val variables = Map.newBuilder[Symbol, Value[Any]]

    def result(): Environment = Environment(variables.result())

    object define {
      def update(name: Symbol, value: Value[Any]) {
        variables += (name -> value)
      }
    }
  }
}
