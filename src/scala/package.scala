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

package org.saserr

import scalaz.{std, syntax}
import scalaz.{\/, Equal}

package object sleazy extends std.AllInstances
                      with    syntax.ToDataOps
                      with    syntax.ToTypeClassOps
                      with    syntax.std.ToAllStdOps {

  type List[+A] = scala.collection.immutable.List[A]
  val List = scala.collection.immutable.List

  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  type HList = List[Value[Any]]
  type Symbol = scala.Symbol

  type Validation[+A] = Error \/ A
  type Result[+A] = Validation[Value[A]]

  def fail(message: String): Result[Nothing] = failure(EvaluationError(message))
  def failure(error: Error): Validation[Nothing] = error.left

  implicit class OptionOps[A](o: Option[A]) {
    def or(error: String): Validation[A] = o \/> EvaluationError(error)
  }
}
