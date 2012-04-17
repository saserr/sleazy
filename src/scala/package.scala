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

package object sleazy extends std.AllInstances
                      with    syntax.ToDataOps
                      with    syntax.ToTypeClassOps
                      with    syntax.std.ToAllStdOps {

  type List[+A] = scala.collection.immutable.List[A]
  val List = scala.collection.immutable.List

  type Seq[+A] = scala.collection.immutable.Seq[A]
  val Seq = scala.collection.immutable.Seq

  type HList = Seq[Value[Any]]
  type Symbol = scala.Symbol

  def fail(message: String): Nothing = throw new IllegalArgumentException(message)
}
