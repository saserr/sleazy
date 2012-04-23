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

trait Operation[+A] extends ((List[Expression[Any]], Environment) => Result[A]) {
  protected def show: String
  protected def `type`: String
}

object Operation {

  implicit object HasType extends Type[Operation[Any]] {
    override val name = "Operation"
    override def apply(operation: Operation[Any]) = operation.`type`
  }

  implicit object IsShowable extends Show[Operation[Any]] {
    override def apply(operation: Operation[Any]) = operation.show
  }

  implicit object IsLiteral extends Literal[Operation[Any]]
}
