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
package console

import java.util.{Collection, List => JList}

import scala.collection.JavaConversions._
import scala.collection.immutable.Set

import jline.{Completor, ConsoleReader}
import jline.Terminal.{getTerminal => Terminal}

trait JLine extends Console with Configuration {

  type Config <: JLineConfig

  trait JLineConfig {
    def prompt: String
  }

  private val reader = new ConsoleReader

  Terminal disableEcho()
  reader setBellEnabled true

  override def println(s: String): IO[Unit] = IO {
    reader.printString(s)
    reader.printNewline().pure[Option]
  }

  override def readLine(environment: Environment): IO[String] = IO {
    def read(): Option[String] = Option(reader.readLine(config.prompt))

    val completor = new AutoCompletion(environment)
    if (reader addCompletor completor) {
      val result = read()
      reader removeCompletor completor
      result
    } else read()
  }

  private class AutoCompletion(environment: Environment) extends Completor {

    override def complete(buffer: String, cursor: Int, list: JList[_]) = {
      val candidates = list.asInstanceOf[JList[String]]
      if (buffer ne null) {
        val paren = buffer lastIndexOf '('
        candidates addAll (
          if (paren === -1) variables(buffer)
          else {
            val prefix = buffer substring (0, paren + 1)
            val completion = buffer substring (paren + 1)
            val space = completion lastIndexOf ' '
            (if (space === -1) operations(completion)
             else {
               val prefix = completion substring (0, space + 1)
               val name = completion substring (space + 1)
               variables(name) map {prefix + _}
             }) map {prefix + _}
          }
        )
      }

      if (candidates.size() === 1) candidates.set(0, s"${candidates.get(0)} ")
      if (candidates.isEmpty) -1 else 0
    }

    private def variables(prefix: String): Set[String] =
      for {
        variable <- environment.names()
        name = variable.name
        if name startsWith prefix
        value <- environment.find(variable)
        if !value.is[Operation[Any]]
      } yield name

    private def operations(prefix: String): Set[String] =
      for {
        operation <- environment.names()
        name = operation.name
        if name startsWith prefix
        value <- environment.find(operation)
        if value.is[Operation[Any]]
      } yield name
  }
}
