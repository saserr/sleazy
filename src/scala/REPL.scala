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

import library.parser

class REPL extends Runnable {
  self: Library with Console with Parser =>

  override def run() {
    var i = 1
    val global = new Environment(outer = predef.pure[Option])
    var input = readLine(global) filter {!_.isEmpty}
    while (input.isDefined) {
      val expression = parse(input.get)
      expression.evaluate(global) match {
        case Value(()) => /* no result */
        case value =>
          val name = expression match {
            case Variable(symbol) => Show(symbol)
            case _ =>
              val name = s"res$i"
              global.define(Symbol(name)) = value
              i += 1
              name
          }
          println(s"$name: ${Type(value)} = ${Show(value)}")
      }
      input = readLine(global) filter {!_.isEmpty}
    }
  }
}

object REPL extends App {

  val default: REPL = new REPL with library.Base with console.JLine with parser.Simple {

    type Config = JLineConfig

    override object config extends JLineConfig {
      override val prompt = "sleazy> "
    }
  }

  default.run()
}
