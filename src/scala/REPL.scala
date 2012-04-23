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

import scalaz.{Failure, Success}

import library.parser

class REPL extends Runnable {
  self: Library with Console with Parser =>

  override def run() {
    var i = 1
    val global = new Environment(outer = predef.pure[Option])
    var input = readLine(global) filter {!_.isEmpty}
    while (input.isDefined) {
      parse(input.get).validation match {
        case Failure(error) => println(s"syntax error: $error")
        case Success(expression) =>
          Evaluate.in(global)(expression).validation match {
            case Failure(error) => println(s"error: $error")
            case Success(Value(())) => /* no result */
            case Success(value) =>
              val name = expression.as[Symbol].validation match {
                case Success(symbol) => Show(symbol)
                case _ =>
                  val result = s"res$i"
                  global.define(Symbol(result)) = value
                  i += 1
                  result
              }
              println(s"$name: ${Type(value)} = ${Show(value)}")
          }
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
