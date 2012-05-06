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

import scalaz.{Failure, IdT, Success}
import scalaz.Lens.{secondLens => second}

import library.parser

class REPL extends Runnable {
  self: Library with Console with Parser =>

  type World[A] = IdT[IO, A]

  object World {
    def apply[A](a: IO[A]): World[A] = IdT(a)
    def apply[A](a: => A): World[A] = apply(IO(a.pure[Option]))
  }

  def read(current: Environment): World[Validation[Expression[Any]]] =
    for {
      input <- World(readLine(current))
      _ <- if (input.isEmpty) World(IO(None)) else input.pure[World]
    } yield parse(input)

  def eval(current: Environment,
           input: Validation[Expression[Any]]): World[Validation[Value[Any]]] =
    World(input flatMap Evaluate.in(current))

  def print(current: (Int, Environment),
            result: Validation[(Expression[Any], Value[Any])]): World[(Int, Environment)] =
    result.validation match {
      case Failure(error) => World(println(Show(error))) >| current
      case Success((_, Value(()))) => World(current)
      case Success((expression, value)) =>
        val (generation, environment) = current
        val name = expression.as[Symbol] | Symbol(s"res$generation")
        val next = if (expression.is[Symbol]) current
                   else {
                     environment.define(name) = value
                     (generation + 1) -> environment
                   }

        World(println(s"${Show(name)}: ${Type(value)} = ${Show(value)}")) >| next
    }

  def readEvalPrint(current: (Int, Environment)): World[(Int, Environment)] = {
    val environment = second.get(current)

    for {
      input <- read(environment)
      result <- eval(environment, input)
      next <- print(current, (input |@| result).tupled)
    } yield next
  }

  override def run() {
    val genesis = 1 -> new Environment(outer = predef.pure[Option])
    def loop(current: (Int, Environment)): World[Unit] = readEvalPrint(current) flatMap loop

    loop(genesis).run.run.unsafePerformIO()
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
