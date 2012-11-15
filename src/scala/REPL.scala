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

import scalaz.{Failure, StateT, Success}
import scalaz.Lens.{firstLens => first, secondLens => second}

import library.parser

class REPL extends Runnable {
  self: Library with Console with Parser =>

  type World[+A] = StateT[IO, (Int, Environment), A]

  object World {

    def apply[A](a: IO[A]): World[A] = StateT[IO, (Int, Environment), A](s => a map {s.->})

    object generation {
      val get: World[Int] = StateT {case s@(g, _) => (s -> g).pure[IO]}
    }

    object environment {
      val get: World[Environment] = StateT {case s@(_, e) => (s -> e).pure[IO]}
      def put(e: Environment): World[Unit] = StateT {case (g, _) => ((g, e), ()).pure[IO]}
    }

    def put(s: (Int, Environment)): World[Unit] = StateT {_ => (s, ()).pure[IO]}
  }

  val read: World[Validation[Expression[Any]]] =
    for {
      current <- World.environment.get
      input <- World(readLine(current))
      _ <- if (input.isEmpty) World(IO(None)) else input.pure[World]
    } yield parse(input)

  def eval(input: Validation[Expression[Any]]): World[Validation[Value[Any]]] =
    for {
      current <- World.environment.get
      result = input flatMap Evaluate.in(current)
      next = (result map first.get) | current
      value = result map second.get
      _ <- World.environment.put(next)
    } yield value

  def print(result: Validation[(Expression[Any], Value[Any])]): World[Unit] =
    result.validation match {
      case Failure(error) => World(println(Show(error)))
      case Success((_, Value(()))) => ().pure[World]
      case Success((expression, value)) =>
        for {
          generation <- World.generation.get
          current <- World.environment.get
          name = expression.as[Symbol] | Symbol(s"res$generation")
          next = if (expression.is[Symbol]) generation -> current
                 else (generation + 1) -> (current.define(name) = value)
          _ <- World.put(next)
          _ <- World(println(s"${Show(name)}: ${Type(value)} = ${Show(value)}"))
        } yield ()
    }

  def readEvalPrint: World[Validation[Value[Any]]] =
    for {
      input <- read
      result <- eval(input)
      _ <- print((input |@| result).tupled)
    } yield result

  override def run() {
    val genesis = 1 -> new Environment(outer = predef.pure[Option])
    def loop: World[Unit] = readEvalPrint flatMap {_ => loop}

    loop(genesis).run.unsafePerformIO()
  }
}

object REPL extends App {

  val default: REPL = new REPL with library.Base with console.JLine with parser.Simple {

    override type Config = JLineConfig

    override object config extends JLineConfig {
      override val prompt = "sleazy> "
    }
  }

  default.run()
}
