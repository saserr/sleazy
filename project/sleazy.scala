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

import sbt._
import sbt.Keys._

object Info {

  val version = "0.1-SNAPSHOT"

  val settings: Seq[Setting[_]] = Seq(
    Keys.version := version,
    organization := "org.saserr.sleazy",
    licenses +=("Apache License Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    startYear := Some(2012)
  )
}

object Layout {
  val settings: Seq[Setting[_]] = Seq(
    sourceDirectory in Compile <<= baseDirectory(_ / "src"),
    sourceDirectory in Test <<= baseDirectory(_ / "test")
  )
}

object Build {
  val settings: Seq[Setting[_]] = Seq(
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-target:jvm-1.6", "-deprecation", "-unchecked", "-feature",
                          "-Xlog-reflective-calls", "-Ywarn-adapted-args", "-encoding", "utf8"),
    javacOptions ++= Seq("-target", "6", "-source", "6", "-encoding", "utf8")
  )
}

// Shell prompt which show the current project,
// git branch and build version
object Shell {

  object devnull extends ProcessLogger {
    def info(s: => String) {}
    def error(s: => String) {}
    def buffer[T](f: => T): T = f
  }

  def branch() = ("git status -sb" lines_! devnull headOption) getOrElse "-" stripPrefix "## "

  val settings: Seq[Setting[_]] = Seq(
    shellPrompt := {
      (state: State) => "%s:%s:%s> ".format(Project.extract(state).currentProject.id, branch(), Info.version)
    }
  )
}

object Dependency {

  object Scalaz {
    val version = "7.0.2"
    val core = "org.scalaz" %% "scalaz-core" % version
    val effect = "org.scalaz" %% "scalaz-effect" % version
  }

  object JLine {
    val version = "0.9.94"
    val core = "jline" % "jline" % version
  }
}

object sleazy extends Build {

  import Dependency._

  lazy val project = Project(
    "sleazy",
    file("."),
    settings = Defaults.defaultSettings ++
               Info.settings ++
               Layout.settings ++
               Build.settings ++
               Shell.settings ++
               Seq(libraryDependencies ++= Seq(JLine.core, Scalaz.core, Scalaz.effect))
  )
}
