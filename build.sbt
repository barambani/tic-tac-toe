lazy val commonSettings = Seq (
  scalaVersion := "2.12.1"
)

lazy val `tic-tac-toe` = (project in file(".")).settings(commonSettings: _*)

initialCommands := """
  import Api._
  import Algebra._
"""

scalacOptions ++= Seq (
  "-feature",
  "-deprecation",
  "-target:jvm-1.8"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8"
)

logLevel := Level.Info
