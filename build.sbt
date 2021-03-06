scalaVersion := "2.13.5"

libraryDependencies +="org.jsoup" % "jsoup" % "1.11.3"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"

libraryDependencies += "io.chrisdavenport" %% "cats-time"     % "0.3.4"
libraryDependencies += "org.rogach" %% "scallop" % "4.0.2"
libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "2.3.8"
libraryDependencies += "org.wvlet.airframe" %% "airframe-log" % "20.12.1"

val circeVersion = "0.12.3"
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
