val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Borda counter",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.10",
    libraryDependencies += "org.maraist" %% "scala-latex" % "2.0.0"
  )
