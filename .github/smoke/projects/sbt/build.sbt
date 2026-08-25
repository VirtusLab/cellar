lazy val `smoke-app` = (project in file("."))
  .settings(
    scalaVersion := "3.8.1",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test
  )
