lazy val root = Project(id = "pbt-functions", base = file("."))
  .settings(moduleName := "root")
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false
  )
  .aggregate(core, scalacheck)

lazy val core = project
  .settings(
    moduleName := "pbt-functions",
    name       := "core"
  )

lazy val scalacheck = project
  .settings(
    moduleName := "pbt-functions.scalacheck",
    name       := "scalacheck"
  )
  .settings(libraryDependencies += "org.scalacheck" %% "scalacheck" % Versions.scalacheck)
  .dependsOn(core)
