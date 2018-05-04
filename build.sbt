lazy val `compilerplugins-BangNotation` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val `compilerplugins-ResetEverywhere` = project.dependsOn(Dsl % Test, Dsl % Provided)

lazy val Dsl = project

lazy val task =
  project.dependsOn(`keywords-Shift`, `keywords-Fork` % Test, `keywords-AutoClose` % Test, `keywords-Yield` % Test)

lazy val `keywords-Fork` =
  project.dependsOn(Dsl, `keywords-Shift`, `keywords-Catch`, `keywords-Hang`, `keywords-Each`)

lazy val `keywords-Hang` = project.dependsOn(Dsl)

lazy val `keywords-AsynchronousIo` = project.dependsOn(`keywords-Shift`)

lazy val `keywords-Shift` = project.dependsOn(Dsl)

lazy val `keywords-AutoClose` = project.dependsOn(Dsl, `keywords-Shift`, `keywords-Catch`)

lazy val `keywords-Catch` = project.dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Each` = project.dependsOn(Dsl, `keywords-Shift`, `keywords-Yield` % Test)

lazy val `keywords-Yield` = project.dependsOn(Dsl, `keywords-Shift` % Test)

lazy val `keywords-Monadic` = project.dependsOn(Dsl)

lazy val `domains-scalaz` =
  project.dependsOn(Dsl, `keywords-Catch`, `keywords-Monadic`, `keywords-Shift` % Test, `keywords-Yield` % Test)

lazy val `domains-cats` =
  project.dependsOn(Dsl, `keywords-Catch`, `keywords-Monadic`, `keywords-Shift` % Test, `keywords-Yield` % Test)

lazy val benchmarks = project.dependsOn(task, `keywords-Catch`, `keywords-Fork`)

lazy val `package` = project.dependsOn(
  `compilerplugins-BangNotation`,
  `compilerplugins-ResetEverywhere`,
  `domains-cats`,
  `domains-scalaz`,
  `keywords-Shift`,
  `keywords-Each`,
  `keywords-Yield`,
  `keywords-Fork`,
  `keywords-AsynchronousIo`,
  `keywords-AutoClose`,
  LocalProject("task"),
  Dsl
)

organization in ThisBuild := "com.thoughtworks.dsl"

Seq[ProjectReference](
  `domains-cats`,
  `domains-scalaz`,
  `keywords-Fork`,
  `keywords-Catch`,
  `keywords-Hang`,
  `keywords-Shift`,
  `keywords-Each`,
  `keywords-AsynchronousIo`,
  `keywords-Yield`,
  `keywords-AutoClose`,
  benchmarks,
  LocalProject("task"),
  LocalProject("package")
).flatMap { testingProject =>
  Seq(
    scalacOptions in testingProject += raw"""-Xplugin:${(packageBin in `compilerplugins-BangNotation` in Compile).value}""",
    scalacOptions in testingProject += raw"""-Xplugin:${(packageBin in `compilerplugins-ResetEverywhere` in Compile).value}"""
  )
}

crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.6")

scalacOptions in ThisBuild ++= {
  if (scalaBinaryVersion.value == "2.11") {
    Some("-Ybackend:GenBCode")
  } else {
    None
  }
}

lazy val unidoc =
  project
    .enablePlugins(StandaloneUnidoc, TravisUnidocTitle)
    .settings(
      unidocProjectFilter in ScalaUnidoc in BaseUnidocPlugin.autoImport.unidoc := {
        inAggregates(LocalRootProject)
      },
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
      scalacOptions += "-Xexperimental",
      scalacOptions += "-Ypartial-unification"
    )

publishArtifact := false
