libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.2.20"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Seq("–Xexperimental")
    case _      => Seq.empty
  }
}
