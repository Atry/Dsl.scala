libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.6-SNAP1" % Test

scalacOptions in Compile ++= {
  scalaBinaryVersion.value match {
    case "2.11" => Some("–Xexperimental")
    case _      => None
  }
}

libraryDependencies ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Nil
  } else {
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  }
}

libraryDependencies += "com.thoughtworks.enableIf" %% "enableif" % "1.1.6"
