package com.thoughtworks.sbtBestPractice

import org.eclipse.jgit.api.CreateBranchCommand.SetupUpstreamMode
import org.eclipse.jgit.api.Git
import sbt._
import sbt.Keys._
import org.eclipse.jgit.transport.URIish
import resource.managed

/**
  * Configure git from Travis environment variables
  *
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object TravisEnvironmentVariables extends AutoPlugin {

  object autoImport {
    val travisBranch = settingKey[String]("For builds not triggered by a pull request this is the name of the branch currently being built; whereas for builds triggered by a pull request this is the name of the branch targeted by the pull request (in many cases this will be master).")
    val travisRepoSlug = settingKey[String]("The slug (in form: owner_name/repo_name) of the repository currently being built. (for example, “travis-ci/travis-build”).")
    val travisCommit = settingKey[String]("The commit that the current build is testing.")
  }

  import autoImport._

  private val Variables = Seq(
    "TRAVIS_BRANCH" -> travisBranch,
    "TRAVIS_REPO_SLUG" -> travisRepoSlug,
    "TRAVIS_COMMIT" -> travisCommit
  )

  override def globalSettings = {
    Variables.flatMap  {
      case (variableName, key) =>
        sys.env.get(variableName) match {
          case None =>
            Seq.empty
          case Some(value) =>
            Seq(key := value)
        }
    }
  }

  override def trigger = allRequirements

}