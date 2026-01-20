ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name             := "ProofPlayground",
    idePackagePrefix := Some("proofPlayground")
  )

scalacOptions ++= Seq(
  "-feature",
  "-source:future",
  "-language:experimental.modularity",
  "-language:experimental.pureFunctions",
  "-Wsafe-init",
  "-Yexplicit-nulls",
)

libraryDependencies ++= Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
  "org.scalatestplus" %% "scalacheck-1-19" % "3.2.19.0" % Test,
)
