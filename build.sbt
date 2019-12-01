val Http4sVersion  = "0.20.8"
val CirceVersion   = "0.11.1"
val Specs2Version  = "4.1.0"
val LogbackVersion = "1.2.3"
val Specs2CatsVersion = "0.3.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.vbm",
    name := "wiresmoke",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.10",
    libraryDependencies ++= Seq(
      "org.http4s"     %% "http4s-blaze-server"        % Http4sVersion,
      "org.http4s"     %% "http4s-blaze-client"        % Http4sVersion,
      "org.http4s"     %% "http4s-dsl"                 % Http4sVersion,
      "ch.qos.logback" % "logback-classic"             % LogbackVersion,
      "org.specs2"     %% "specs2-core"                % Specs2Version % "test",
      "com.codecommit" %% "cats-effect-testing-specs2" % Specs2CatsVersion % "test"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Ypartial-unification",
  "-Xfatal-warnings"
)
