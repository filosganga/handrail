val catsV = "2.7.0"
val catsEffectV = "3.3.8"
val catsParseV = "0.3.6"
val enumeratumV = "1.7.0"
val caseInsensitiveV = "1.2.0"
val munitV = "0.7.29"
val munitCatsEffectV = "1.0.7"
val scalacheckEffectV = "1.0.3"
val log4j2V = "2.20.0"
val log4catsV = "2.2.0"
val fs2V = "3.2.5"
val apacheCommonsTextV = "1.9"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / organizationName := "Filippo De Luca"
ThisBuild / dynverSeparator := "-"
ThisBuild / evictionErrorLevel := Level.Warn
ThisBuild / organization := "com.github.filosganga"
ThisBuild / organizationHomepage := Some(url("https://filippodeluca.com"))
ThisBuild / homepage := Some(url("https://github.com/filosganga/handrail"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/filosganga/handrail"), "scm:git:git@github.com:filosganga/handrail.git")
)
ThisBuild / licenses := Seq("Apache-2.0" -> url("https://opensource.org/licenses/apache-2.0"))
ThisBuild / developers := List(
  Developer("filippo.deluca", "Filippo De Luca", "me@filippodeluca.com", url("https://filippodeluca.com"))
)

lazy val handrail = (project in file("."))
  .settings(
    name := "handrail",
    description := "Handlebars parser in scala",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % munitV % Test,
      "org.scalameta" %% "munit-scalacheck" % munitV % Test,
      "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectV % Test,
      "org.typelevel" %% "scalacheck-effect-munit" % scalacheckEffectV % Test,
      "com.beachape" %% "enumeratum" % enumeratumV,
      "org.typelevel" %% "cats-core" % catsV,
      "org.typelevel" %% "cats-effect" % catsEffectV,
      "org.typelevel" %% "cats-parse" % catsParseV,
      "co.fs2" %% "fs2-core" % fs2V,
      "co.fs2" %% "fs2-io" % fs2V,
      "org.typelevel" %% "log4cats-slf4j" % log4catsV,
      "org.apache.commons" % "commons-text" % apacheCommonsTextV,
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4j2V % Test
    )
  )

lazy val benchmark = (project in file("benchmark"))
  .dependsOn(handrail)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "handrail-benchmark",
    libraryDependencies ++= List(
      "com.github.jknack" % "handlebars" % "4.3.0"
    )
  )

lazy val docs = project
  .enablePlugins(MdocPlugin, DocusaurusPlugin)
  .in(file("handrail-docs"))
  .dependsOn(handrail)
  .settings(
    moduleName := "handrail-docs",
    mdocVariables := Map(
      "VERSION" -> version.value
    )
  )
