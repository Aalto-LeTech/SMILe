val scala3Version = "3.3.3" // Scala LTS
val smileVersion  = "0.1.0"

lazy val SMILe = project
  .in(file("."))
  .settings(
    publish      := {},
    publishLocal := {},
    scalaVersion := scala3Version
  )
  .aggregate(smile.js, smile.jvm)

lazy val smile = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name         := "SMILe",
    version      := smileVersion,
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-Wunused:linted"),
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "smile"
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided"
  )
  .jsSettings(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.4.0",
    libraryDependencies += "com.lihaoyi"  %%% "scalatags"   % "0.12.0",
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withSourceMap(true)
    },
    packageJsonTask := {
      val jsDir     = (Compile / fastLinkJS / crossTarget).value / "smile-fastopt"
      val outputDir = (Compile / fastLinkJS / crossTarget).value / "smile"
      val jsonFile  = outputDir / "package.json"
      IO.copyDirectory(jsDir, outputDir)
      IO.write(jsonFile, packageJson)
      streams.value.log.success(s"Generated package.json in $outputDir")
    },
    addCommandAlias("js", ";fastLinkJS;packageJsonTask")
  )

lazy val packageJsonTask = taskKey[Unit]("Generates package.json file")

lazy val packageJson = s"""{
  "name": "smile",
  "version": "$smileVersion"
}"""

// SCALADOC SETTINGS

Compile / doc / scalacOptions ++= Seq("-siteroot", "docs")
Compile / doc / scalacOptions ++= Seq("-project-logo", "images/logo.png")
Compile / doc / scalacOptions ++= Seq(
  "-project-footer",
  "Scala Media Interactive Learning / Aalto LeTech"
)
Compile / doc / scalacOptions ++= Seq(
  "-social-links:github::https://github.com/Aalto-LeTech/SMILe"
)
