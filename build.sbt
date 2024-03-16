val scala3Version = "3.4.0"

lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "SMILe",
    version                                := "0.1.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    scalacOptions ++= Seq("-deprecation", "-Wunused:linted")
  )

Compile / doc / scalacOptions ++= Seq("-siteroot", "docs")
Compile / doc / scalacOptions ++= Seq("-project-logo", "images/logo.png")
Compile / doc / scalacOptions ++= Seq(
  "-project-footer",
  "Scala Media Interactive Learning / Aalto LeTech"
)
Compile / doc / scalacOptions ++= Seq(
  "-social-links:github::https://github.com/Aalto-LeTech/SMILe"
)
