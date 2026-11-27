addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.6.2")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.23.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.14.0")
// The jsdom environment is published for Scala 2.13 only, which the Scala 3 build definition of
// sbt 2 can use. Its own Scala 2.13 copies of the environment APIs are left out, because the
// Scala.js plugin already brings the Scala 3 ones.
libraryDependencies += ("org.scala-js" % "scalajs-env-jsdom-nodejs_2.13" % "1.1.1")
  .exclude("org.scala-js", "scalajs-js-envs_2.13")
  .exclude("org.scala-js", "scalajs-env-nodejs_2.13")

// The optional real-browser test environment that smile.browser selects in build.sbt. It refers
// to the environment's classes in both modes, so its small jar is always on the classpath.
// Playwright's Node driver is about 200 MB, so it is resolved only when smile.browser is set.
libraryDependencies += {
  val playwrightEnv = "org.wvlet.uni" %% "uni-jsenv-playwright" % "2026.1.41"
  if (sys.props.get("smile.browser").exists(_ != "false")) playwrightEnv
  else playwrightEnv.exclude("com.microsoft.playwright", "driver-bundle")
}

// The environment was compiled against an older Playwright. project/playwright.version names
// the release everything runs on. This override, the parity harness in build.sbt and the browser
// CI installs all read it, so they share one Chromium build.
dependencyOverrides ++= {
  val version = IO.read(baseDirectory.value / "playwright.version").trim
  Seq("playwright", "driver", "driver-bundle").map("com.microsoft.playwright" % _ % version)
}
