val scala3Version = "3.10.1"
val smileVersion  = "2.0.0"

// The bytecode targets Java 25, so JDK 25 is the minimum. An older JDK fails with
// UnsupportedClassVersionError.
ThisBuild / scalacOptions += "-release:25"
ThisBuild / javacOptions ++= Seq("--release", "25")

// The Scala.js suites run under jsdom by default because it is fast, needs no browser download,
// and gets a working canvas from @napi-rs/canvas. Passing -Dsmile.browser=true runs the same
// suites in a real headless browser through Playwright, which has the SVG text layout and the
// fetch that jsdom lacks. -Dsmile.browser.name picks chromium, webkit or firefox. Chromium is the
// default and the only one supported.
lazy val useBrowserJSEnv  = sys.props.get("smile.browser").exists(_ != "false")
lazy val browserJSEnvName = sys.props.getOrElse("smile.browser.name", "chromium")

// The environment the Scala.js suites run in. The browser environment keeps no failure artifacts.
// The artifact would be a Playwright trace of the whole run, and that trace shows a blank page
// without the canvases the suites draw on.
def testJSEnv: org.scalajs.jsenv.JSEnv =
  if (useBrowserJSEnv) {
    import wvlet.uni.jsenv.playwright.{PlaywrightConfig, PlaywrightJSEnv}
    new PlaywrightJSEnv(
      PlaywrightConfig(browserName = browserJSEnvName, headless = true)
        .withCaptureArtifactsOnFailure(false)
    )
  } else new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

lazy val SMILe = project
  .in(file("."))
  .settings(
    // sbt keeps each project's output in a directory named after the project, and the library
    // itself is the one called SMILe.
    name         := "SMILe-root",
    publish      := {},
    publishLocal := {},
    scalaVersion := scala3Version
  )
  .settings(
    // quick runs every test except those tagged smile.testing.Slow, in every project and on both
    // platforms. testFull and CI run the Slow ones too.
    addCommandAlias("quick", "testOnly * -- --exclude-tags=Slow"),
    addCommandAlias("quickJVM", "smileJVM/testOnly * -- --exclude-tags=Slow"),
    addCommandAlias("quickJS", "smileJS/testOnly * -- --exclude-tags=Slow"),
    // fullCheck runs what CI checks, which is the format of the sources and of the build files and
    // every test.
    addCommandAlias("fullCheck", ";scalafmtCheckAll;scalafmtSbtCheck;testFull"),
    docPictures := Def.uncached(docPicturesTask.value),
    apiDocs     := Def.uncached(apiDocsTask.value)
  )
  .aggregate(
    smile.js,
    smile.jvm,
    testing.js,
    testing.jvm,
    parity.js,
    parity.jvm,
    apidoc
  )

lazy val smile = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name    := "SMILe",
    version := smileVersion,
    // Nothing is published. Other builds use SMILe as source.
    publish / skip := true,
    scalaVersion   := scala3Version,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:strictEquality",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-Wconf:msg=match may not be exhaustive:e"
    ),
    // Source paths are recorded from the repository's root, which the documentation's links to
    // the sources follow.
    scalacOptions ++= Seq("-sourceroot", (ThisBuild / baseDirectory).value.getPath),
    // Warnings fail the build in CI and stay warnings locally.
    scalacOptions ++= (if (sys.env.contains("CI")) Seq("-Werror") else Seq.empty),
    // Initialization checking covers the library sources only, because munit suites register
    // their tests during construction, which the checker's this-escape analysis cannot verify.
    // The Test subtraction is needed because Test delegates to Compile for unset keys.
    Compile / scalacOptions += "-Wsafe-init",
    Test / scalacOptions := (Test / scalacOptions).value.filterNot(_ == "-Wsafe-init"),
    buildInfoKeys        := Seq[BuildInfoKey](version),
    buildInfoPackage     := "smile"
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.github.weisj" % "jsvg"  % "2.2.0",
      "org.scalameta"   %% "munit" % "1.3.6" % Test
    ),
    // The fixtures write pictures of failed comparisons. The parity harness uses them too.
    Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "testFixtures" / "scala",
    Test / fork              := true,
    Test / parallelExecution := false,
    Test / javaOptions ++= Def.uncached {
      Seq(
        "-Djava.awt.headless=true",
        s"-Dsmile.visual.output=${target.value / "visual-test-results" / "test"}"
      )
    }
  )
  .jvmSettings(
    // One site documents both platforms. It holds the JVM's classes and the public API that only
    // Scala.js has, whose comments say so.
    Compile / doc / tastyFiles := Def.uncached {
      val jsDir = (LocalProject("smileJS") / Compile / classDirectory).value
      val _     = (LocalProject("smileJS") / Compile / compile).value
      (Compile / doc / tastyFiles).value ++ Seq(
        "smile/infrastructure/JSRenderer.tasty",
        "smile/pictures/JSBitmapConversions$package.tasty",
        "smile/pictures/SvgPictureJS$package.tasty"
      ).map(jsDir / _)
    },
    Compile / doc / dependencyClasspath ++= Def.uncached {
      val converter = fileConverter.value
      val _         = (LocalProject("smileJS") / Compile / compile).value
      val jsClasses = (LocalProject("smileJS") / Compile / classDirectory).value
      (LocalProject("smileJS") / Compile / dependencyClasspath).value :+
        Attributed.blank(converter.toVirtualFile(jsClasses.toPath))
    }
  )
  .jsSettings(
    libraryDependencies += "org.scala-js"  %% "scalajs-dom" % "2.8.1",
    libraryDependencies += "com.lihaoyi"   %% "scalatags"   % "0.13.1",
    libraryDependencies += "org.scalameta" %% "munit"       % "1.3.6" % Test,
    // Suites run one at a time because they share one browser page or jsdom instance.
    Test / parallelExecution := false,
    // JS tests run in jsdom with @napi-rs/canvas from package.json, which gives them a working
    // canvas, so buffer- and rendering-dependent tests can run on both platforms. The jsdom
    // environment cannot load ES modules, so tests link as a classic script. The
    // library's own link keeps the ESModule kind from the shared linker config.
    Test / jsEnv := Def.uncached(testJSEnv),
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withSourceMap(true)
    }
  )

// The cross-backend parity harness renders one catalog of pictures with every backend and
// compares the results. It is a separate, unpublished project because the comparison needs the
// catalog both on the JVM, next to the AWT renderer, and as a Scala.js bundle a browser can load.
//
// Without a browser, its suites hold each platform's bitmap renderer to the catalog's
// hand-derived probes. -Dsmile.parity=true adds the comparison. The JVM suite in src/browserTest
// drives a headless Chromium through Playwright, loads the bundle there, and compares AWT, canvas
// and SVG in one process. Like the browser test environment above, Playwright is resolved only
// when asked for.
lazy val useParityBrowser = sys.props.get("smile.parity").exists(_ != "false")
// The Playwright release that project/plugins.sbt pins for the browser test environment.
lazy val playwrightVersion = IO.read(file("project/playwright.version")).trim
lazy val parityBundle      =
  taskKey[Unit]("Links the Scala.js bundle the three-way parity tier loads into a browser")

lazy val parity = crossProject(JSPlatform, JVMPlatform)
  .in(file("parity"))
  .dependsOn(smile, testing)
  .settings(
    name           := "SMILe-parity",
    scalaVersion   := scala3Version,
    publish / skip := true,
    publish        := {},
    publishLocal   := {},
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Wunused:linted"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    // The suites share one output directory.
    Test / parallelExecution := false
  )
  .jsSettings(
    Test / jsEnv := Def.uncached(testJSEnv),
    // Linked as a classic script because the jsdom test environment loads no ES modules and the
    // three-way tier adds the main bundle to a page with a plain script tag.
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) }
  )
  .jvmSettings(
    Test / fork := true,
    Test / unmanagedSourceDirectories +=
      (ThisBuild / baseDirectory).value / "jvm" / "src" / "testFixtures" / "scala",
    Test / unmanagedSourceDirectories ++= {
      if (useParityBrowser) Seq(baseDirectory.value / "src" / "browserTest" / "scala") else Nil
    },
    libraryDependencies ++= {
      if (useParityBrowser)
        Seq("com.microsoft.playwright" % "playwright" % playwrightVersion % Test)
      else Nil
    },
    Test / javaOptions ++= Def.uncached {
      Seq(
        "-Djava.awt.headless=true",
        s"-Dsmile.visual.output=${target.value / "visual-test-results" / "parity"}",
        s"-Dsmile.parity.bundle=${(LocalProject("parityJS") / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value / "main.js"}"
      )
    },
    parityBundle := Def.taskDyn {
      if (useParityBrowser)
        Def.task { val _ = (LocalProject("parityJS") / Compile / fastLinkJS).value }
      else Def.task(())
    }.value,
    // The incremental test and testFull, which runs every suite, both need the bundle.
    Test / test     := (Test / test).dependsOn(parityBundle).evaluated,
    Test / testFull := Def.uncached { (Test / testFull).dependsOn(parityBundle).value }
  )

// The pictures the doc comments show are drawn by SMILe itself, in apidoc/. A comment shows one
// as ![alt](pictures/name.png) right after a code block labelled scala, where name is a val of
// the block. docPictures draws each into a pictures directory beside the source that shows it,
// which is where an IDE looks for it when it renders the comment. The files are not committed.
lazy val docPictures =
  taskKey[Unit]("Draws the pictures the doc comments show, each beside its source")

// The API documentation as it is published, which is Scaladoc's site for both platforms with
// those pictures. Scaladoc copies a picture's path onto pages at every depth, so each page's path
// is made relative here, and each picture, drawn at two device pixels per logical pixel, is given
// its logical width.
lazy val apiDocs =
  taskKey[File]("Builds the API documentation with its pictures into target/api-docs")

// The sources whose comments show pictures, and the landing page of the documentation.
def picturedSources(root: File): Seq[File] =
  Seq("shared", "jvm").flatMap(side => (root / side / "src" / "main" ** "*.scala").get()) :+
    root / "apidoc" / "root.md"

def drawnPictures(root: File): Seq[File] =
  Seq(root / "shared" / "src" / "main", root / "jvm" / "src" / "main", root / "apidoc")
    .flatMap(base => (base ** "pictures" * "*.png").get())

// What the pictures were last drawn from, so that loading the build draws them only when a
// comment's code has changed.
def picturesStamp(root: File): File = root / "target" / "doc-pictures.stamp"

def docPicturesTask = Def.taskDyn {
  val root = (ThisBuild / baseDirectory).value
  Def.task {
    (LocalProject("apidoc") / Compile / runMain)
      .toTask(s" smile.apidoc.DocPictures ${root.getPath}")
      .value
    IO.write(picturesStamp(root), picturedSnippets(root, picturedSources(root)))
  }
}

def apiDocsTask = Def.task {
  val root = (ThisBuild / baseDirectory).value
  val out  = root / "target" / "api-docs"
  val site = (LocalProject("smileJVM") / Compile / doc).value
  docPictures.value
  val pictures = drawnPictures(root)
  IO.delete(out)
  IO.copyDirectory(site, out)
  pictures.foreach(file => IO.copyFile(file, out / "pictures" / file.getName))
  val widths = pictures.map { file =>
    file.getName -> javax.imageio.ImageIO.read(file).getWidth / 2
  }.toMap
  // A white card, so that a picture reads the same on the dark theme.
  val pictureStyle = "height: auto; padding: 12px; border-radius: 6px; background: #fff"
  val shown        = """<img src="pictures/([^"]+)"""".r
  for (page <- (out ** "*.html").get()) {
    val depth = IO.relativize(out, page).get.count(_ == '/')
    val html  = IO.read(page)
    val fixed = shown.replaceAllIn(
      html,
      found => {
        val name  = found.group(1)
        val width = widths.getOrElse(name, sys.error(s"$page shows a picture nothing draws: $name"))
        scala.util.matching.Regex.quoteReplacement(
          s"""<img width="$width" style="$pictureStyle" src="${"../" * depth}pictures/$name""""
        )
      }
    )
    if (fixed != html) IO.write(page, fixed)
  }
  out
}

// Loading the build draws the pictures when the code they are drawn from has changed, so that an
// IDE shows them without anyone asking. CI and -Dsmile.pictures=false leave them undrawn.
Global / onLoad := {
  val previous = (Global / onLoad).value
  previous.andThen { state =>
    val root  = Project.extract(state).get(ThisBuild / baseDirectory)
    val stale = scala.util
      .Try {
        val stamp = picturesStamp(root)
        !stamp.exists || IO.read(stamp) != picturedSnippets(root, picturedSources(root))
      }
      .getOrElse(false)
    val wanted = !sys.env.contains("CI") && !sys.props.get("smile.pictures").contains("false")
    if (stale && wanted) "docPictures" :: state else state
  }
}

// The code blocks of the doc comments that a picture follows, as the source of an object whose
// `all` gives each pictured value with its name and the directory of the source that shows it.
def picturedSnippets(root: File, files: Seq[File]): String = {
  val picture = """!\[[^\]]*\]\(pictures/(\w+)\.png\)""".r
  val blocks  = files.sortBy(_.getPath).flatMap { file =>
    val lines = IO.readLines(file).map(_.replaceFirst("""^\s*\*( |$)""", ""))
    // Scaladoc compiles a comment's code inside the package it documents.
    val scope = lines.find(_.startsWith("package ")).map(_.replaceFirst("package", "import") + ".*")
    val home  = IO.relativize(root, file.getParentFile).get
    lines.indices.filter(lines(_).trim == "```scala").flatMap { start =>
      val end = lines.indexWhere(_.trim == "```", start + 1)
      // The paragraph after the block, whose lines the formatter may have wrapped.
      val after = lines.drop(end + 1).dropWhile(_.trim.isEmpty).takeWhile(_.trim.nonEmpty)
      val names = picture.findAllMatchIn(after.mkString(" ")).map(_.group(1)).toSeq
      if (names.isEmpty) None else Some((home, names, scope.toSeq ++ lines.slice(start + 1, end)))
    }
  }
  val names      = blocks.flatMap(_._2)
  val duplicates = names.diff(names.distinct)
  if (duplicates.nonEmpty) sys.error(s"Pictures drawn twice: ${duplicates.mkString(", ")}")
  val entries = blocks.map { case (home, names, code) =>
    val drawn = names.map(name => s"""("$home", "$name", $name)""").mkString(", ")
    (code :+ s"Seq[(String, String, smile.pictures.Element)]($drawn)")
      .map("      " + _)
      .mkString("\n")
  }
  s"""package smile.apidoc
     |
     |// Generated from the doc comments by build.sbt.
     |object Pictured:
     |  val all: Seq[(String, String, smile.pictures.Element)] = Seq(
     |${entries.map(entry => s"    locally {\n$entry\n    }").mkString(",\n")}
     |  ).flatten
     |""".stripMargin
}

// Draws the pictures the doc comments show, when docPictures runs it.
lazy val apidoc = project
  .in(file("apidoc"))
  .dependsOn(smile.jvm)
  .settings(
    name           := "SMILe-apidoc",
    scalaVersion   := scala3Version,
    publish / skip := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    fork := true,
    run / javaOptions += "-Djava.awt.headless=true",
    Compile / sourceGenerators += Def.task {
      val root     = (ThisBuild / baseDirectory).value
      val pictured = (Compile / sourceManaged).value / "smile" / "apidoc" / "Pictured.scala"
      IO.write(pictured, picturedSnippets(root, picturedSources(root)))
      Seq(pictured)
    }.taskValue
  )

// Tools for checking pictures in tests. They render a picture into plain pixels, compare two
// renderings with antialiasing tolerated, ask where colors are, and encode pixels as PNG. The
// parity harness compares the backends with it, and other projects can check pictures with it.
lazy val testing = crossProject(JSPlatform, JVMPlatform)
  .in(file("testing"))
  .dependsOn(smile)
  .settings(
    name           := "SMILe-testing",
    version        := smileVersion,
    publish / skip := true,
    scalaVersion   := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Wunused:linted"),
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test
  )
  .jsSettings(
    Test / parallelExecution := false,
    Test / jsEnv             := Def.uncached(testJSEnv),
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) }
  )
  .jvmSettings(Test / javaOptions += "-Djava.awt.headless=true", Test / fork := true)

// The API documentation. Scaladoc compiles the code examples in the comments, links members to
// their sources and to the Java, Scala and Scala.js documentation, and sorts members into the
// groups their comments name.
Compile / doc / scalacOptions ++= Seq(
  "-project-version",
  smileVersion,
  "-project-logo",
  "images/logo.png",
  "-project-footer",
  "Scala Media Interactive Learning / Aalto LeTech",
  "-social-links:github::https://github.com/Aalto-LeTech/SMILe",
  "-source-links:github://Aalto-LeTech/SMILe/main",
  "-doc-root-content",
  ((ThisBuild / baseDirectory).value / "apidoc" / "root.md").getPath,
  "-doc-canonical-base-url",
  "https://aalto-letech.github.io/SMILe/",
  "-skip-by-regex:smile\\.BuildInfo.*",
  "-groups",
  "-snippet-compiler:compile",
  // The Java documentation is that of Java 8, the last one whose pages are not split by module,
  // which Scaladoc's links cannot follow.
  "-external-mappings:" + Seq(
    "org/scalajs/dom/.*::scaladoc3::https://javadoc.io/doc/org.scala-js/scalajs-dom_sjs1_3/2.8.1/",
    "scala/(?!scalajs/).*::scaladoc3::https://scala-lang.org/api/3.x/",
    ".*/modules/java\\..*::javadoc::https://docs.oracle.com/javase/8/docs/api/"
  ).mkString(",")
)
