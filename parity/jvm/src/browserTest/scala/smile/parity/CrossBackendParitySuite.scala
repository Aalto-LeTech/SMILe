package smile.parity

import smile.testing.{VisualArtifacts, VisualSuite}

import java.awt.GraphicsEnvironment
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import smile.testing.*

/** The three-way tier. Every case of the catalog is rendered at each of its scales by AWT in this
  * JVM and by the canvas and SVG backends in the headless Chromium of
  * [[smile.parity.BrowserSession]], then compared pair by pair with [[smile.testing.ParityMetric]],
  * the pairs with SVG at the scales [[smile.parity.ParityCase.pairsAt]] names. It is compiled and
  * run only with `-Dsmile.parity=true`.
  *
  * A pair that disagrees writes both pictures, a picture of the difference, the measurements and
  * the SVG markup into the visual report at
  * `target/out/jvm/scala-3.10.1/smile-parity/visual-test-results/parity`. A pair that is known to
  * differ passes where it agrees or differs within the known difference's bound, and where it
  * differs, writes them too. `summary.txt` there lists what was measured for every pair, agreeing
  * or not, and the outcome of every known difference.
  *
  * A browser that does not start is a failure, and so is any case that was not compared.
  */
class CrossBackendParitySuite extends VisualSuite:

  private lazy val session: Try[BrowserSession] =
    Try(new BrowserSession(Paths.get(sys.props("smile.parity.bundle"))))

  private lazy val awtFonts: Set[String] =
    GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.toSet

  /** The first candidate font both AWT and the browser have. */
  private lazy val commonFont: Option[String] =
    ParityCases.FontCandidates.find(font => awtFonts(font) && session.get.hasFont(font))

  private val renders   = mutable.Map.empty[String, Map[Backend, Raster]]
  private val svgTexts  = mutable.Map.empty[String, String]
  private val summary   = mutable.ArrayBuffer.empty[String]
  private val compared  = mutable.Set.empty[(String, Double, Backend, Backend)]
  private val unmetNeed = mutable.Map.empty[String, String]

  private def startedBrowser(): BrowserSession =
    session match
      case Success(browser) => browser
      case Failure(error)   => fail(s"The browser the parity tier needs did not start: $error", error)

  private def rendersOf(
      parityCase: ParityCase,
      browser: BrowserSession,
      font: String,
      scale: Double
  ) =
    val name = parityCase.nameAt(scale)
    renders.getOrElseUpdate(
      name, {
        val inBrowser = browser.render(parityCase.id, font, scale)
        svgTexts(name) = inBrowser.svgText
        Map(
          Backend.Awt    -> Raster.of(parityCase.picture(), parityCase.settings, scale),
          Backend.Canvas -> inBrowser.canvas,
          Backend.Svg    -> inBrowser.svg
        )
      }
    )

  override def afterAll(): Unit =
    val _ = Files.writeString(
      VisualArtifacts.outputRoot.resolve("summary.txt"),
      summary.mkString("\n\n") + "\n",
      StandardCharsets.UTF_8
    )
    session.foreach(_.close())
    super.afterAll()

  test("the browser's catalog is this JVM's catalog") {
    val browser = startedBrowser()
    assertEquals(browser.ids, ParityCases.all.map(_.id))
    println(
      s"Parity tier: AWT on Java ${sys.props("java.version")} against ${browser.browserVersion}; " +
        s"text cases set in ${commonFont.getOrElse("no common font - SKIPPED")}"
    )
  }

  for
    listed          <- ParityCases.all
    scale           <- listed.scales
    (first, second) <- listed.pairsAt(scale)
  do
    val name = s"${first.label} and ${second.label} agree on ${listed.id} at " +
      s"${ParityCase.label(scale)} (${listed.catches})"
    test(name) {
      val browser = startedBrowser()
      if listed.needs(Needs.CommonFont) && commonFont.isEmpty then
        val reason =
          s"none of the fonts ${ParityCases.FontCandidates.mkString(", ")} is known to both " +
            "AWT and the browser"
        unmetNeed(listed.id) = reason
        assume(false, s"NOT COMPARED: $reason")

      val font        = commonFont.getOrElse(ParityCases.FontCandidates.head)
      val parityCase  = ParityCases.byId(listed.id, font)
      val expectation = parityCase.expectationFor(first, second, scale)
      val rendered    = rendersOf(parityCase, browser, font, scale)
      val verdict     = ParityMetric.judge(
        rendered(first),
        rendered(second),
        expectation,
        parityCase.translucent
      )

      val pairName = s"${parityCase.nameAt(scale)}--${first.label}-vs-${second.label}"
      val known    = expectation match
        case Expectation.KnownDiffers(reason, _, _, _) =>
          println(s"Known difference, $pairName: ${verdict.outcome}")
          s"known to differ: $reason\n"
        case _ => ""
      val report =
        s"case: ${parityCase.id} (${parityCase.catches})\nscale: ${ParityCase.label(scale)}\n" +
          s"pair: ${first.label} against ${second.label}\n$known${verdict.report}\n"
      summary += s"$pairName\n$report".trim
      compared += ((parityCase.id, scale, first, second))

      // A known difference within its bound is written to the visual report too.
      if !verdict.diff.passed then
        val diff      = verdict.bound.filter(_ => verdict.passed).getOrElse(verdict.diff)
        val directory = VisualArtifacts.persistComparison(
          pairName,
          first.label  -> BrowserSession.imageOf(rendered(first)),
          second.label -> BrowserSession.imageOf(rendered(second)),
          BrowserSession.imageOf(diff.diff),
          report + "markup: picture.svg\n",
          Seq("picture.svg" -> svgTexts(parityCase.nameAt(scale)))
        )
        if !verdict.passed then
          fail(
            s"${first.label} and ${second.label} disagree on ${parityCase.id}:\n" +
              verdict.diff.failures.mkString("\n") +
              verdict.bound
                .map(bound =>
                  "\nbeyond the known difference's bound:\n" + bound.failures.mkString("\n")
                )
                .getOrElse("") +
              s"\nvisual report: ${directory.getParent.resolve("index.html")}"
          )
    }

  test("no case went uncompared") {
    val expected =
      for
        parityCase      <- ParityCases.all
        scale           <- parityCase.scales
        (first, second) <- parityCase.pairsAt(scale)
        if !unmetNeed.contains(parityCase.id)
      yield (parityCase.id, scale, first, second)
    val missing = expected.filterNot(compared)
    val planned =
      ParityCases.all.map(c => c.scales.map(scale => c.pairsAt(scale).size).sum).sum
    println(
      s"Parity tier: ${compared.size} of $planned comparisons made" +
        unmetNeed.map((id, reason) => s"\n  NOT COMPARED $id: $reason").mkString
    )
    assert(missing.isEmpty, s"not compared: ${missing.mkString(", ")}")
    assert(unmetNeed.isEmpty, s"skipped for an unmet need: ${unmetNeed.mkString(", ")}")
  }
