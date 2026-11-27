package smile.parity

import smile.testing.*

import scala.concurrent.ExecutionContext

/** Holds this platform's bitmap renderer, AWT on the JVM and canvas on Scala.js, to the probes of
  * every case at each of its scales. A probe is a point whose ink was worked out by hand from the
  * documented behavior. A comparison between backends cannot see a mistake they all make alike, and
  * this suite can.
  *
  * The text cases carry no probes, since where glyphs fall depends on the platform's font. They are
  * left to the comparing suites.
  */
class ParityOracleSuite extends munit.FunSuite:

  private given ExecutionContext = munitExecutionContext

  /** Every case but the text cases. */
  private val probed =
    ParityCases.all.filter(c => !c.needs(Needs.SvgTextLayout) && !c.needs(Needs.CommonFont))

  test("the catalog's ids are unique and usable as directory names") {
    val ids = ParityCases.all.map(_.id)
    assertEquals(ids.distinct, ids)
    assert(ids.forall(_.matches("[a-z0-9-]+")), ids.mkString(", "))
  }

  test("rounding to device pixels is read from the plan") {
    def named(id: String) = ParityCases.byId(id, ParityCases.FontCandidates.head)
    assert(named("viewport-crop").roundsToDevicePixels(2.0))
    assert(!named("rect-fill").roundsToDevicePixels(2.0))
    assert(named("mask").roundsToDevicePixels(2.0))
    assert(named("clip-rotated").roundsToDevicePixels(2.0))
    assert(named("wrap-fractional-period").roundsToDevicePixels(2.0))
    // Resampled at 2x, but snapped in the plan at 1x, which the SVG backend draws.
    assert(named("sprite-odd-centered").roundsToDevicePixels(2.0))
    assert(!named("sprite-turned").roundsToDevicePixels(2.0))
  }

  // So that a change to the planner that moves a case in or out of SVG's comparisons at 2x shows
  // in review.
  test("the cases that round to device pixels at 2x") {
    assertEquals(
      probed.filter(_.roundsToDevicePixels(2.0)).map(_.id),
      Seq(
        "point",
        "bitmap-int",
        "bitmap-subpixel",
        "bitmap-direct",
        "bitmap-dense",
        "bitmap-dense-nearest",
        "bitmap-alpha",
        "mask",
        "mask-inverted",
        "mask-black",
        "mask-gradient",
        "mask-nested",
        "mask-subpixel",
        "mask-past-content",
        "deep-masks",
        "deep-masked",
        "deep-masked-flat",
        "deep-turned-crops",
        "deep-turned-wraps",
        "viewport-crop",
        "viewport-larger",
        "subpixel",
        "clip-rotated",
        "clip-fractional",
        "wrap-fractional-offset",
        "wrap-fractional-period",
        "bitmap-crop-smooth",
        "sprite-odd-centered",
        "sprite-odd-pixelated",
        "clip-in-mask",
        "mask-in-clip",
        "svg-as-mask",
        "svg-masked"
      )
    )
  }

  for
    parityCase <- probed
    scale      <- parityCase.scales
  do
    test(
      s"${parityCase.id} shows what it should at ${ParityCase.label(scale)} " +
        s"(${parityCase.catches})"
    ) {
      Preparation
        .ready(parityCase)
        .map: _ =>
          val rendered = Raster.of(parityCase.picture(), parityCase.settings, scale)
          val missed   = RasterProbes.probeFailures(parityCase, rendered, scale)
          assert(missed.isEmpty, missed.mkString("\n"))
    }
