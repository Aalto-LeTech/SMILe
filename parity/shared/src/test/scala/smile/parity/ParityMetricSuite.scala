package smile.parity

import smile.testing.*

import smile.modeling.{Bounds, Pos}
import smile.testing.Expectation.*
import smile.pictures.*

/** The metric's own tests. It has to pass the same picture and fail, with a message that says what
  * is wrong, on each kind of difference the harness exists to find. The differences are made here
  * on purpose with one renderer, so the outcome does not depend on any two renderers agreeing.
  */
class ParityMetricSuite extends munit.FunSuite:

  private val frame = Bounds(Pos(-40, -30), Pos(40, 30))

  private def render(elements: Element*): Raster =
    Raster.of(Picture(elements*).crop(frame))

  private def ellipse(rotation: Double, at: Pos = Pos(0, 0)) =
    Arc(at, 60, 30, 0, 360, fillWith(ParityCases.red), None).rotateAround(rotation, at)

  private def failuresOf(a: Raster, b: Raster, expectation: Expectation): Seq[String] =
    ParityMetric.compare(a, b, expectation, translucent = false).failures

  test("a picture agrees with itself under every profile") {
    val picture = render(ellipse(30))
    for expectation <- Seq(Exact, AA, Gradient, Layout(1)) do
      assertEquals(failuresOf(picture, picture, expectation), Seq.empty, expectation.toString)
  }

  test("a rotation the other way is a failure") {
    val failures = failuresOf(render(ellipse(30)), render(ellipse(-30)), AA)
    assert(failures.exists(_.startsWith("flat areas")), failures.mkString("\n"))
    assert(failures.exists(_.startsWith("edges")), failures.mkString("\n"))
  }

  test("a sweep the other way is a failure") {
    def arc(arcAngle: Double) =
      render(Arc(Pos(0, 0), 60, 40, 0, arcAngle, fillWith(ParityCases.red), None))
    val failures = failuresOf(arc(90), arc(-90), AA)
    assert(failures.exists(_.startsWith("centroid")), failures.mkString("\n"))
  }

  test("a shift by a whole pixel is caught by the centroid though every edge finds a neighbor") {
    val failures = failuresOf(render(ellipse(30)), render(ellipse(30, Pos(1, 0))), AA)
    assert(failures.exists(_.startsWith("centroid")), failures.mkString("\n"))
  }

  test("a shift by half a pixel is caught by the centroid") {
    val failures = failuresOf(render(ellipse(30)), render(ellipse(30, Pos(0.5, 0))), AA)
    assert(failures.exists(_.startsWith("centroid")), failures.mkString("\n"))
  }

  test("a color a few steps off fails in the flat areas, and passes where gradients may round") {
    def rectangle(redness: Int) =
      render(Rectangle(40, 20, Pos(0, 0), fillWith(smile.colors.Color(redness, 0, 0)), None))
    val failures = failuresOf(rectangle(255), rectangle(250), AA)
    assert(failures.exists(_.startsWith("flat areas")), failures.mkString("\n"))
    assertEquals(failuresOf(rectangle(255), rectangle(250), Gradient), Seq.empty)
  }

  test("a stroke one pixel wider is a failure") {
    def stroked(width: Double) =
      render(Circle(Pos(0, 0), 15, None, Some(StrokeStyle(ParityCases.blue, width))))
    val failures = failuresOf(stroked(3), stroked(4), AA)
    assert(failures.exists(_.startsWith("alpha mass")), failures.mkString("\n"))
  }

  test("the exact profile fails on a single pixel") {
    val picture = render(ellipse(30))
    val altered = picture.copy(argb = picture.argb.clone())
    altered.argb(30 * picture.width + 40) = 0xff0000f0
    val failures = failuresOf(picture, altered, Exact)
    assert(failures.exists(_.startsWith("pixels: 1 differ")), failures.mkString("\n"))
    assertEquals(
      ParityMetric.compare(picture, altered, Exact, translucent = false).mismatchBounds,
      Some((40, 30, 40, 30))
    )
  }

  test("different dimensions fail, except where each platform measures for itself") {
    val small = Raster.of(Picture(ellipse(0)))
    val large =
      Raster.of(Picture(ellipse(0)).crop(Bounds(Pos(0, 0), 64.0, 34.0)))
    assert(failuresOf(small, large, AA).exists(_.startsWith("dimensions")))
    assertEquals(failuresOf(small, large, Layout(1)), Seq.empty)
  }

  test("a known difference is measured against the profile that would hold without it") {
    val known = KnownDiffers("made up", AA, AA)
    assert(failuresOf(render(ellipse(30)), render(ellipse(-30)), known).nonEmpty)
    assertEquals(failuresOf(render(ellipse(30)), render(ellipse(30)), known), Seq.empty)
  }

  test("a known difference passes where it agrees or stays within its bound, and fails beyond") {
    val (a, b)  = (render(ellipse(30)), render(ellipse(-30)))
    val within  = KnownDiffers("made up", AA, Layout(10))
    val beyond  = KnownDiffers("made up", AA, AA)
    val agreed  = ParityMetric.judge(a, a, beyond, translucent = false)
    val bounded = ParityMetric.judge(a, b, within, translucent = false)
    val failed  = ParityMetric.judge(a, b, beyond, translucent = false)
    assertEquals((agreed.passed, agreed.outcome), (true, "agree"))
    assertEquals((bounded.passed, bounded.outcome), (true, "differ as known, within the bound"))
    assertEquals((failed.passed, failed.outcome), (false, "disagree"))
  }
