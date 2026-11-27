package smile.infrastructure

import org.scalajs.dom
import smile.colors.Color
import smile.modeling.{Bounds, JSBoundaryCalculator, Pos}
import smile.pictures.*

import scala.util.Try

/** JS-only rendering behavior, exercised on a canvas under jsdom or in a browser. The SVG
  * renderer's markup is `SvgPlanRendererSuite`'s and `SvgAgreementSuite`'s.
  */
class JSRenderingSuite extends munit.FunSuite with smile.testing.InkAssertions:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  test("synchronous image loading without preloading fails with guidance") {
    val error = intercept[UnsupportedOperationException]:
      Bitmap("never-loaded.png")

    assert(error.getMessage.contains("Bitmap.load"), s"unexpected message: ${error.getMessage}")

    val animationError = intercept[UnsupportedOperationException]:
      AnimatedPicture.fromGifPath("never-loaded.gif")

    assert(animationError.getMessage.contains("AnimatedPicture.load"))
  }

  test("canvas text with customBounds paints within the layout box") {
    val text = Text(
      pos = Pos(20, 10),
      customBounds = Some(Bounds(Pos(0, 0), Pos(40, 20))),
      content = "wide",
      font = "sans-serif",
      size = 8,
      weight = 400,
      fill = fillWith(red),
      line = None
    )

    val picture = Picture(text).crop(Bounds(Pos(0, 0), Pos(40, 20)))
    val bitmap  = picture.toBitmap()

    val paintedPixels = bitmap.pixels().colors.count(_.opacity > 0)
    assert(paintedPixels > 0, "scaled text should paint pixels inside the layout box")

    // A crop the size of the box shows what is painted inside it and hides what is not.
    // On a canvas with room to spare, the glyphs stay within the box and are stretched over most
    // of its width, because the canvas scales them by their measured advance. A canvas that sizes
    // glyphs by the vertical scale factor alone overflows the box by a pixel. See
    // InkAssertions.stretchesGlyphsFaithfully.
    val ink = inkBounds(text).get
    assert(ink.width > 30, s"the glyphs should be stretched across the box: $ink")
    if stretchesGlyphsFaithfully then
      assertInkWithinBoundary("canvas text on a 40 x 20 box", text)
      assertInkWithinBoundary("the same text, turned and moved", text.rotateAround(40, Pos(-5, 9)))
  }

  test("a failed text measurement leaves no probe SVG behind") {
    // JSBoundaryCalculator measures the glyphs through a hidden <svg> it appends to the document
    // body. Under jsdom the measurement throws because there is no getBBox, and a text's bounds
    // are a lazy val whose failed initializer runs again on every access, so a probe node left
    // behind accumulates. This asserts that the node goes whether or not the measurement
    // succeeds, which holds in a real browser too, where it returns a box.
    val text = Text(
      pos = Pos(0, 0),
      customBounds = None,
      content = "SMILe probe",
      font = "sans-serif",
      size = 20,
      weight = 400,
      fill = fillWith(red),
      line = None
    )

    val before       = dom.document.body.querySelectorAll("svg").length
    val cachedBefore = JSBoundaryCalculator.textMetricsMeasurements.size
    val measured     = Try(text.bounds)

    assertEquals(
      dom.document.body.querySelectorAll("svg").length,
      before,
      "the measurement's probe node should not outlive the measurement"
    )
    // Nor may a failed measurement be remembered, so the next attempt measures again.
    if measured.isFailure then
      assertEquals(JSBoundaryCalculator.textMetricsMeasurements.size, cachedBefore)
  }

  test("SVG strokes use the miter limit the other backends use") {
    val triangle = Triangle(10, Pos(0, 0), None, Some(StrokeStyle(blue, width = 2)))
    val svg      = JSRenderer.createSvgFrom(triangle).outerHTML

    // SVG's own default is 4, and canvas's and Java2D's is 10. Bounds can account for one.
    assert(svg.contains("stroke-miterlimit=\"10\""), svg)
  }
end JSRenderingSuite
