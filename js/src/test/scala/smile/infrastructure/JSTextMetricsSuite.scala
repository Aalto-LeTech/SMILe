package smile.infrastructure

import smile.colors.Color
import smile.modeling.{JSBoundaryCalculator, Pos}
import smile.pictures.*

/** A text is measured once, for its bounds and for the baseline both JavaScript renderers place it
  * by, however often it is drawn.
  */
class JSTextMetricsSuite extends munit.FunSuite:

  test("a text drawn on canvas and in SVG over 10 frames is measured once") {
    def label(frame: Int) = Text(
      pos = Pos(frame, 0),
      customBounds = None,
      content = "Measured once, drawn every frame",
      font = "Serif",
      size = 19,
      weight = 400,
      fill = fillWith(Color(0, 0, 0)),
      line = None
    )
    val before = JSTextMetrics.measurements
    val cached = JSBoundaryCalculator.textMetricsMeasurements.size
    for frame <- 1 to 10 do
      val _ = Picture(label(frame)).toBitmap()
      val _ = JSRenderer.createSvgFrom(label(frame))
    assertEquals(JSTextMetrics.measurements, before + 1)
    assertEquals(JSBoundaryCalculator.textMetricsMeasurements.size, cached + 1)
  }
end JSTextMetricsSuite
