package smile.pictures

import smile.modeling.{Bounds, Pos}
import smile.testing.{Cases, InkAssertions}

/** What must hold between arcs that describe the same curve in different ways: a sweep and the same
  * sweep run backwards from its other end, angles a whole turn apart, a rotation and a shift of the
  * start angle. Each relation is checked on the boundary and on the pixels.
  */
class ArcSweepLawsSuite extends munit.FunSuite with InkAssertions:

  private val sweeps: Seq[(Double, Double)] =
    Seq(
      (0.0, 90.0),
      (20.0, 110.0),
      (-50.0, -200.0),
      (300.0, 200.0),
      (100.0, 359.0),
      (-400.0, -100.0)
    )

  private val rotations = Seq(0.0, 25.0, 90.0, -140.0)

  private def arc(
      start: Double,
      extent: Double,
      rotation: Double = 0,
      width: Double = 60,
      height: Double = 34,
      fill: Option[FillStyle] = fillWith(Cases.red),
      line: Option[StrokeStyle] = None
  ) =
    val center = Pos(5, -3)
    Arc(center, width, height, start, extent, fill, line).rotateAround(rotation, center)

  /** How many pixels two renderings disagree on, in being painted or not. */
  private def differingPixels(one: Element, other: Element): Int =
    onInkCanvas(one)
      .pixels()
      .argbData
      .zip(onInkCanvas(other).pixels().argbData)
      .count((a, b) => ((a >>> 24) > 127) != ((b >>> 24) > 127))

  test("a sweep and the same sweep run backwards from its end cover the same curve") {
    for (start, extent) <- sweeps; rotation <- rotations do
      val forwards  = arc(start, extent, rotation)
      val backwards = arc(start + extent, -extent, rotation)
      val clue      = s"from $start by $extent, turned by $rotation"

      assertClose(backwards.bounds.get, forwards.bounds.get, 1e-9, clue)
      // The two paths are the same pixels up to the direction they were traced in, which
      // some rasterizers let flip a few pixels along the edge.
      assert(
        differingPixels(forwards, backwards) <= 24,
        s"$clue: ${differingPixels(forwards, backwards)}"
      )
  }

  test("the same holds for a stroke, whose caps swap ends with the sweep") {
    for (start, extent) <- sweeps; cap <- StrokeStyle.Cap.values do
      val forwards  = arc(start, extent, 25, fill = None, line = Cases.stroke(8, cap))
      val backwards = arc(start + extent, -extent, 25, fill = None, line = Cases.stroke(8, cap))
      assertClose(
        backwards.bounds.get,
        forwards.bounds.get,
        1e-9,
        s"from $start by $extent, $cap caps"
      )
  }

  test("angles a whole turn apart are the same angles") {
    for (start, extent) <- sweeps; rotation <- rotations; turns <- Seq(1, -2) do
      val clue = s"from $start by $extent, turned by $rotation, $turns turns on"
      assertClose(
        arc(start + 360.0 * turns, extent, rotation).bounds.get,
        arc(start, extent, rotation).bounds.get,
        1e-9,
        clue
      )
      assertClose(
        arc(start, extent, rotation + 360.0 * turns).bounds.get,
        arc(start, extent, rotation).bounds.get,
        1e-9,
        clue
      )
      assert(
        differingPixels(
          arc(start + 360.0 * turns, extent, rotation),
          arc(start, extent, rotation)
        ) <= 24,
        clue
      )
  }

  test("turning a circular arc is shifting its start angle") {
    // A circle looks the same turned any way, so turning a circular arc by some degrees shows
    // what adding those degrees to its start angle shows.
    for (start, extent) <- sweeps; rotation <- Seq(30.0, 90.0, -140.0) do
      val turned  = arc(start, extent, rotation, width = 50, height = 50, line = Cases.stroke(6))
      val shifted =
        arc(start + rotation, extent, 0, width = 50, height = 50, line = Cases.stroke(6))
      val clue = s"from $start by $extent, turned or shifted by $rotation"

      assertClose(turned.bounds.get, shifted.bounds.get, 1e-9, clue)
      assert(differingPixels(turned, shifted) <= 40, s"$clue: ${differingPixels(turned, shifted)}")
  }

  test("a sweep and the rest of the turn share their ends and fill the ellipse between them") {
    for (start, extent) <- Seq((20.0, 110.0), (-50.0, -200.0), (300.0, 200.0)) do
      val rest = if extent > 0 then extent - 360 else extent + 360

      // The two chord-closed segments tile the whole ellipse. Together their boundaries span
      // it, and their areas add up to its area.
      val whole                     = arc(0, 360)
      def painted(element: Element) =
        onInkCanvas(element).pixels().argbData.count(a => (a >>> 24) > 127)
      val union = Bounds(
        Pos(
          arc(start, extent).bounds.get.upperLeftCorner.x
            .min(arc(start, rest).bounds.get.upperLeftCorner.x),
          arc(start, extent).bounds.get.upperLeftCorner.y
            .min(arc(start, rest).bounds.get.upperLeftCorner.y)
        ),
        Pos(
          arc(start, extent).bounds.get.lowerRightCorner.x
            .max(arc(start, rest).bounds.get.lowerRightCorner.x),
          arc(start, extent).bounds.get.lowerRightCorner.y
            .max(arc(start, rest).bounds.get.lowerRightCorner.y)
        )
      )
      assertClose(union, whole.bounds.get, 1e-9, s"from $start by $extent and by $rest")
      assertEqualsDouble(
        (painted(arc(start, extent)) + painted(arc(start, rest))).toDouble,
        painted(whole).toDouble,
        painted(whole) * 0.06,
        s"from $start by $extent and by $rest"
      )
  }

  test("a boundary grows steadily into the whole ellipse's as the sweep closes") {
    // No jump at 360 degrees, where the arc becomes a full cycle and takes another branch.
    for rotation <- rotations; direction <- Seq(1.0, -1.0) do
      val almost = arc(40, direction * 359.9999, rotation).bounds.get
      val whole  = arc(40, direction * 360, rotation).bounds.get
      assertClose(almost, whole, 1e-3, s"turned by $rotation, direction $direction")
      assertEquals(arc(40, direction * 500, rotation).bounds.get, whole)
  }

  test("a butt-capped stroke closes into the whole ellipse's boundary too") {
    val stroke = Cases.stroke(8, StrokeStyle.Cap.Butt)
    for rotation <- rotations do
      assertClose(
        arc(40, 359.9999, rotation, fill = None, line = stroke).bounds.get,
        arc(40, 360, rotation, fill = None, line = stroke).bounds.get,
        1e-3,
        s"turned by $rotation"
      )
  }

  test("an arc's boundary lies within the whole ellipse's, and grows with the sweep") {
    for (start, extent) <- sweeps; rotation <- rotations do
      val whole   = arc(0, 360, rotation).bounds.get
      val partial = arc(start, extent, rotation).bounds.get
      val shorter = arc(start, extent / 2, rotation).bounds.get
      val clue    = s"from $start by $extent, turned by $rotation"

      def within(inner: Bounds, outer: Bounds): Boolean =
        inner.upperLeftCorner.x >= outer.upperLeftCorner.x - 1e-9 &&
          inner.upperLeftCorner.y >= outer.upperLeftCorner.y - 1e-9 &&
          inner.lowerRightCorner.x <= outer.lowerRightCorner.x + 1e-9 &&
          inner.lowerRightCorner.y <= outer.lowerRightCorner.y + 1e-9
      assert(within(partial, whole), s"$clue: $partial in $whole")
      assert(within(shorter, partial), s"$clue: $shorter in $partial")
  }

  test("an arc refuses a negative size and names it") {
    val widthError = intercept[IllegalArgumentException](arc(0, 90, width = -60))
    assert(
      widthError.getMessage.contains("width") && widthError.getMessage.contains("-60"),
      widthError
    )
    val heightError = intercept[IllegalArgumentException](arc(0, 90, height = -34))
    assert(
      heightError.getMessage.contains("height") && heightError.getMessage.contains("-34"),
      heightError
    )
  }

  test("a mirrored arc is the mirror image, sweep and all") {
    // A lopsided sweep, so that the mirror image is no rotation of the original.
    val original = arc(20, 110, 25, line = Cases.stroke(4))
    val pivot    = Pos(7, -3)

    for (h, v) <- Seq((-1.0, 1.0), (1.0, -1.0), (-1.0, -1.0)) do
      val mirrored = original.scaleAround(h, v, pivot)
      assert(mirrored.width > 0 && mirrored.height > 0, s"$mirrored keeps a positive size")

      val source                                    = onInkCanvas(original)
      val target                                    = onInkCanvas(mirrored)
      def inked(bitmap: Bitmap, spot: Pos): Boolean =
        bitmap
          .colorAt(
            (spot.x - inkCanvas.upperLeftCorner.x).floor.toInt,
            (spot.y - inkCanvas.upperLeftCorner.y).floor.toInt
          )
          .exists(_.opacity > 127)
      val spots       = for x <- -60 to 60 by 2; y <- -60 to 60 by 2 yield Pos(x + 0.5, y + 0.5)
      val disagreeing =
        spots.count(spot => inked(source, spot) != inked(target, spot.scaleBy(h, v, pivot)))
      val painted = spots.count(inked(source, _))

      assert(painted > 50, "the arc should cover a good many sample spots")
      assert(disagreeing <= painted / 10, s"by ($h, $v): $disagreeing of $painted spots disagree")
  }
