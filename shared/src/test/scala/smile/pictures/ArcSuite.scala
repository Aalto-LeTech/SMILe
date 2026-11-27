package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.testing.Cases.stroke as strokeOf
import smile.testing.InkAssertions

/** Arcs and ellipses: a partial sweep's tight bounds (issue #13), rotated and stroked, and the
  * pixels inside them. A sweep runs counterclockwise from three o'clock, and the renderers paint
  * exactly the boundary. [[smile.pictures.ArcSweepLawsSuite]] relates arcs that describe the same
  * curve in different ways.
  */
class ArcSuite extends munit.FunSuite with InkAssertions:

  private val red = Color(255, 0, 0)

  test("#13 a partial arc has tight bounds instead of the full ellipse's bounds") {
    val quarterArc = Arc(
      pos = Pos.Origin,
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = 90,
      fill = fillWith(red),
      line = None
    )

    // Sweeps run counterclockwise from three o'clock. This is the upper right quarter.
    assertEquals(quarterArc.bounds.get, Bounds(Pos(0, -40), Pos(50, 0)))

    // A negative sweep covers the mirrored quarter.
    val negativeQuarter = Arc(
      pos = Pos.Origin,
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = -90,
      fill = fillWith(red),
      line = None
    )

    assertEquals(negativeQuarter.bounds.get, Bounds(Pos(0, 0), Pos(50, 40)))
  }

  test("canvas rendering honors a turned Arc") {
    val rotatedEllipse =
      Arc(pos = Pos.Origin, width = 6, height = 2, fill = fillWith(red), line = None)
        .rotateAround(90, Pos.Origin)

    val bitmap = rotatedEllipse.toBitmap()

    // The rotated boundary swaps the dimensions.
    assertEquals(bitmap.pixels().width, 2)
    assertEquals(bitmap.pixels().height, 6)
    // The painted content follows. The top row is inside the rotated ellipse, and is empty if the
    // renderer ignores the rotation.
    assert(bitmap.getColor(1, 0).opacity > 0)
  }

  test("a rotated partial Arc's boundary is the tight box of the rotated sweep") {
    val quarterArc = Arc(
      pos = Pos.Origin,
      width = 40,
      height = 20,
      startAngle = 0,
      arcAngle = 90,
      fill = fillWith(red),
      line = None
    ).rotateAround(30, Pos.Origin)

    // The upper right quarter, turned 30 degrees counterclockwise, runs from (17.32, -10) to
    // (-5, -8.66). On the way it rises to y = -sqrt(175) = -13.23, at a point that is no axis
    // extreme of the unrotated ellipse. A boundary that rotates only those axis extremes stops at
    // the start point's y and cuts the rise off.
    val boundary = quarterArc.bounds.get
    assertEqualsDouble(boundary.upperLeftCorner.x, -5.0, 1e-9)
    assertEqualsDouble(boundary.upperLeftCorner.y, -math.sqrt(175), 1e-9)
    assertEqualsDouble(boundary.lowerRightCorner.x, 10 * math.sqrt(3), 1e-9)
    assertEqualsDouble(boundary.lowerRightCorner.y, -5 * math.sqrt(3), 1e-9)
  }

  test("a right-angle rotation keeps a partial Arc's boundary exact") {
    def quarterArc(rotationAngle: Double) = Arc(
      pos = Pos.Origin,
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = 90,
      fill = fillWith(red),
      line = None
    ).rotateAround(rotationAngle, Pos.Origin)

    // The upper right quarter turns counterclockwise into the upper left one, and so on.
    assertEquals(quarterArc(90).bounds.get, Bounds(Pos(-40, -50), Pos(0, 0)))
    assertEquals(quarterArc(180).bounds.get, Bounds(Pos(-50, 0), Pos(0, 40)))
    assertEquals(quarterArc(-90).bounds.get, Bounds(Pos(0, 0), Pos(40, 50)))
  }

  // assertPaintsExactlyItsBoundary comes from InkAssertions, in two halves. No ink may lie
  // outside the boundary rounded outward to whole pixels, because ink outside is ink the
  // renderers clip. Java2D's stroke normalization gets no extra pixel. The JVM renders with pure
  // stroke control (see StrokeControlSuite). And the ink has to come within a pixel of every
  // edge from the inside.

  test("rotated partial Arcs paint inside their boundary and reach each of its edges") {
    def arc(
        startAngle: Double,
        arcAngle: Double,
        rotationAngle: Double,
        fill: Option[FillStyle] = fillWith(red),
        line: Option[StrokeStyle] = None
    ) = Arc(Pos(3, -4), 80, 40, startAngle, arcAngle, fill, line)
      .rotateAround(rotationAngle, Pos(3, -4))

    assertPaintsExactlyItsBoundary("a quarter sweep", arc(0, 90, 30))
    assertPaintsExactlyItsBoundary("a negative sweep", arc(20, -130, 50))
    assertPaintsExactlyItsBoundary("a negative rotation", arc(100, 60, -40))
    assertPaintsExactlyItsBoundary("a sweep across zero degrees", arc(300, 200, -75))
    assertPaintsExactlyItsBoundary("angles beyond a full turn", arc(-400, -100, 400))
    assertPaintsExactlyItsBoundary("an unrotated sweep", arc(200, 110, 0))

    // A stroke's ends reach past the sweep's by what their caps add: nothing for a butt cap,
    // the stroke radius in every direction for a round one, and for a square one the corners
    // of a square, 1.41 radii away. Each of these sweeps ends at or near an extreme of the
    // turned ellipse, where the difference shows.
    val sweeps = Seq((-30.0, 280.0, 30.0), (0.0, 180.0, 0.0), (10.0, -95.0, 115.0))
    for
      cap                            <- StrokeStyle.Cap.values
      (start, extent, rotationAngle) <- sweeps
    do
      val clue  = s"a sweep from $start by $extent turned by $rotationAngle, with $cap caps"
      val swept = arc(start, extent, rotationAngle, fill = None, line = strokeOf(12, cap))
      // Butt caps may be bounded conservatively, by at most the stroke's half-width.
      if cap == StrokeStyle.Cap.Butt then
        assertInkWithinBoundary(clue, swept)
        assertInkReachesBoundary(clue, swept, slack = 7)
      else assertPaintsExactlyItsBoundary(clue, swept)
  }

  test("a stroked partial Arc's boundary holds its caps") {
    // A half circle above its diameter, radius 20, with a 12 pixel stroke. Square and round
    // caps extend the ends 6 pixels down past the diameter, and a butt cap stops at it, which its
    // bounds may reserve room for conservatively, by at most the stroke's half-width.
    def halfCircle(cap: StrokeStyle.Cap) =
      Arc(Pos.Origin, 40, 40, 0, 180, None, strokeOf(12, cap)).bounds.get

    val butt = halfCircle(StrokeStyle.Cap.Butt)
    assertEquals(butt.upperLeftCorner, Pos(-26, -26))
    assertEquals(butt.lowerRightCorner.x, 26.0)
    assert(butt.lowerRightCorner.y >= 0 && butt.lowerRightCorner.y <= 6, s"$butt")
    assertEquals(halfCircle(StrokeStyle.Cap.Square), Bounds(Pos(-26, -26), Pos(26, 6)))
    assertEquals(halfCircle(StrokeStyle.Cap.Round), Bounds(Pos(-26, -26), Pos(26, 6)))

    // Turned 45 degrees, a square cap's corner points straight along an axis and reaches
    // 6 * sqrt(2) past the sweep's end, where a round cap reaches 6.
    def turned(cap: StrokeStyle.Cap) =
      Arc(Pos.Origin, 40, 40, 0, 180, None, strokeOf(12, cap))
        .rotateAround(45, Pos.Origin)
        .bounds
        .get
    val end = 20 / math.sqrt(2)

    assertEqualsDouble(
      turned(StrokeStyle.Cap.Square).lowerRightCorner.x,
      end + 6 * math.sqrt(2),
      1e-9
    )
    assertEqualsDouble(turned(StrokeStyle.Cap.Round).lowerRightCorner.x, end + 6, 1e-9)
    val buttEnd = turned(StrokeStyle.Cap.Butt).lowerRightCorner.x
    assert(
      buttEnd >= end + 6 / math.sqrt(2) - 1e-9 && buttEnd <= end + 6 / math.sqrt(2) + 6,
      s"$buttEnd"
    )
  }

  test("a rotated full ellipse has the tight box of the turned ellipse") {
    def ellipse(rotationAngle: Double, line: Option[StrokeStyle] = None) =
      Arc(Pos(10, 20), 40, 20, 0, 360, fillWith(red), line).rotateAround(rotationAngle, Pos(10, 20))

    assertEquals(ellipse(0).bounds.get, Bounds(Pos(-10, 10), Pos(30, 30)))
    assertEquals(ellipse(90).bounds.get, Bounds(Pos(0, 0), Pos(20, 40)))
    assertEquals(ellipse(-180).bounds.get, Bounds(Pos(-10, 10), Pos(30, 30)))

    // The half-extents are sqrt(a^2 cos^2 + b^2 sin^2) and sqrt(a^2 sin^2 + b^2 cos^2). The box
    // around the turned 40 x 20 rectangle is larger, at 44.6 x 37.3.
    for rotationAngle <- Seq(30.0, -30.0, 150.0, 210.0) do
      val boundary = ellipse(rotationAngle).bounds.get
      assertEqualsDouble(boundary.width, 2 * math.sqrt(325), 1e-9)
      assertEqualsDouble(boundary.height, 2 * math.sqrt(175), 1e-9)
      assertEqualsDouble(boundary.center.x, 10.0, 1e-9)
      assertEqualsDouble(boundary.center.y, 20.0, 1e-9)

    // A stroke adds its radius on every side.
    val stroked = ellipse(30, strokeOf(4)).bounds.get
    assertEqualsDouble(stroked.width, 2 * math.sqrt(325) + 4, 1e-9)
    assertEqualsDouble(stroked.height, 2 * math.sqrt(175) + 4, 1e-9)
  }

  test("rotated full ellipses paint inside their boundary and reach each of its edges") {
    def ellipse(rotationAngle: Double, fill: Option[FillStyle], line: Option[StrokeStyle]) =
      Arc(Pos(3, -4), 90, 30, 0, 360, fill, line).rotateAround(rotationAngle, Pos(3, -4))

    for rotationAngle <- Seq(0.0, 30.0, -50.0, 5.0, 90.0, 225.0) do
      assertPaintsExactlyItsBoundary(
        s"an ellipse turned by $rotationAngle",
        ellipse(rotationAngle, fillWith(red), None)
      )
      assertPaintsExactlyItsBoundary(
        s"a stroked ellipse turned by $rotationAngle",
        ellipse(rotationAngle, fillWith(red), strokeOf(10))
      )
      assertPaintsExactlyItsBoundary(
        s"an ellipse outline turned by $rotationAngle",
        ellipse(rotationAngle, None, strokeOf(10))
      )
  }

  test("an Arc's sweep runs counterclockwise from three o'clock") {
    def sweep(
        startAngle: Double,
        arcAngle: Double,
        rotationAngle: Double = 0
    ): (Double, Double) => Boolean =
      val colorAt =
        colorsOf(
          Arc(Pos.Origin, 40, 40, startAngle, arcAngle, fillWith(red), None)
            .rotateAround(rotationAngle, Pos.Origin)
        )
      (x, y) => colorAt(x, y).opacity > 0

    // A filled arc covers the segment between the curve and its chord. A positive quarter turn from
    // zero covers the upper right.
    val quarter = sweep(0, 90)
    assert(quarter(12, -12), "upper right")
    assert(!quarter(12, 12) && !quarter(-12, 12) && !quarter(-12, -12))

    // A negative extent sweeps clockwise into the lower right.
    val negative = sweep(0, -90)
    assert(negative(12, 12), "lower right")
    assert(!negative(12, -12) && !negative(-12, 12) && !negative(-12, -12))

    // 90 degrees is twelve o'clock, 180 nine, 270 (or -90) six.
    assert(sweep(60, 60)(0, -18.5), "around twelve o'clock")
    assert(!sweep(60, 60)(0, 18.5))
    assert(sweep(150, 60)(-18.5, 0), "around nine o'clock")
    assert(sweep(240, 60)(0, 18.5), "around six o'clock")
    assert(sweep(-120, 60)(0, 18.5), "around six o'clock, by a negative angle")

    // A sweep across zero degrees is one piece around three o'clock, either way round.
    for acrossZero <- Seq(sweep(-45, 90), sweep(315, 90), sweep(45, -90)) do
      assert(acrossZero(17, 0), "around three o'clock")
      assert(!acrossZero(-17, 0) && !acrossZero(0, 17) && !acrossZero(0, -17))

    // More than half a turn, from six o'clock counterclockwise to nine, covers everything but
    // the lower left segment.
    val threeQuarters = sweep(-90, 270)
    assert(threeQuarters(12, 12) && threeQuarters(12, -12) && threeQuarters(-12, -12))
    assert(!threeQuarters(-12, 12))

    // Rotation turns the same way. The upper right quarter, turned 90 degrees, is the
    // upper left one.
    val turned = sweep(0, 90, rotationAngle = 90)
    assert(turned(-12, -12), "upper left")
    assert(!turned(12, 12) && !turned(12, -12) && !turned(-12, 12))
  }
