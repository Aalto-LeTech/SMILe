package smile.pictures

import smile.modeling.{Bounds, Pos}
import smile.testing.{Cases, InkAssertions}
import smile.testing.Cases.stroke as strokeOf

/** A stroked polygon's boundary, corner by corner. The cases cover how far each kind of join
  * reaches, where a miter gives way to a bevel, and what paths without a proper corner do. Every
  * figure is checked against the geometry and against the pixels the renderers paint, since all
  * three backends have to bevel at the same limit for the boundary to be right on each.
  */
class StrokeOutlineSuite extends munit.FunSuite with InkAssertions:

  private val red  = Cases.red
  private val blue = Cases.blue

  private val radius = 4.0

  /** An upright isosceles spike with its apex on top, `apexAngle` degrees wide there. */
  private def spike(apexAngle: Double, join: StrokeStyle.Join, height: Double = 60): Shape =
    val halfBase = height * math.tan(math.toRadians(apexAngle / 2))
    Polygon(
      Pos(3, 10),
      Seq(Pos(0, -height / 2), Pos(halfBase, height / 2), Pos(-halfBase, height / 2)),
      fillWith(Cases.red),
      Cases.stroke(2 * radius, join = join)
    ) match
      case shape: Shape => shape
      case other        => fail(s"A polygon of three points is a shape, not $other")

  /** How far above its apex a spike's boundary reaches. */
  private def reachAboveApex(polygon: Shape): Double =
    polygon.vertices.head.y - polygon.bounds.get.upperLeftCorner.y

  test("a miter tip lies the stroke radius over the sine of half the corner past the corner") {
    for apexAngle <- Seq(170.0, 120.0, 90.0, 60.0, 36.0, 20.0, 12.0) do
      assertEqualsDouble(
        reachAboveApex(spike(apexAngle, StrokeStyle.Join.Miter)),
        radius / math.sin(math.toRadians(apexAngle / 2)),
        1e-9,
        s"a corner of $apexAngle degrees"
      )
  }

  test("a corner too sharp for the miter limit is beveled, in the boundary as in the pixels") {
    // The limit of 10 is reached where 1 / sin(angle / 2) is 10, at 11.48 degrees.
    val limitAngle = 2 * math.toDegrees(math.asin(1 / StrokeStyle.MiterLimit))
    assertEqualsDouble(limitAngle, 11.478, 1e-3)

    val mitered = spike(12, StrokeStyle.Join.Miter)
    val beveled = spike(11, StrokeStyle.Join.Miter)
    assertEqualsDouble(reachAboveApex(mitered), radius / math.sin(math.toRadians(6)), 1e-9)
    // A bevel's corners lie the radius from the apex, sideways from the two edges.
    assertEqualsDouble(reachAboveApex(beveled), radius * math.sin(math.toRadians(5.5)), 1e-9)
    // Past the limit a miter join at the apex is a bevel join. The spike's two base corners
    // are blunt and keep their miters.
    assertEqualsDouble(
      reachAboveApex(beveled),
      reachAboveApex(spike(11, StrokeStyle.Join.Bevel)),
      1e-9
    )

    // The backends agree on which side of the limit each spike is.
    assertPaintsExactlyItsBoundary("a 12 degree spike, mitered", mitered)
    assertPaintsExactlyItsBoundary("an 11 degree spike, beveled", beveled)
  }

  test("round and bevel joins reach the stroke radius, whatever the corner") {
    for apexAngle <- Seq(120.0, 60.0, 20.0, 8.0) do
      assertEqualsDouble(reachAboveApex(spike(apexAngle, StrokeStyle.Join.Round)), radius, 1e-9)
      assertEqualsDouble(
        reachAboveApex(spike(apexAngle, StrokeStyle.Join.Bevel)),
        radius * math.sin(math.toRadians(apexAngle / 2)),
        1e-9
      )
  }

  for join <- StrokeStyle.Join.values do
    test(s"spikes of every sharpness paint exactly their boundary: $join joins") {
      for apexAngle <- Seq(120.0, 90.0, 45.0, 20.0, 8.0); turn <- Seq(0.0, 33.0, 180.0) do
        assertPaintsExactlyItsBoundary(
          s"a $apexAngle degree spike turned by $turn",
          spike(apexAngle, join, height = 40).rotateAround(turn, Pos(3, 10))
        )
    }

  test("the turn's direction does not matter: a polygon listed backwards has the same boundary") {
    for shape <- Cases.polygons.filter(_.label.startsWith("stroked")) do
      val (fill, line) = shape.value match
        case polygon: Shape => (polygon.fill, polygon.stroke)
        case other          => fail(s"$other is not a polygon")
      assertClose(
        Polygon(Pos.Origin, shape.value.vertices.reverse, fill, line).bounds.get,
        shape.value.bounds.get,
        1e-9,
        shape.label
      )
  }

  test("a concave corner adds no join beyond the stroke's sides") {
    // The notch at the back of this arrowhead turns the other way from the three tips.
    val arrow = Polygon(
      Pos.Origin,
      Seq(Pos(0, -30), Pos(24, 30), Pos(0, 12), Pos(-24, 30)),
      fillWith(Cases.red),
      Cases.stroke(6)
    )
    assertPaintsExactlyItsBoundary("an arrowhead", arrow)
    assertPaintsExactlyItsBoundary("an arrowhead, turned", arrow.rotateAround(77, Pos.Origin))
  }

  test("repeated and collinear vertices are no corners") {
    val stroke = Cases.stroke(6)
    val square = Seq(Pos(-20, -10), Pos(20, -10), Pos(20, 10), Pos(-20, 10))
    val plain  = Polygon(Pos(3, 4), square, fillWith(Cases.red), stroke)

    // A vertex given twice, one in the middle of an edge, and the first one given again at
    // the end. Each describes the same outline.
    val doubled   = Seq(square(0), square(0), square(1), square(2), square(2), square(3))
    val collinear = Seq(square(0), Pos(0, -10), square(1), square(2), square(3))
    val closed    = square :+ square.head
    for (name, points) <- Seq("doubled" -> doubled, "collinear" -> collinear, "closed" -> closed) do
      val polygon = Polygon(Pos(3, 4), points, fillWith(Cases.red), stroke)
      assertEquals(polygon.bounds.get, plain.bounds.get, name)
      assertPaintsExactlyItsBoundary(s"a square with $name vertices", polygon)
  }

  test("a path that doubles back on itself is capped by its joins, not left open") {
    // Three points on a line, out and back. The far end is a join turning half a circle, which
    // a miter cannot make and which a round join rounds off.
    def flat(join: StrokeStyle.Join) = Polygon(
      Pos.Origin,
      Seq(Pos(-20, 0), Pos(20, 0), Pos(-20, 0)),
      None,
      Cases.stroke(8, join = join)
    )
    assertEquals(flat(StrokeStyle.Join.Miter).bounds.get, Bounds(Pos(-20, -4), Pos(20, 4)))
    assertEquals(flat(StrokeStyle.Join.Bevel).bounds.get, Bounds(Pos(-20, -4), Pos(20, 4)))
    assertEquals(flat(StrokeStyle.Join.Round).bounds.get, Bounds(Pos(-24, -4), Pos(24, 4)))
    for join <- StrokeStyle.Join.values do
      assertInkWithinBoundary(s"a doubled-back path with $join joins", flat(join))
  }

  test("an open two-point path has caps, and they reach what each cap style adds") {
    def line(cap: StrokeStyle.Cap) =
      Line(Pos(-20, 0), Pos(20, 0), StrokeStyle(Cases.blue, 8, cap))
    assertEquals(line(StrokeStyle.Cap.Butt).bounds.get, Bounds(Pos(-20, -4), Pos(20, 4)))
    assertEquals(line(StrokeStyle.Cap.Square).bounds.get, Bounds(Pos(-24, -4), Pos(24, 4)))
    assertEquals(line(StrokeStyle.Cap.Round).bounds.get, Bounds(Pos(-24, -4), Pos(24, 4)))

    // Slanted at 45 degrees, a square cap's corner points along an axis and outreaches a
    // round one.
    def slanted(cap: StrokeStyle.Cap) =
      Line(
        Pos(-10, -10),
        Pos(10, 10),
        StrokeStyle(Cases.blue, 8, cap)
      ).bounds.get.lowerRightCorner.x
    assertEqualsDouble(slanted(StrokeStyle.Cap.Butt), 10 + 4 / math.sqrt(2), 1e-9)
    assertEqualsDouble(slanted(StrokeStyle.Cap.Round), 14.0, 1e-9)
    assertEqualsDouble(slanted(StrokeStyle.Cap.Square), 10 + 4 * math.sqrt(2), 1e-9)

    for cap <- StrokeStyle.Cap.values; turn <- Seq(0.0, 45.0, 107.0) do
      assertPaintsExactlyItsBoundary(
        s"a line with $cap caps turned by $turn",
        line(cap).rotateAround(turn, Pos.Origin)
      )
  }

  test("a stroke without a width, and a polygon without an extent, add nothing") {
    val square = Seq(Pos(-20, -10), Pos(20, -10), Pos(20, 10), Pos(-20, 10))
    assertEquals(
      Polygon(Pos.Origin, square, None, Cases.stroke(0)).bounds.get,
      Bounds(Pos(-20, -10), Pos(20, 10))
    )
    // Every vertex in one place gives a point, with the stroke's plain margin around it.
    assertEquals(
      Polygon(Pos(5, 5), Seq.fill(3)(Pos(1, 1)), None, Cases.stroke(4)).bounds.get,
      Bounds(Pos(4, 4), Pos(8, 8))
    )
    // No points at all give the empty picture, which pulls no point into a union with anything.
    assertEquals[Element, Element](Polygon(Pos(5, 5), Seq.empty, None, Cases.stroke(4)), Empty)
    val square4 = Rectangle(4, Pos(100, 100), None, Cases.stroke(1))
    assertEquals(
      Picture(Polygon(Pos(5, 5), Seq.empty, fillWith(Cases.red), None), square4).bounds,
      square4.bounds
    )
  }

  test("a stroke's width must be finite and not negative") {
    for width <- Seq(-1.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      intercept[IllegalArgumentException](StrokeStyle(Cases.blue, width))
    assertEquals(StrokeStyle(Cases.blue, 0).width, 0.0)
  }

  test("a stroke scales with its shape, along each axis by that axis's factor") {
    // A circle with a 4 pixel outline scaled 3 wide and 1 tall has an outline 12 pixels thick
    // at the sides and 4 at the top, so the bounds scale exactly by the factors, and scaling
    // back undoes it.
    val circle  = Circle(Pos.Origin, 10, None, Some(StrokeStyle(Cases.blue, 4)))
    val widened = circle.scaleBy(3, 1)
    assertClose(widened.bounds.get, Bounds(Pos(-36, -12), Pos(36, 12)), 1e-9, "widened")
    assertClose(widened.scaleBy(1.0 / 3, 1).bounds.get, circle.bounds.get, 1e-9, "and back")
    // The style itself is kept. It lives in the circle's own frame.
    widened match
      case shape: Shape => assertEquals(shape.stroke, Some(StrokeStyle(Cases.blue, 4)))
      case other        => fail(s"$other is not a shape")
    // In pixels the outline is three times as thick at the sides as at the top.
    val drawn  = widened.toBitmap()
    val row    = (0 until drawn.pixels().width).map(drawn.getColor(_, 12))
    val column = (0 until drawn.pixels().height).map(drawn.getColor(36, _))
    val side   = row.takeWhile(_.opacity > 127).length
    val top    = column.takeWhile(_.opacity > 127).length
    assert(side >= 3 * top - 2 && side <= 3 * top + 2, s"side $side, top $top")
  }

  test("a stroked Polygon's bounds hold its joins") {
    // A 2 pixel stroke on an equilateral triangle with side 10. A miter join comes to a point
    // twice the stroke radius beyond each 60 degree corner, along the corner's bisector.
    val height                           = 5 * math.sqrt(3)
    def triangle(join: StrokeStyle.Join) =
      Triangle(10, Pos.Origin, None, strokeOf(2, join = join)).bounds.get

    val mitered = triangle(StrokeStyle.Join.Miter)
    assertEqualsDouble(mitered.upperLeftCorner.x, -5 - math.sqrt(3), 1e-9)
    assertEqualsDouble(mitered.upperLeftCorner.y, -height / 2 - 2, 1e-9)
    assertEqualsDouble(mitered.lowerRightCorner.x, 5 + math.sqrt(3), 1e-9)
    assertEqualsDouble(mitered.lowerRightCorner.y, height / 2 + 1, 1e-9)

    // A round join reaches the stroke radius in every direction.
    val rounded = triangle(StrokeStyle.Join.Round)
    assertEqualsDouble(rounded.upperLeftCorner.x, -6.0, 1e-9)
    assertEqualsDouble(rounded.upperLeftCorner.y, -height / 2 - 1, 1e-9)
    assertEqualsDouble(rounded.lowerRightCorner.y, height / 2 + 1, 1e-9)

    // A bevel join cuts the corner off at the ends of the two edges' strokes.
    val beveled = triangle(StrokeStyle.Join.Bevel)
    assertEqualsDouble(beveled.upperLeftCorner.x, -5 - math.sqrt(3) / 2, 1e-9)
    assertEqualsDouble(beveled.upperLeftCorner.y, -height / 2 - 0.5, 1e-9)
    assertEqualsDouble(beveled.lowerRightCorner.y, height / 2 + 1, 1e-9)

    // Right-angle miters extend the rectangle's box by one stroke radius on each side.
    val rectangle = Rectangle(10, 4, Pos.Origin, None, strokeOf(2))
    assertEquals(rectangle.bounds.get, Bounds(Pos(-6, -3), Pos(6, 3)))
  }

  test("stroked Polygons and Lines paint inside their boundary and reach each of its edges") {
    for join <- StrokeStyle.Join.values do
      assertPaintsExactlyItsBoundary(
        s"a triangle with $join joins",
        Triangle(60, Pos(3, -4), fillWith(red), strokeOf(12, join = join))
      )
      assertPaintsExactlyItsBoundary(
        s"a turned rectangle with $join joins",
        Rectangle(70, 30, Pos(3, -4), fillWith(red), strokeOf(12, join = join))
          .rotateAround(35, Pos.Origin)
      )
      assertPaintsExactlyItsBoundary(
        s"a star with $join joins",
        Star5(60, 25, Pos(3, -4), None, strokeOf(6, join = join)).rotateAround(10, Pos.Origin)
      )

    for cap <- StrokeStyle.Cap.values do
      assertPaintsExactlyItsBoundary(
        s"a slanted line with $cap caps",
        Line(Pos(-40, -10), Pos(35, 25), StrokeStyle(blue, 14, cap))
      )
      assertPaintsExactlyItsBoundary(
        s"a level line with $cap caps",
        Line(Pos(-40, 5), Pos(35, 5), StrokeStyle(blue, 14, cap))
      )
  }
