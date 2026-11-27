package smile.pictures

import smile.modeling.Pos
import smile.testing.Cases

/** What the shape factories owe a caller: a shape of the size that was asked for, or a refusal that
  * names the argument and the value that was wrong. A shape made of NaN would paint nothing and say
  * nothing.
  */
class FactoryContractSuite extends munit.FunSuite:

  private val fill = fillWith(Cases.red)

  private def pointsOf(shape: Element): Seq[Pos] = shape.vertices

  private def distance(a: Pos, b: Pos): Double = math.hypot(a.x - b.x, a.y - b.y)

  private def assertFinite(shape: Element, clue: String): Unit =
    val bounds = shape.bounds.get
    val values = Seq(
      bounds.upperLeftCorner.x,
      bounds.upperLeftCorner.y,
      bounds.lowerRightCorner.x,
      bounds.lowerRightCorner.y
    )
    assert(values.forall(value => !value.isNaN && !value.isInfinite), s"$clue: $bounds")

  private def refusal(body: => Any): String = intercept[IllegalArgumentException](body).getMessage

  test("a scalene triangle has the three side lengths it was given, each in its place") {
    val cases = Seq(
      (30.0, 18.0, 26.0),
      (10.0, 6.0, 8.0),
      (10.0, 4.0, 13.0),
      (10.0, 13.0, 4.0),
      (5.0, 5.0, 5.0)
    )
    for (base, left, right) <- cases do
      val Seq(apex, baseLeft, baseRight) =
        pointsOf(Triangle(base, left, right, Pos(5, -3), fill, None)): @unchecked
      val clue = s"Triangle($base, $left, $right)"

      assertEqualsDouble(distance(baseLeft, baseRight), base, 1e-9, s"$clue: base")
      assertEqualsDouble(distance(baseLeft, apex), left, 1e-9, s"$clue: left side")
      assertEqualsDouble(distance(baseRight, apex), right, 1e-9, s"$clue: right side")
      // The base lies level at the bottom, its left end to the left, the apex above it.
      assertEqualsDouble(baseLeft.y, baseRight.y, 1e-9, clue)
      assert(
        baseLeft.x < baseRight.x && apex.y < baseLeft.y,
        s"$clue: $apex, $baseLeft, $baseRight"
      )
      assertEqualsDouble(
        Triangle(base, left, right, Pos(5, -3), fill, None).bounds.get.center.x,
        5.0,
        1e-9,
        clue
      )
      assertEqualsDouble(
        Triangle(base, left, right, Pos(5, -3), fill, None).bounds.get.center.y,
        -3.0,
        1e-9,
        clue
      )
  }

  test("the isosceles and equilateral factories give the sides and heights they name") {
    val Seq(apex, right, left) = pointsOf(Triangle(13, 10, Pos(5, -3), fill, None)): @unchecked
    assertEqualsDouble(distance(left, right), 10.0, 1e-9, "base")
    assertEqualsDouble(distance(apex, left), 13.0, 1e-9, "left side")
    assertEqualsDouble(distance(apex, right), 13.0, 1e-9, "right side")
    assertEqualsDouble(
      Triangle(13, 10, Pos(5, -3), fill, None).bounds.get.height,
      12.0,
      1e-9
    )

    val equilateral = pointsOf(Triangle(10, Pos(5, -3), fill, None))
    for (a, b) <- equilateral.zip(equilateral.tail :+ equilateral.head) do
      assertEqualsDouble(distance(a, b), 10.0, 1e-9)

    val byHeight = Triangle.basedOnHeight(9, Pos(5, -3), fill, None)
    assertEqualsDouble(byHeight.bounds.get.height, 9.0, 1e-9)
    val corners = pointsOf(byHeight)
    for (a, b) <- corners.zip(corners.tail :+ corners.head) do
      assertEqualsDouble(
        distance(a, b),
        9 * Triangle.SideOfEquilateralTriangleAsFactorOfHeight,
        1e-9
      )

    val byHeightAndBase = Triangle.basedOnHeightAndBase(9, 14, Pos(5, -3), fill, None)
    assertEqualsDouble(byHeightAndBase.bounds.get.height, 9.0, 1e-9)
    assertEqualsDouble(byHeightAndBase.bounds.get.width, 14.0, 1e-9)
    assertEquals(byHeightAndBase.bounds.get.center, Pos(5, -3))
  }

  test("sides that cannot close a triangle are refused, with the sides in the message") {
    // Without the refusal, the isosceles factory returns vertices at NaN for these sides.
    val isosceles = refusal(Triangle(1, 10, Pos.Origin, fill, None))
    // The JVM prints this value as "10.0" and JavaScript prints "10".
    assert(isosceles.contains("of 10") && isosceles.contains("of 1"), isosceles)
    assert(isosceles.toLowerCase.contains("base"), isosceles)

    val scalene = refusal(Triangle(10, 3, 4, Pos.Origin, fill, None))
    assert(scalene.contains("10") && scalene.contains("3") && scalene.contains("4"), scalene)
  }

  test("a triangle whose sides only just close is flat, not NaN") {
    val flat = Seq[(String, Element)](
      "sides on the base" -> Triangle(10, 4, 6, Pos(5, -3), fill, None),
      "isosceles, flat"   -> Triangle(5, 10, Pos(5, -3), fill, None),
      "no height, a base" -> Triangle.basedOnHeightAndBase(0, 10, Pos(5, -3), fill, None)
    )
    for (name, triangle) <- flat do
      assertFinite(triangle, name)
      assertEqualsDouble(triangle.bounds.get.center.x, 5.0, 1e-9, name)
      assertEqualsDouble(triangle.bounds.get.center.y, -3.0, 1e-9, name)
      assertEqualsDouble(triangle.bounds.get.height, 0.0, 1e-9, name)
  }

  test("every triangle factory refuses a side of no length, naming it and its value") {
    val refusals = Seq[(String, () => Any)](
      "side"       -> (() => Triangle(0, Pos.Origin, fill, None)),
      "side"       -> (() => Triangle(0, fill, None)),
      "height"     -> (() => Triangle.basedOnHeight(0, Pos.Origin, fill, None)),
      "height"     -> (() => Triangle.basedOnHeight(0, fill, None)),
      "side"       -> (() => Triangle(0, 5, Pos.Origin, fill, None)),
      "base"       -> (() => Triangle(5, 0, Pos.Origin, fill, None)),
      "base"       -> (() => Triangle(5, 0, fill, None)),
      "base"       -> (() => Triangle.basedOnHeightAndBase(5, 0, Pos.Origin, fill, None)),
      "base"       -> (() => Triangle.basedOnHeightAndBase(5, 0, fill, None)),
      "base"       -> (() => Triangle(0, 3, 3, Pos.Origin, fill, None)),
      "left side"  -> (() => Triangle(3, 0, 3, Pos.Origin, fill, None)),
      "right side" -> (() => Triangle(3, 3, 0, Pos.Origin, fill, None)),
      "base"       -> (() => Triangle(0, 0, 0, fill, None))
    )
    for (name, build) <- refusals do
      val message = refusal(build())
      assert(message.toLowerCase.contains(name), s"expected '$name' in: $message")
      assert(message.contains("zero") && message.contains("0"), s"expected the value in: $message")
  }

  test("every factory refuses a negative size, naming it and its value") {
    val refusals = Seq[(String, () => Any)](
      "side"         -> (() => Triangle(-2, Pos.Origin, fill, None)),
      "base"         -> (() => Triangle(5, -2, Pos.Origin, fill, None)),
      "left side"    -> (() => Triangle(5, -2, 5, Pos.Origin, fill, None)),
      "right side"   -> (() => Triangle(5, 5, -2, Pos.Origin, fill, None)),
      "height"       -> (() => Triangle.basedOnHeight(-2, Pos.Origin, fill, None)),
      "height"       -> (() => Triangle.basedOnHeightAndBase(-2, 5, Pos.Origin, fill, None)),
      "base"         -> (() => Triangle.basedOnHeightAndBase(5, -2, Pos.Origin, fill, None)),
      "side length"  -> (() => Rectangle(-2, Pos.Origin, fill, None)),
      "base length"  -> (() => Rectangle(-2, 5, Pos.Origin, fill, None)),
      "height"       -> (() => Rectangle(5, -2, Pos.Origin, fill, None)),
      "radius"       -> (() => Circle(Pos.Origin, -2, fill, None)),
      "semi-major"   -> (() => Ellipse(Pos.Origin, -2, 5, fill, None)),
      "semi-minor"   -> (() => Ellipse(Pos.Origin, 5, -2, fill, None)),
      "width"        -> (() => Pentagon(-2, 5, Pos.Origin, fill, None)),
      "height"       -> (() => Pentagon(5, -2, Pos.Origin, fill, None)),
      "circumradius" -> (() => Pentagon(-2, Pos.Origin, fill, None)),
      "width"        -> (() => Star5(-2, 5, 1, Pos.Origin, fill, None)),
      "height"       -> (() => Star5(5, -2, 1, Pos.Origin, fill, None)),
      "cusp radius"  -> (() => Star5(5, 5, -2, Pos.Origin, fill, None)),
      "circumradius" -> (() => Star5(-2, 1, Pos.Origin, fill, None)),
      "cusp radius"  -> (() => Star5(5, -2, Pos.Origin, fill, None)),
      "width"        -> (() => Arc(Pos.Origin, -2, 5, 0, 90, fill, None)),
      "height"       -> (() => Arc(Pos.Origin, 5, -2, 0, 90, fill, None))
    )
    for (name, build) <- refusals do
      val message = refusal(build())
      assert(message.toLowerCase.contains(name), s"expected '$name' in: $message")
      assert(message.contains("-2"), s"expected the value in: $message")
  }

  test("every factory refuses a size that is not a number") {
    val refusals = Seq[() => Any](
      () => Triangle(Double.NaN, Pos.Origin, fill, None),
      () => Triangle(5, 5, Double.NaN, Pos.Origin, fill, None),
      () => Rectangle(Double.NaN, 5, Pos.Origin, fill, None),
      () => Circle(Pos.Origin, Double.NaN, fill, None),
      () => Ellipse(Pos.Origin, 5, Double.NaN, fill, None),
      () => Pentagon(Double.NaN, Pos.Origin, fill, None),
      () => Star5(5, Double.NaN, Pos.Origin, fill, None),
      () => Arc(Pos.Origin, Double.NaN, 5, 0, 90, fill, None)
    )
    for build <- refusals do
      val message = refusal(build())
      assert(message.contains("NaN"), message)
  }

  test("every factory refuses a size that is infinite, naming it") {
    val inf      = Double.PositiveInfinity
    val refusals = Seq[(String, () => Any)](
      "side"         -> (() => Triangle(inf, Pos.Origin, fill, None)),
      "left side"    -> (() => Triangle(5, inf, 5, Pos.Origin, fill, None)),
      "base length"  -> (() => Rectangle(inf, 5, Pos.Origin, fill, None)),
      "height"       -> (() => Rectangle(5, inf, Pos.Origin, fill, None)),
      "radius"       -> (() => Circle(Pos.Origin, inf, fill, None)),
      "semi-minor"   -> (() => Ellipse(Pos.Origin, 5, inf, fill, None)),
      "circumradius" -> (() => Pentagon(inf, Pos.Origin, fill, None)),
      "cusp radius"  -> (() => Star5(5, inf, Pos.Origin, fill, None)),
      "width"        -> (() => Arc(Pos.Origin, inf, 5, 0, 90, fill, None))
    )
    for (name, build) <- refusals do
      val message = refusal(build())
      assert(message.toLowerCase.contains(name), s"expected '$name' in: $message")
      assert(message.contains("Infinity"), s"expected the value in: $message")
  }

  test("every factory refuses a position that is not finite") {
    val nan      = Double.NaN
    val inf      = Double.PositiveInfinity
    val stroke   = StrokeStyle(Cases.blue, 2, StrokeStyle.Cap.Butt)
    val refusals = Seq[() => Any](
      () => Rectangle(10, 10, Pos(nan, 0), fill, None),
      () => Circle(Pos(0, inf), 5, fill, None),
      () => Text(Pos(nan, 0), None, "a", "Dialog", 12, 400, fill, None),
      () => Point(Pos(inf, 0), Cases.red),
      () => Line(Pos(0, 0), Pos(nan, 1), stroke),
      () => Polygon(Pos.Origin, Seq(Pos(0, 0), Pos(nan, 1), Pos(2, 0)), fill, None)
    )
    for build <- refusals do
      val message = refusal(build())
      assert(message.contains("finite"), message)
  }

  test("a size of zero is a shape with no extent, on every factory") {
    val shapes = Seq[(String, Element)](
      "rectangle" -> Rectangle(0, 0, Pos(5, -3), fill, None),
      "square"    -> Rectangle(0, Pos(5, -3), fill, None),
      "circle"    -> Circle(Pos(5, -3), 0, fill, None),
      "ellipse"   -> Ellipse(Pos(5, -3), 0, 0, fill, None),
      "pentagon"  -> Pentagon(0, Pos(5, -3), fill, None),
      "star"      -> Star5(0, 0, Pos(5, -3), fill, None)
    )
    for (name, shape) <- shapes do
      assertFinite(shape, name)
      assertEquals(shape.bounds.get.width, 0.0, name)
      assertEquals(shape.bounds.get.height, 0.0, name)
      assertEquals(shape.anchor, Pos(5, -3), name)
  }

  test("Ellipse takes its horizontal and vertical radii, and Circle one radius") {
    val ellipse = Ellipse(Pos(5, -3), 20, 7, fill, None)
    assertEquals(ellipse.bounds.get.width, 40.0)
    assertEquals(ellipse.bounds.get.height, 14.0)
    assertEquals(ellipse.bounds.get.center, Pos(5, -3))
    assertEquals(ellipse, Arc(Pos(5, -3), 40, 14, 0, 360, fill, None))

    // Taller than wide is as good an ellipse, whatever the parameters are called.
    assertEquals(Ellipse(Pos(5, -3), 7, 20, fill, None).bounds.get.height, 40.0)

    val circle = Circle(Pos(5, -3), 9, fill, None)
    assertEquals(circle, Arc(Pos(5, -3), 18, 18, 0, 360, fill, None))
  }

  test("a pentagon and a star fit the width and height they are given") {
    for (width, height) <- Seq((40.0, 40.0), (40.0, 20.0), (20.0, 40.0)) do
      val pentagon = Pentagon(width, height, Pos(5, -3), fill, None)
      val star     = Star5(width, height, 3, Pos(5, -3), fill, None)
      for (name, shape) <- Seq("pentagon" -> pentagon, "star" -> star) do
        val clue = s"$name in $width x $height"
        assert(shape.bounds.get.width <= width + 1e-9, s"$clue: ${shape.bounds.get}")
        assert(shape.bounds.get.height <= height + 1e-9, s"$clue: ${shape.bounds.get}")
        // The shape fills one of the two dimensions.
        assert(
          (shape.bounds.get.width - width).abs < 1e-9 ||
            (shape.bounds.get.height - height).abs < 1e-9,
          s"$clue: ${shape.bounds.get}"
        )

    // The circumradius the limit works out is the one the vertices lie at.
    val radius   = Pentagon.limitCircumradiusTo(40, 20)
    val vertices = Pentagon(radius, Pos.Origin, fill, None).vertices
    val center   =
      Pos(vertices.map(_.x).sum / vertices.size, vertices.map(_.y).sum / vertices.size)
    for point <- vertices do
      assertEqualsDouble(math.hypot(point.x - center.x, point.y - center.y), radius, 1e-9)
    assertEquals(Pentagon(40, 20, Pos.Origin, fill, None), Pentagon(radius, Pos.Origin, fill, None))
  }

  test("a line runs between the two points it is given, whichever way they are passed") {
    val stroke = StrokeStyle(Cases.blue, 3, StrokeStyle.Cap.Butt)
    val line   = Line(Pos(-20, -6), Pos(17, 11), stroke)

    assertEquals(pointsOf(line), Seq(Pos(-20, -6), Pos(17, 11)))
    assertEquals(line.anchor, Pos(-1.5, 2.5))
    assertEquals(Line(-20, -6, 17, 11, stroke), line)
    assertEquals(line.fill, None)
  }
