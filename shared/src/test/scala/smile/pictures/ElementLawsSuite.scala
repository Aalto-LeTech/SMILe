package smile.pictures

import smile.colors.{Color, LinearGradient}
import smile.modeling.{Bounds, Pos}
import smile.pictures.{FillStyle, StrokeStyle}

import scala.util.Random

/** The laws every element's transforms keep, over generated shapes and transforms. Text needs real
  * measurement, so its laws are checked on the JVM (`TextElementSuite`).
  */
class ElementLawsSuite extends munit.FunSuite:

  private val red  = Some(FillStyle(Color(255, 0, 0)))
  private val Grid = 1.0 / 1048576 // 2^-20, the grid translations snap to

  private def stroke(width: Double, cap: StrokeStyle.Cap, join: StrokeStyle.Join) =
    Some(StrokeStyle(Color(0, 0, 0), width, cap, join))

  private def randomStroke(random: Random): Option[StrokeStyle] =
    if random.nextBoolean() then None
    else
      val caps  = StrokeStyle.Cap.values
      val joins = StrokeStyle.Join.values
      stroke(
        random.between(1, 9).toDouble,
        caps(random.nextInt(caps.length)),
        joins(random.nextInt(joins.length))
      )

  private def randomPos(random: Random): Pos =
    Pos(random.between(-300, 300).toDouble, random.between(-300, 300).toDouble)

  private def size(random: Random): Double = random.between(1, 120).toDouble

  /** One element of each leaf kind but text, with random sizes, styles and positions. */
  private def randomElement(random: Random): Shape =
    val at   = randomPos(random)
    val line = randomStroke(random)
    random.nextInt(10) match
      case 0 => Rectangle(size(random), size(random), at, red, line)
      case 1 => Triangle(size(random), at, red, line)
      case 2 => Pentagon(size(random), at, red, line)
      case 3 => Star5(size(random), size(random) / 3, at, red, line)
      case 4 =>
        Line(at, randomPos(random), line.getOrElse(StrokeStyle(Color(0, 0, 0), 2)))
      case 5 => Circle(at, size(random) / 2, red, line)
      case 6 => Ellipse(at, size(random) / 2, size(random) / 2, red, line)
      case 7 =>
        Arc(
          at,
          size(random),
          size(random),
          random.between(-360, 360).toDouble,
          random.between(-359, 359).toDouble,
          red,
          line
        )
      case 8 =>
        val (base, left) = (size(random) + 20, size(random) + 20)
        val right        = random.between((base - left).abs + 1, base + left - 1)
        Triangle(base, left, right, at, red, line)
      case _ => Point(at, Color(0, 0, 255))

  private def samples(seed: Int, count: Int = 200): Seq[Shape] =
    val random = new Random(seed)
    Seq.fill(count)(randomElement(random))

  /** Elements first put through a random chain of a turn, a scaling and a move, so that their
    * affines and anchors are off the values that factories give.
    */
  private def chained(seed: Int, count: Int = 200): Seq[Element] =
    val random = new Random(seed)
    Seq
      .fill(count)(randomElement(random))
      .map: e =>
        e.rotateBy(random.between(-400.0, 400.0))
          .scaleBy(random.between(0.1, 3.0), random.between(-3.0, -0.1))
          .moveBy(random.between(-50.0, 50.0), random.between(-50.0, 50.0))

  private def sides(b: Bounds) =
    (b.upperLeftCorner.x, b.upperLeftCorner.y, b.lowerRightCorner.x, b.lowerRightCorner.y)

  private def boundsOf(e: Element): Bounds =
    e.bounds.getOrElse(fail(s"$e has no bounds"))

  private def assertNear(actual: Pos, expected: Pos, tolerance: Double, clue: => Any): Unit =
    assert(
      (actual.x - expected.x).abs <= tolerance && (actual.y - expected.y).abs <= tolerance,
      s"$actual is not within $tolerance of $expected: $clue"
    )

  private def assertRelative(actual: Double, expected: Double, clue: => Any): Unit =
    assert(
      (actual - expected).abs <= 1e-9 * expected.abs.max(1),
      s"$actual is not within 1e-9 of $expected: $clue"
    )

  // Every composition rounds the translation to the grid, so places agree within a few grid steps.
  private def assertSameBounds(a: Element, b: Element, clue: => Any): Unit =
    val (left, top, right, bottom)     = sides(boundsOf(a))
    val (left2, top2, right2, bottom2) = sides(boundsOf(b))
    for (x, y) <- Seq(left -> left2, top -> top2, right -> right2, bottom -> bottom2) do
      assert((x - y).abs <= 4 * Grid, s"$x is not within four grid steps of $y: $clue")

  private val quarterTurns = Seq(-450.0, -270.0, -180.0, -90.0, 0.0, 90.0, 180.0, 270.0, 360.0)

  test("two quarter-turn rotations are exactly one combined rotation") {
    val random = new Random(5)
    for e <- samples(1) do
      val a = quarterTurns(random.nextInt(quarterTurns.size))
      val b = quarterTurns(random.nextInt(quarterTurns.size))
      assertEquals(e.rotateBy(a).rotateBy(b), e.rotateBy(a + b), s"$e by $a and $b")
      assertEquals(e.rotateBy(a).rotateBy(b).hashCode, e.rotateBy(a + b).hashCode)
  }

  test("two rotations are one combined rotation within rounding at any angle") {
    val random = new Random(6)
    for e <- samples(2) do
      val (a, b)   = (random.between(-400.0, 400.0), random.between(-400.0, 400.0))
      val twice    = e.rotateBy(a).rotateBy(b)
      val combined = e.rotateBy(a + b)
      assertSameBounds(twice, combined, s"$e by $a and $b")
      assertNear(twice.anchor, combined.anchor, 4 * Grid, e)
  }

  test("quarter turns compose exactly after any chain of transforms") {
    val random = new Random(105)
    for e <- chained(101) do
      val a = quarterTurns(random.nextInt(quarterTurns.size))
      val b = quarterTurns(random.nextInt(quarterTurns.size))
      assertEquals[Element, Element](
        e.rotateBy(a).rotateBy(b),
        e.rotateBy(a + b),
        s"$e by $a and $b"
      )
      assertEquals(e.rotateBy(a).rotateBy(b).hashCode, e.rotateBy(a + b).hashCode)
      assertEquals[Element, Element](e.rotateBy(90).rotateBy(90), e.rotateBy(180), e)
  }

  test("every stored anchor lies on the grid, up to the rounding of its affine") {
    for e <- chained(102) do
      val p = e.anchor
      assertNear(p, Pos(Math.rint(p.x / Grid) * Grid, Math.rint(p.y / Grid) * Grid), 1e-9, e)
  }

  test("a quarter turn and back is exactly the element it started as") {
    for e <- samples(3) ++ chained(103) do
      assertEquals(e.rotateBy(90).rotateBy(-90), e)
      assertEquals(e.rotateBy(90).rotateBy(-90).hashCode, e.hashCode)
  }

  test("rotating and scaling keep the anchor where it is") {
    val random = new Random(7)
    for e <- samples(4) do
      val angle = random.between(-400.0, 400.0)
      // The turn's translation and the anchor are each rounded by up to half a grid step.
      assertNear(e.rotateBy(angle).anchor, e.anchor, 2 * Grid, s"$e by $angle")
      val (h, v) = (random.between(0.1, 4.0), random.between(-4.0, -0.1))
      assertNear(e.scaleBy(h, v).anchor, e.anchor, 2 * Grid, s"$e by ($h, $v)")
      assertNear(e.scaleBy(h).anchor, e.anchor, 2 * Grid, s"$e by $h")
      assertNear(e.rotateBy(90).anchor, e.anchor, 2 * Grid, e)
      assertNear(e.scaleBy(2, 0.5).anchor, e.anchor, 2 * Grid, e)
  }

  test("an element starts with its anchor in the middle of its unstroked bounds") {
    for e <- samples(8) do
      val unstroked = new Shape(e.shape, e.fill, None, e.affine, e.localAnchor)
      assertNear(e.anchor, boundsOf(unstroked).center, Grid, e)
  }

  test("moving to a point puts the anchor there") {
    val random = new Random(9)
    for e <- samples(10) do
      val p = randomPos(random)
      assertEquals(e.moveTo(p).anchor, p, e)
      assertNear(e.rotateBy(37).moveTo(p).anchor, p, Grid, e)
  }

  test("a new anchor moves nothing") {
    val random = new Random(11)
    for e <- samples(12) do
      val p     = randomPos(random)
      val moved = e.withAnchor(p)
      assertEquals(moved.bounds, e.bounds)
      assertEquals(moved.anchor, p)
      val turned = e.rotateBy(random.between(-180.0, 180.0))
      assertEquals(turned.withAnchor(p).bounds, turned.bounds)
      assertNear(turned.withAnchor(p).anchor, p, Grid, turned)
  }

  test("a clock hand anchored at its base sweeps around the base") {
    val hand  = Line(Pos(0, 0), Pos(0, -50), StrokeStyle(Color(0, 0, 0), 2)).withAnchor(Pos(0, 0))
    val swept = hand.rotateBy(-90)
    assertEquals(swept.anchor, Pos(0, 0))
    assertEquals(sides(boundsOf(swept)), (-1.0, -1.0, 51.0, 1.0))
  }

  test("scaling to a size gives exactly that size when the factors are exact") {
    val random = new Random(13)
    for _ <- 0 until 200 do
      val at    = randomPos(random)
      val line  = stroke(random.between(0, 5) * 2.0, StrokeStyle.Cap.Round, StrokeStyle.Join.Round)
      val shape = random.nextInt(4) match
        case 0 => Rectangle(size(random), size(random), at, red, line)
        case 1 => Circle(at, size(random), red, line)
        case 2 => Ellipse(at, size(random), size(random), red, line)
        case _ => Point(at, Color(0, 0, 0))
      for shown <- Seq(shape, shape.rotateBy(90), shape.flipVertically) do
        val b      = boundsOf(shown)
        val (w, h) = (b.width * 4, b.height / 8)
        val scaled = shown.scaleTo(w, h)
        assertEquals((scaled.width, scaled.height), (w, h), shown)
  }

  test("scaling to a size gives that size within rounding, rotated or not") {
    val random = new Random(14)
    for e <- samples(15) do
      val (w, h) = (random.between(1.0, 500.0), random.between(1.0, 500.0))
      for shown <- Seq(e, e.rotateBy(random.between(-180.0, 180.0))) do
        val scaled = shown.scaleTo(w, h)
        assertRelative(scaled.width, w, shown)
        assertRelative(scaled.height, h, shown)
  }

  test("scaling to a negative width flips first") {
    val random = new Random(16)
    for e <- samples(17) do
      val (w, h) = (random.between(1.0, 500.0), random.between(1.0, 500.0))
      assertEquals(e.scaleTo(-w, h), e.flipHorizontally.scaleTo(w, h))
      assertEquals(e.scaleTo(w, -h), e.flipVertically.scaleTo(w, h))
      assertEquals(e.scaleTo(-w, -h), e.flipDiagonally.scaleTo(w, h))
  }

  test("flips mirror through the anchor and twice undo themselves") {
    for e <- samples(18) ++ chained(104) do
      assertEquals(e.flipHorizontally.flipHorizontally, e)
      assertEquals(e.flipVertically.flipVertically, e)
      assertEquals[Element, Element](e.flipDiagonally, e.flipHorizontally.flipVertically)
      assertEquals(e.flipHorizontally.anchor, e.anchor)
      assertEquals(e.flipVertically.flipVertically.hashCode, e.hashCode)
  }

  test("scaling an axis with no extent leaves it") {
    val flat   = Rectangle(0, 10, Pos(5, 5), red, None)
    val scaled = flat.scaleTo(8, 20)
    assertEquals(scaled.width, 0.0)
    assertEquals(scaled.height, 20.0)
  }

  test("a star turned by 37 degrees keeps its anchor where the unturned star's middle was") {
    val star   = Star5(100, 40, Pos(20, 30), red, None)
    val turned = star.rotateBy(37)
    assertNear(turned.anchor, boundsOf(star).center, Grid, turned)
    assertNear(star.anchor, Pos(20, 30), 0, star)
  }

  test("a stroke scales with its shape") {
    val circle =
      Circle(Pos(10, 20), 15, red, stroke(4, StrokeStyle.Cap.Round, StrokeStyle.Join.Round))
    val scaled = circle.scaleBy(3, 1)
    assertEquals(scaled.width, 3 * (2 * 15.0 + 4))
    assertEquals(scaled.height, 2 * 15.0 + 4)
  }

  test("a point is a 1x1 box that scales like everything else") {
    val point = Point(Pos(3, 4), Color(0, 0, 0))
    assertEquals(sides(boundsOf(point)), (2.5, 3.5, 3.5, 4.5))
    assertEquals(sides(boundsOf(point.scaleBy(3))), (1.5, 2.5, 4.5, 5.5))
  }

  test("factories place their shapes at the given position") {
    val square = Rectangle(10, 20, Pos(100, 50), red, None)
    assertEquals(sides(boundsOf(square)), (95.0, 40.0, 105.0, 60.0))
    assertEquals(square.anchor, Pos(100, 50))
    val line = Line(Pos(0, 0), Pos(10, 0), StrokeStyle(Color(0, 0, 0), 2, StrokeStyle.Cap.Butt))
    assertEquals(sides(boundsOf(line)), (0.0, -1.0, 10.0, 1.0))
    val quarter = Arc(Pos(0, 0), 20, 20, 0, 90, red, None)
    assertEquals(sides(boundsOf(quarter)), (0.0, -10.0, 10.0, 0.0))
    assertEquals(quarter.anchor, Pos(5, -5))
    val turned = quarter.rotateAround(90, Pos(0, 0))
    assertEquals(sides(boundsOf(turned)), (-10.0, -10.0, 0.0, 0.0))
  }

  test("a pentagon's position is the middle of its bounds") {
    val pentagon = Pentagon(50, Pos(7, 9), red, None)
    assertNear(boundsOf(pentagon).center, Pos(7, 9), 1e-12, pentagon)
  }

  test("degenerate amounts keep the bounds finite and the affine invertible") {
    for e <- samples(19, 50) do
      for degenerate <- Seq(e.scaleTo(0.001, 5), e.scaleBy(1e6), e.rotateBy(33).scaleTo(0.001, 5))
      do
        val b = boundsOf(degenerate)
        assert(sides(b).toList.forall(_.isFinite), degenerate)
        assert(degenerate.affine.determinant != 0 && degenerate.affine.determinant.isFinite)
  }

  test("a zero scale or a zero size gives the empty picture") {
    for e <- samples(20, 30) do
      for empty <- Seq(
          e.scaleBy(0),
          e.scaleBy(0, 2),
          e.scaleAround(3, 0, Pos(1, 1)),
          e.scaleTo(0, 5)
        )
      do
        assertEquals(empty.bounds, None)
        assertEquals((empty.width, empty.height), (0.0, 0.0))
        assertEquals(empty.anchor, Pos.Origin)
        assertEquals(empty.rotateBy(30).scaleTo(4, 4).moveBy(5, 5), empty)
  }

  private def refuses(body: => Any): Unit =
    val _ = intercept[IllegalArgumentException](body)

  test("scaling that overflows the transform is refused with a clear message") {
    val e = Rectangle(10, 10, red, None)
    for overflow <- Seq(() => e.scaleBy(1e200).scaleBy(1e200), () => e.scaleTo(1e300, 1e300))
    do
      val error = intercept[IllegalArgumentException](overflow())
      assert(error.getMessage.contains("overflows"), error.getMessage)
    val tiny  = Rectangle(1e-300, 10, red, None)
    val error = intercept[IllegalArgumentException](tiny.scaleTo(1e10, 10))
    assert(error.getMessage.contains("overflows"), error.getMessage)
  }

  test("amounts that are not numbers are refused") {
    val e = Rectangle(10, 10, red, None)
    for amount <- Seq(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      refuses(e.scaleBy(amount))
      refuses(e.scaleBy(1, amount))
      refuses(e.scaleTo(amount, 5))
      refuses(e.rotateBy(amount))
      refuses(e.moveBy(amount, 0))
      refuses(e.moveTo(Pos(0, amount)))
      refuses(e.withAnchor(Pos(amount, 0)))
      refuses(e.scaleAround(2, Pos(amount, 0)))
      refuses(e.rotateAround(5, Pos(amount, 0)))
  }

  test("equal elements are equal values with equal hashes, negative zeros included") {
    val a = Rectangle(10, 10, Pos(-0.0, 0.0), red, None)
    val b = Rectangle(10, 10, Pos(0.0, -0.0), red, None)
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
    assertNotEquals(a, a.withAnchor(Pos(1, 1)))
    assertNotEquals(a, Rectangle(10, 10, Pos(0, 0), None, None))
    assertEquals(a.withAnchor(Pos(1, 1)).withAnchor(a.anchor), a)
    for e <- chained(106) do
      val again = e.withAnchor(e.anchor)
      assertEquals(again, e)
      assertEquals(again.hashCode, e.hashCode)
  }

  test("styles with negative zeros are equal and hash equally") {
    val gradient = (x: Double) =>
      Some(
        FillStyle(
          LinearGradient(
            Pos(x, 0),
            Pos(10, 10),
            Seq(0.0 -> Color(0, 0, 0), 1.0 -> Color(255, 255, 255))
          )
        )
      )
    val a = Rectangle(10, 10, gradient(-0.0), Some(StrokeStyle(Color(0, 0, 0), -0.0)))
    val b = Rectangle(10, 10, gradient(0.0), Some(StrokeStyle(Color(0, 0, 0), 0.0)))
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
  }
end ElementLawsSuite
