package smile.modeling

import smile.colors.Color
import smile.modeling.{Affine, Bounds, Pos}
import smile.pictures.StrokeStyle

import scala.util.Random

/** The support-function bounds of local shapes, checked against exact values and against the curves
  * themselves. The JVM side samples the ink Java2D paints (`SupportSamplingSuite`).
  */
class SupportSuite extends munit.FunSuite:

  private def stroke(width: Double, cap: StrokeStyle.Cap, join: StrokeStyle.Join) =
    Some(StrokeStyle(Color(0, 0, 0), width, cap, join))

  private def sides(b: Bounds) =
    (b.upperLeftCorner.x, b.upperLeftCorner.y, b.lowerRightCorner.x, b.lowerRightCorner.y)

  test("a circle of radius 10 is 20 wide and high at every angle") {
    val circle = EllipseArcShape(10, 10, 0, 360)
    val random = new Random(4)
    val angles = (0 until 360).map(_.toDouble) ++ Seq.fill(500)(random.between(-720.0, 720.0))
    for angle <- angles do
      val b = Support.worldBounds(circle, None, Affine.rotation(angle, Pos.Origin))
      assertEquals(b.width, 20.0, s"width at $angle")
      assertEquals(b.height, 20.0, s"height at $angle")
  }

  test("an ellipse 30 by 10 turned by 90 degrees is 10 wide and 30 high") {
    val ellipse = EllipseArcShape(15, 5, 0, 360)
    val b       = Support.worldBounds(ellipse, None, Affine.rotation(90, Pos(7, -3)))
    assertEquals(b.width, 10.0)
    assertEquals(b.height, 30.0)
  }

  test("a round stroke adds its radius to a turned ellipse, scaled with it") {
    val ellipse = EllipseArcShape(15, 5, 0, 360)
    val round   = stroke(4, StrokeStyle.Cap.Round, StrokeStyle.Join.Round)
    val turned  = Support.worldBounds(ellipse, round, Affine.rotation(90, Pos.Origin))
    assertEquals(sides(turned), (-7.0, -17.0, 7.0, 17.0))
    val scaled = Support.worldBounds(ellipse, round, Affine.scaling(3, 1))
    assertEquals(sides(scaled), (-51.0, -7.0, 51.0, 7.0))
  }

  test("scaling along the axes scales the bounds by exactly the same factors") {
    val random = new Random(23)
    for (shape, line) <- Seq.fill(300)(randomShape(random) -> randomStroke(random)) do
      val (left, top, right, bottom) = sides(Support.worldBounds(shape, line, Affine.Identity))
      val scaled                     = Support.worldBounds(shape, line, Affine.scaling(2, 3))
      assertEquals(sides(scaled), (2 * left, 3 * top, 2 * right, 3 * bottom), s"$shape $line")
  }

  test("a partial arc's bounds are where its curve reaches under any affine") {
    val random = new Random(71)
    for _ <- 0 until 300 do
      val arc = EllipseArcShape(
        random.between(0.0, 30.0),
        random.between(0.0, 30.0),
        random.between(-400.0, 400.0),
        random.between(-359.0, 359.0)
      )
      val m        = randomAffine(random)
      val expected = sampledArcBounds(arc, m)
      val actual   = sides(Support.worldBounds(arc, None, m))
      val scale    = 1e-9 * (1 + arc.rx.max(arc.ry)) * (m.a.abs + m.b.abs + m.c.abs + m.d.abs)
      for (a, e) <- actual.toList.zip(expected.toList) do
        assertEqualsDouble(a, e, scale + 1e-9, s"$arc under $m")
  }

  test("an arc ending at a right angle keeps its bounds exact") {
    // A quarter from three to twelve o'clock. Its chord fill reaches the center's level only.
    val quarter = EllipseArcShape(10, 4, 0, 90)
    assertEquals(sides(quarter.localBox), (0.0, -4.0, 10.0, 0.0))
    assertEquals(sides(EllipseArcShape(10, 4, 90, -180).localBox), (0.0, -4.0, 10.0, 4.0))
    assertEquals(
      sides(Support.worldBounds(quarter, None, Affine.rotation(90, Pos.Origin))),
      (-4.0, -10.0, 0.0, 0.0)
    )
  }

  test("a path's bounds are those of its vertices and what its stroke adds") {
    val square = PathShape(IArray(-5.0, -5.0, 5.0, -5.0, 5.0, 5.0, -5.0, 5.0), closed = true)
    assertEquals(sides(square.localBox), (-5.0, -5.0, 5.0, 5.0))
    val miter = stroke(2, StrokeStyle.Cap.Butt, StrokeStyle.Join.Miter)
    assertEquals(sides(Support.worldBounds(square, miter, Affine.Identity)), (-6.0, -6.0, 6.0, 6.0))
    // A miter tip of a square turned by 45 degrees lies sqrt(2) radii past the corner.
    val turned = Support.worldBounds(square, miter, Affine.rotation(45, Pos.Origin))
    assertEqualsDouble(turned.width, 10 * math.sqrt(2) + 2 * math.sqrt(2), 1e-12)
    val round       = stroke(2, StrokeStyle.Cap.Butt, StrokeStyle.Join.Round)
    val roundTurned = Support.worldBounds(square, round, Affine.rotation(45, Pos.Origin))
    assertEqualsDouble(roundTurned.width, 10 * math.sqrt(2) + 2, 1e-12)
  }

  test("an open path's square caps reach past its ends, and its butt caps do not") {
    val line   = PathShape(IArray(0.0, 0.0, 10.0, 0.0), closed = false)
    val butt   = stroke(4, StrokeStyle.Cap.Butt, StrokeStyle.Join.Miter)
    val square = stroke(4, StrokeStyle.Cap.Square, StrokeStyle.Join.Miter)
    assertEquals(sides(Support.worldBounds(line, butt, Affine.Identity)), (0.0, -2.0, 10.0, 2.0))
    assertEquals(
      sides(Support.worldBounds(line, square, Affine.Identity)),
      (-2.0, -2.0, 12.0, 2.0)
    )
  }

  test("a path whose vertices all coincide holds its stroke's square under a turn") {
    val lone  = PathShape(IArray(3.0, -2.0, 3.0, -2.0, 3.0, -2.0), closed = false)
    val turn  = Affine.rotation(45, Pos(1, 1))
    val reach = 2.0 // the stroke's radius
    for cap <- StrokeStyle.Cap.values; join <- StrokeStyle.Join.values do
      val line   = stroke(2 * reach, cap, join)
      val bounds = Support.worldBounds(lone, line, turn)
      // A backend draws the lone point's stroke as a disc of the radius (round caps), the square
      // of the radius facing the local axes (square caps) or nothing, all in the local frame.
      val (l, t, r, b) = sides(bounds)
      val disc         = (0 until 360 by 5)
        .map(_.toDouble.toRadians)
        .map: angle =>
          Pos(3 + reach * math.cos(angle), -2 + reach * math.sin(angle))
      val square = for dx <- Seq(-reach, reach); dy <- Seq(-reach, reach) yield Pos(3 + dx, -2 + dy)
      for local <- disc ++ square do
        val inked = turn(local)
        assert(inked.x >= l - 1e-12 && inked.x <= r + 1e-12, s"$cap $join $inked in $bounds")
        assert(inked.y >= t - 1e-12 && inked.y <= b + 1e-12, s"$cap $join $inked in $bounds")
      // The rotated square reaches the bounds at its corners.
      assertEqualsDouble(r - l, 2 * reach * math.sqrt(2), 1e-12)
      // The bound is a support function, so scaling along the world axes scales it exactly.
      val scaled = Support.worldBounds(lone, line, turn.andThen(Affine.scaling(2, 3)))
      assertEqualsDouble(scaled.upperLeftCorner.x, 2 * l, 1e-12)
      assertEqualsDouble(scaled.upperLeftCorner.y, 3 * t, 1e-12)
      assertEqualsDouble(scaled.lowerRightCorner.x, 2 * r, 1e-12)
      assertEqualsDouble(scaled.lowerRightCorner.y, 3 * b, 1e-12)
  }

  test("a point is the 1x1 box around the origin and turns with its affine") {
    val point = PointShape()
    assertEquals(sides(point.localBox), (-0.5, -0.5, 0.5, 0.5))
    assertEquals(point.center, Pos.Origin)
    val scaled =
      Support.worldBounds(point, None, Affine.scaling(3, 3).andThen(Affine.translation(1, 1)))
    assertEquals(sides(scaled), (-0.5, -0.5, 2.5, 2.5))
    val turned = Support.worldBounds(point, None, Affine.rotation(45, Pos.Origin))
    assertEqualsDouble(turned.width, math.sqrt(2), 1e-15)
  }

  test("a partial arc's center is the center of its own box") {
    assertEquals(EllipseArcShape(10, 4, 0, 90).center, Pos(5, -2))
  }

  test("shapes are equal by their geometry") {
    assertEquals(PathShape(IArray(0.0, 1.0), true), PathShape(IArray(-0.0, 1.0), true))
    assertEquals(
      PathShape(IArray(0.0, 1.0), true).hashCode,
      PathShape(IArray(-0.0, 1.0), true).hashCode
    )
    assertNotEquals(PathShape(IArray(0.0, 1.0), true), PathShape(IArray(0.0, 1.0), false))
    assertEquals(EllipseArcShape(1, 2, 3, 4), EllipseArcShape(1, 2, 3, 4))
    assertEquals(PointShape(), PointShape())
  }

  /** The bounds of the arc's chord segment under `m`, found on the curve itself. The best of many
    * sampled angles is refined by a golden-section search between its neighbors.
    */
  private def sampledArcBounds(arc: EllipseArcShape, m: Affine) =
    val from             = arc.startAngle.min(arc.startAngle + arc.arcAngle).toRadians
    val to               = arc.startAngle.max(arc.startAngle + arc.arcAngle).toRadians
    def point(t: Double) = m(arc.rx * math.cos(t), -arc.ry * math.sin(t))
    def farthest(along: Pos => Double): Double =
      val count    = 2000
      val step     = (to - from) / count
      val best     = (0 to count).maxBy(i => along(point(from + i * step)))
      var (lo, hi) = (from + (best - 1).max(0) * step, from + (best + 1).min(count) * step)
      val ratio    = (math.sqrt(5) - 1) / 2
      for _ <- 0 until 100 do
        val (x1, x2) = (hi - ratio * (hi - lo), lo + ratio * (hi - lo))
        if along(point(x1)) < along(point(x2)) then lo = x1 else hi = x2
      along(point((lo + hi) / 2)).max(along(point(from))).max(along(point(to)))
    (-farthest(p => -p.x), -farthest(p => -p.y), farthest(_.x), farthest(_.y))

  private def randomAffine(random: Random): Affine =
    SupportSuite.randomAffine(random)

  private def randomShape(random: Random): LocalShape =
    random.nextInt(3) match
      case 0 =>
        val count = random.between(1, 8)
        PathShape(IArray.fill(2 * count)(random.between(-20.0, 20.0)), random.nextBoolean())
      case 1 =>
        EllipseArcShape(
          random.between(0.0, 25.0),
          random.between(0.0, 25.0),
          random.between(-360.0, 360.0),
          random.between(-400.0, 400.0)
        )
      case _ => PointShape()

  private def randomStroke(random: Random): Option[StrokeStyle] =
    Option.when(random.nextInt(4) > 0):
      StrokeStyle(
        Color(0, 0, 0),
        random.between(0.1, 8.0),
        StrokeStyle.Cap.fromOrdinal(random.nextInt(3)),
        StrokeStyle.Join.fromOrdinal(random.nextInt(3))
      )
end SupportSuite

object SupportSuite:
  /** An affine of one of the kinds the bounds must hold under: general maps, shears, mirrors and
    * quarter turns, each with a move.
    */
  def randomAffine(random: Random): Affine =
    val move = Affine.translation(random.between(-50.0, 50.0), random.between(-50.0, 50.0))
    val linear: Affine = random.nextInt(4) match
      case 0 =>
        Iterator
          .continually(Seq.fill(4)(random.between(-2.0, 2.0)))
          .collectFirst {
            case Seq(a, b, c, d) if (a * d - b * c).abs > 0.1 => Affine(a, b, c, d, 0, 0)
          }
          .get
      case 1 =>
        val shear = random.between(-2.0, 2.0)
        Affine(1, 0, shear, 1, 0, 0).andThen(
          Affine.rotation(random.between(0.0, 360.0), Pos.Origin)
        )
      case 2 =>
        Affine
          .mirroring(random.nextBoolean(), true, Pos.Origin)
          .andThen(Affine.rotation(random.between(0.0, 360.0), Pos.Origin))
      case _ =>
        Affine
          .scaling(random.between(0.2, 3.0), random.between(0.2, 3.0))
          .andThen(Affine.rotation(90.0 * random.nextInt(4), Pos.Origin))
    linear.andThen(move)
end SupportSuite
