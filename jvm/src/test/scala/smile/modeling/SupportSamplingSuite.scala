package smile.modeling

import smile.colors.Color
import smile.infrastructure.GlyphCache
import smile.modeling.{Affine, Bounds}
import smile.pictures.StrokeStyle

import java.awt.{BasicStroke, Shape}
import java.awt.geom.{AffineTransform, FlatteningPathIterator, Path2D, PathIterator}
import scala.util.Random

/** The support-function bounds of every kind of local shape against the ink Java2D paints for it.
  * The shape is filled and stroked in its local frame, then mapped by a random affine. Curves are
  * handed to Java2D as fine polylines on the exact curve.
  *
  * The ink's outline is flattened finely, and its vertices sampled. The box around them must lie
  * within the bounds and touch them on every side, within 1e-6 per unit of scale. Java2D draws
  * round joins and caps as cubic curves, which bulge past the true circle by up to about 2.7e-4 of
  * its radius, so those get that much more room, and the polylines standing in for arcs sag inside
  * the curve by a little. The exactness of the curved parts themselves is checked analytically in
  * `SupportSuite`.
  */
class SupportSamplingSuite extends munit.FunSuite:

  /** How finely Java2D's curves are flattened, in world pixels. */
  private val Flatness = 1e-7

  /** How close the sampled ink must come to exact bounds, in world pixels per unit of scale. */
  private val Precision = 1e-6

  /** How far inside the exact curve the polyline standing in for an arc may stay, per unit of
    * scale. Its chords sag by about the radius times the square of their angle over 8, more where a
    * flat ellipse turns sharply.
    */
  private val ArcSag = 1e-5

  /** How far a cubic quarter circle bulges past the circle, relative to its radius. */
  private val CubicBulge = 3e-4

  private final case class Case(
      shape: LocalShape,
      local: Shape,
      stroke: Option[StrokeStyle],
      m: Affine,
      curvedReach: Double,
      conservative: Boolean,
      sag: Double = 0
  )

  test("a path's bounds hold its ink and touch it under any affine") {
    val random = new Random(401)
    check(Seq.fill(200):
      val count  = random.between(2, 8)
      val xy     = IArray.fill(2 * count)(random.between(-20.0, 20.0))
      val closed = random.nextBoolean()
      val path   = new Path2D.Double()
      path.moveTo(xy(0), xy(1))
      for i <- 1 until count do path.lineTo(xy(2 * i), xy(2 * i + 1))
      if closed then path.closePath()
      val line = randomStroke(random)
      Case(
        PathShape(xy, closed),
        path,
        line,
        SupportSuite.randomAffine(random),
        curvedReach = line.filter(isRound).fold(0.0)(_.width / 2),
        conservative = false
      ))
  }

  test("an ellipse's and an arc's bounds hold their ink and touch it under any affine") {
    val random = new Random(402)
    check(Seq.fill(200):
      val (rx, ry) = (random.between(1.0, 25.0), random.between(1.0, 25.0))
      val full     = random.nextInt(3) == 0
      val start    = random.between(-360.0, 360.0)
      val extent   = if full then 360.0 else random.between(-350.0, 350.0)
      val local    = exactArc(rx, ry, start, extent, full)
      val line     = randomStroke(random)
      Case(
        EllipseArcShape(rx, ry, start, extent),
        local,
        line,
        SupportSuite.randomAffine(random),
        curvedReach = line.filter(isRound).fold(0.0)(_.width / 2),
        conservative = !full && line.exists(_.cap == StrokeStyle.Cap.Butt),
        sag = ArcSag
      ))
  }

  test("a point's bounds are its box under any affine") {
    val random = new Random(403)
    check(Seq.fill(200):
      Case(
        PointShape(),
        new java.awt.geom.Rectangle2D.Double(-0.5, -0.5, 1, 1),
        None,
        SupportSuite.randomAffine(random),
        curvedReach = 0,
        conservative = false
      ))
  }

  test("a text's bounds hold its glyphs, stroked or not, under any affine") {
    val random = new Random(404)
    val texts  = Seq(
      ("SMILe", "SansSerif", 20.0, 400),
      ("fjord Qg", "Serif", 31.0, 700),
      ("Åäö", "Monospaced", 12.5, 400)
    )
    for _ <- 0 until 200 do
      val (content, font, size, weight) = texts(random.nextInt(texts.size))
      val layout                        = TextLayout(content, font, size, weight)
      val glyphs                        = GlyphCache.natural(content, font, size, weight).outline
      val line                          = randomStroke(random).map(_.forGlyphs)
      val m                             = SupportSuite.randomAffine(random)
      val bounds                        = Support.worldBounds(TextShape(layout), line, m)
      val ink                           = sampledBox(glyphs, line, m)
      val slack = (Precision + CubicBulge * line.fold(0.0)(_.width / 2)) * (1 + norm(m))
      assertWithin(ink, bounds, slack, s"$content")
  }

  test("a text's local box is its layout box together with its glyphs' ink") {
    val layout = TextLayout("fjord Qg", "Serif", 31.0, 400)
    val glyphs = GlyphCache.natural("fjord Qg", "Serif", 31.0, 400).outline.getBounds2D
    val visual = layout.visualBox
    assertEqualsDouble(visual.upperLeftCorner.x, glyphs.getMinX, 1e-3)
    assertEqualsDouble(visual.upperLeftCorner.y, glyphs.getMinY, 1e-3)
    assertEqualsDouble(visual.lowerRightCorner.x, glyphs.getMaxX, 1e-3)
    assertEqualsDouble(visual.lowerRightCorner.y, glyphs.getMaxY, 1e-3)
    val box = TextShape(layout).localBox
    assertEquals(box.upperLeftCorner.y, layout.layoutBox.upperLeftCorner.y.min(glyphs.getMinY))
    assert(box.lowerRightCorner.y >= layout.layoutBox.lowerRightCorner.y)
    assertEquals(layout.layoutBox.center.x, 0.0)
    assertEquals(layout.layoutBox.center.y, 0.0)
  }

  test("an empty text is its layout box") {
    val layout = TextLayout("", "SansSerif", 20, 400)
    assertEquals(layout.visualBox, layout.layoutBox)
    assertEquals(layout.layoutBox.width, 0.0)
    assert(layout.layoutBox.height > 0)
  }

  private def isRound(style: StrokeStyle) =
    style.join == StrokeStyle.Join.Round || style.cap == StrokeStyle.Cap.Round

  /** The arc as a fine polyline on the exact curve, `(rx * cos(t), -ry * sin(t))`. Java2D's own
    * ellipses and arcs are cubic curves that bulge past the curve, and its stroker offsets them
    * only approximately, by up to a hundredth of a pixel, so the suite has it stroke this polyline.
    * The polyline's turns are so slight that its joins add nothing visible.
    */
  private def exactArc(rx: Double, ry: Double, start: Double, extent: Double, full: Boolean) =
    val steps = 20000
    // An open arc's caps face the way its first and last segments run, so those are tiny chords
    // that follow the curve's tangent at its ends. A closed curve's last point is its first,
    // which closePath returns to.
    val nudge  = extent * 1e-9
    val angles =
      if full then (0 until steps).map(i => start + extent * i / steps)
      else
        (start +: (start + nudge) +: (1 until steps).map(i => start + extent * i / steps)) ++
          Seq(start + extent - nudge, start + extent)
    val path = new Path2D.Double()
    for (degrees, i) <- angles.zipWithIndex do
      val t = math.toRadians(degrees)
      if i == 0 then path.moveTo(rx * math.cos(t), -ry * math.sin(t))
      else path.lineTo(rx * math.cos(t), -ry * math.sin(t))
    if full then path.closePath()
    path

  private def check(cases: Seq[Case]): Unit =
    for c <- cases do
      val bounds = Support.worldBounds(c.shape, c.stroke, c.m)
      val ink    = sampledBox(c.local, c.stroke, c.m)
      val slack  = (Precision + CubicBulge * c.curvedReach) * (1 + norm(c.m))
      val touch  = slack + c.sag * (1 + norm(c.m))
      val clue   = s"${c.shape} ${c.stroke} ${c.m}"
      assertWithin(ink, bounds, slack, clue)
      // Butt caps on an arc are bounded by the stroke's round band, which may reach up to the
      // stroke's radius past the cap along each axis.
      val radius                        = c.stroke.fold(0.0)(_.width / 2)
      def reach(ax: Double, ay: Double) =
        if c.conservative then radius * math.hypot(ax, ay) else 0.0
      val (il, it, ir, ib) = sides(ink)
      val (bl, bt, br, bb) = sides(bounds)
      assert(il - bl <= touch + reach(c.m.a, c.m.c), s"left: $il vs $bl, $clue")
      assert(br - ir <= touch + reach(c.m.a, c.m.c), s"right: $ir vs $br, $clue")
      assert(it - bt <= touch + reach(c.m.b, c.m.d), s"top: $it vs $bt, $clue")
      assert(bb - ib <= touch + reach(c.m.b, c.m.d), s"bottom: $ib vs $bb, $clue")

  private def assertWithin(ink: Bounds, bounds: Bounds, slack: Double, clue: String): Unit =
    val (il, it, ir, ib) = sides(ink)
    val (bl, bt, br, bb) = sides(bounds)
    assert(il >= bl - slack && it >= bt - slack, s"ink $ink pokes out of $bounds: $clue")
    assert(ir <= br + slack && ib <= bb + slack, s"ink $ink pokes out of $bounds: $clue")

  /** The box around the vertices of the finely flattened outline of what Java2D paints for `local`
    * under `m`, which is its fill and, if there is one, its stroke.
    */
  private def sampledBox(local: Shape, stroke: Option[StrokeStyle], m: Affine): Bounds =
    val transform = new AffineTransform(m.a, m.b, m.c, m.d, m.e, m.f)
    val inked     = local +: stroke.toSeq.map(style => awtStroke(style).createStrokedShape(local))
    var (minX, minY, maxX, maxY) =
      (Double.MaxValue, Double.MaxValue, Double.MinValue, Double.MinValue)
    val coordinates = new Array[Double](6)
    for shape <- inked do
      val iterator = new FlatteningPathIterator(shape.getPathIterator(transform), Flatness, 24)
      while !iterator.isDone do
        if iterator.currentSegment(coordinates) != PathIterator.SEG_CLOSE then
          minX = minX.min(coordinates(0))
          maxX = maxX.max(coordinates(0))
          minY = minY.min(coordinates(1))
          maxY = maxY.max(coordinates(1))
        iterator.next()
    Bounds(minX, minY, maxX, maxY)

  private def awtStroke(style: StrokeStyle): BasicStroke =
    val cap = style.cap match
      case StrokeStyle.Cap.Butt   => BasicStroke.CAP_BUTT
      case StrokeStyle.Cap.Round  => BasicStroke.CAP_ROUND
      case StrokeStyle.Cap.Square => BasicStroke.CAP_SQUARE
    val join = style.join match
      case StrokeStyle.Join.Miter => BasicStroke.JOIN_MITER
      case StrokeStyle.Join.Round => BasicStroke.JOIN_ROUND
      case StrokeStyle.Join.Bevel => BasicStroke.JOIN_BEVEL
    new BasicStroke(style.width.toFloat, cap, join, StrokeStyle.MiterLimit.toFloat)

  private def randomStroke(random: Random): Option[StrokeStyle] =
    Option.when(random.nextInt(4) > 0):
      StrokeStyle(
        Color(0, 0, 0),
        // Single-precision widths, so that Java2D strokes exactly the width the bounds assume.
        random.between(0.5f, 8.0f).toDouble,
        StrokeStyle.Cap.fromOrdinal(random.nextInt(3)),
        StrokeStyle.Join.fromOrdinal(random.nextInt(3))
      )

  private def norm(m: Affine): Double = math.hypot(m.a, m.b).max(math.hypot(m.c, m.d))

  private def sides(b: Bounds) =
    (b.upperLeftCorner.x, b.upperLeftCorner.y, b.lowerRightCorner.x, b.lowerRightCorner.y)
end SupportSamplingSuite
