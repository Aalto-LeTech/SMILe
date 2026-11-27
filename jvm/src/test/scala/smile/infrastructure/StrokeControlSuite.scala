package smile.infrastructure

import smile.RenderSettings
import smile.modeling.{Bounds, PathShape}
import smile.pictures.*
import smile.testing.Cases

import java.awt.geom.Path2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, RenderingHints}

/** Why the JVM renders with `RenderingHints.VALUE_STROKE_PURE`.
  *
  * Java2D's default stroke control, `VALUE_STROKE_NORMALIZE`, nudges a stroke's outline toward
  * pixel centers so that thin lines look crisp without antialiasing. The nudge is up to half a
  * pixel, down and to the right, and it moves ink out of the bounds the library computes. A
  * renderer that sizes a buffer from the bounds then clips that ink. It also made the shared
  * rasterization tests need a pixel and a half of tolerance where the canvas needs one.
  *
  * This suite keeps the experiment that established it. The same closed, stroked paths are drawn
  * straight through Java2D with each hint and measured against the library's bounds. The second
  * half checks that the library's own pipeline renders with the pure hint.
  */
class StrokeControlSuite extends munit.FunSuite:

  private val canvasSize = 400
  private val origin     = 200.0

  private def awtJoin(join: StrokeStyle.Join): Int = join match
    case StrokeStyle.Join.Miter => BasicStroke.JOIN_MITER
    case StrokeStyle.Join.Round => BasicStroke.JOIN_ROUND
    case StrokeStyle.Join.Bevel => BasicStroke.JOIN_BEVEL

  /** The box of whole pixels a polygon's stroke paints when drawn with plain Java2D, its local path
    * under its affine as the renderer draws it, under the given stroke-control hint.
    */
  private def paintedBox(
      polygon: Shape,
      strokeControl: AnyRef,
      antialiasing: AnyRef = RenderingHints.VALUE_ANTIALIAS_ON
  ): Bounds =
    val image = new BufferedImage(canvasSize, canvasSize, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, antialiasing)
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, strokeControl)
    g.translate(origin, origin)
    val m = polygon.affine
    g.transform(new java.awt.geom.AffineTransform(m.a, m.b, m.c, m.d, m.e, m.f))

    val points = polygon.shape match
      case path: PathShape => path.xy
      case other           => fail(s"$other is not a polygon")
    val path = new Path2D.Double()
    for index <- 0 until points.length / 2 do
      val (x, y) = (points(2 * index), points(2 * index + 1))
      if index == 0 then path.moveTo(x, y) else path.lineTo(x, y)
    path.closePath()

    val stroke = polygon.stroke.get
    g.setStroke(
      new BasicStroke(
        stroke.width.toFloat,
        BasicStroke.CAP_SQUARE,
        awtJoin(stroke.join),
        StrokeStyle.MiterLimit.toFloat
      )
    )
    g.setColor(java.awt.Color.BLUE)
    g.draw(path)
    g.dispose()

    val inked =
      for
        y <- 0 until canvasSize
        x <- 0 until canvasSize
        if (image.getRGB(x, y) >>> 24) > 0
      yield (x, y)
    Bounds(
      inked.map(_._1).min - origin,
      inked.map(_._2).min - origin,
      inked.map(_._1).max + 1 - origin,
      inked.map(_._2).max + 1 - origin
    )

  /** How far a painted box of whole pixels strays from the bounds, at its worst edge. */
  private def worstDeviation(painted: Bounds, boundary: Bounds): Double =
    Seq(
      painted.upperLeftCorner.x - boundary.upperLeftCorner.x,
      painted.upperLeftCorner.y - boundary.upperLeftCorner.y,
      painted.lowerRightCorner.x - boundary.lowerRightCorner.x,
      painted.lowerRightCorner.y - boundary.lowerRightCorner.y
    ).map(_.abs).max

  private val shapes: Seq[(String, Shape)] =
    for
      join          <- StrokeStyle.Join.values.toSeq
      (name, shape) <- Seq[(String, Shape)](
        "triangle" -> Triangle(60, smile.modeling.Pos(3, -4), None, Cases.stroke(12, join = join)),
        "turned rectangle" -> Rectangle(
          70,
          30,
          smile.modeling.Pos(3, -4),
          None,
          Cases.stroke(12, join = join)
        )
          .rotateAround(35, smile.modeling.Pos.Origin),
        "turned star" -> Star5(
          60,
          25,
          smile.modeling.Pos(3, -4),
          None,
          Cases.stroke(6, join = join)
        )
          .rotateAround(10, smile.modeling.Pos.Origin)
      )
    yield (s"$name with $join joins", shape)

  test("pure stroke control paints every outline within a pixel of its boundary") {
    for (name, polygon) <- shapes do
      val deviation =
        worstDeviation(paintedBox(polygon, RenderingHints.VALUE_STROKE_PURE), polygon.bounds.get)
      assert(deviation <= 1.0, s"$name: $deviation")
  }

  test("normalized stroke control is what pushed outlines past a pixel") {
    val deviations = shapes.map: (name, polygon) =>
      name -> worstDeviation(
        paintedBox(polygon, RenderingHints.VALUE_STROKE_NORMALIZE),
        polygon.bounds.get
      )
    val pure = shapes.map: (_, polygon) =>
      worstDeviation(paintedBox(polygon, RenderingHints.VALUE_STROKE_PURE), polygon.bounds.get)

    // A JDK's rasterizer may change, so no exact deviation is asserted. Normalizing never does
    // better than the pure hint, and the pure hint stays within a pixel.
    assert(deviations.map(_._2).max >= pure.max, s"normalized $deviations, pure $pure")
    assert(pure.max <= 1.0, s"pure $pure")
  }

  /** The box of whole pixels a polygon paints through the library's own pipeline, rendered on a
    * canvas larger than its boundary.
    */
  private def libraryBox(polygon: Shape, settings: RenderSettings): Bounds =
    val canvas = Bounds(-origin, -origin, origin, origin)
    val bitmap = Bitmap(polygon.crop(canvas), settings)
    val inked  =
      for
        y <- 0 until bitmap.pixels().height
        x <- 0 until bitmap.pixels().width
        if bitmap.getColor(x, y).opacity > 0
      yield (x, y)
    Bounds(
      inked.map(_._1).min - origin,
      inked.map(_._2).min - origin,
      inked.map(_._1).max + 1 - origin,
      inked.map(_._2).max + 1 - origin
    )

  test("the library renders strokes with pure stroke control") {
    // The same shapes through the library's own pipeline. Their ink stays within a pixel of
    // their boundaries, as under the pure hint above.
    for (name, polygon) <- shapes do
      assertEquals(
        libraryBox(polygon, RenderSettings.Default),
        paintedBox(polygon, RenderingHints.VALUE_STROKE_PURE),
        s"$name: the library's rendering should be the pure-hint rendering"
      )
  }

  test("normalizesStrokes renders with Java2D's normalized stroke control, and only it does") {
    val aliased    = RenderingHints.VALUE_ANTIALIAS_OFF
    val pure       = RenderSettings(drawingIsAntiAliased = false)
    val normalized = RenderSettings(drawingIsAntiAliased = false, normalizesStrokes = true)
    for (name, polygon) <- shapes do
      assertEquals(
        libraryBox(polygon, normalized),
        paintedBox(polygon, RenderingHints.VALUE_STROKE_NORMALIZE, aliased),
        s"$name with normalizesStrokes"
      )
      assertEquals(
        libraryBox(polygon, pure),
        paintedBox(polygon, RenderingHints.VALUE_STROKE_PURE, aliased),
        s"$name without normalizesStrokes"
      )
  }

  test(
    "normalizesStrokes moves a small fill's edge down and to the right, as Java2D's default does"
  ) {
    // A circle three pixels across, which the earlier renderer drew as two by
    // two pixels at the lower right. Normalized, its top-left pixel stays empty. It lies on whole
    // pixels, so that its bitmap is those three by three.
    val circle =
      Circle(smile.modeling.Pos(1.5, 1.5), 1.5, fillWith(smile.colors.PresetColor.Blue), None)
    def corner(settings: RenderSettings) = circle.toBitmap(settings).getColor(0, 0).opacity
    assertEquals(corner(RenderSettings(drawingIsAntiAliased = false)), 255)
    assertEquals(corner(RenderSettings(drawingIsAntiAliased = false, normalizesStrokes = true)), 0)
  }
