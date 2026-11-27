package smile.testing

import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.pictures.{Bitmap, Element, FillStyle, Text, crop, toBitmap}

/** Assertions about where an element paints, measured by rasterizing it. */
trait InkAssertions:
  self: munit.Assertions =>

  /** The world-coordinate area the helpers below render, large enough that nothing they draw is
    * clipped. Rendering an element alone would not do. The renderer sizes its buffer from the
    * bounds under test and cuts off whatever falls outside them.
    */
  protected val inkCanvas: Bounds = Bounds(Pos(-120, -120), Pos(120, 120))

  /** Whether this platform's rasterizer stretches glyphs the way it stretches everything else.
    *
    * The JVM, the browsers and jsdom's canvas do. A canvas that draws its text with another library
    * than its shapes may not. node-canvas, with Cairo and Pango, sizes the glyphs by the vertical
    * factor alone under a transform that scales the two axes differently, and only spaces them by
    * the horizontal one. A text stretched to be narrow and tall then comes out as wide as it is
    * tall and overflows its layout box, which cannot be told apart from a real overflow. Tests of
    * unevenly stretched text ask this first and skip those cases when the answer is no.
    */
  protected lazy val stretchesGlyphsFaithfully: Boolean =
    System.getProperty("java.vm.name") != "Scala.js" || {
      // Stretched onto a box much taller than wide, the glyphs should be as narrow as the box.
      val narrow = Text(
        pos = Pos.Origin,
        customBounds = Some(Bounds(Pos(-8, -40), Pos(8, 40))),
        content = "HH",
        font = "sans-serif",
        size = 12,
        weight = 400,
        fill = Some(FillStyle(smile.colors.Color(255, 0, 0))),
        line = None,
        rotationAngle = 0
      )
      inkBounds(narrow).exists(_.width <= 18)
    }

  protected def onInkCanvas(element: Element): Bitmap = element.crop(inkCanvas).toBitmap()

  /** Looks up the color `element` paints at a world position, rendered on the ink canvas. */
  protected def colorsOf(element: Element): (Double, Double) => Color =
    val bitmap = onInkCanvas(element)
    (x, y) =>
      bitmap.getColor(
        (x - inkCanvas.upperLeftCorner.x).floor.toInt,
        (y - inkCanvas.upperLeftCorner.y).floor.toInt
      )

  /** The world-coordinate box of whole pixels around everything `element` paints, or `None` if it
    * paints nothing.
    */
  protected def inkBounds(element: Element): Option[Bounds] =
    val bitmap = onInkCanvas(element)
    val width  = bitmap.pixels().width
    val data   = bitmap.pixels().argbData

    var (minX, minY, maxX, maxY) = (Int.MaxValue, Int.MaxValue, -1, -1)
    var index                    = 0
    while index < data.length do
      if (data(index) >>> 24) > 0 then
        val (x, y) = (index % width, index / width)
        minX = minX.min(x)
        minY = minY.min(y)
        maxX = maxX.max(x)
        maxY = maxY.max(y)
      index += 1

    Option.when(maxX >= 0)(
      Bounds(
        inkCanvas.upperLeftCorner + (minX.toDouble, minY.toDouble),
        inkCanvas.upperLeftCorner + (maxX + 1.0, maxY + 1.0)
      )
    )

  /** Fails unless every pixel `element` paints lies within its bounds, grown by `snapping`. The
    * bounds are rounded outward to whole pixels and no further. Ink beyond that is ink the
    * renderers clip away when they size a buffer from the bounds. Bitmaps and points are drawn with
    * their corners snapped to whole device pixels, which moves them by up to half a pixel, so the
    * law allows them `snapping` of half a pixel.
    *
    * On the JVM that is all there is to it. On a canvas the boundary is first grown by
    * [[smile.testing.Rasterizer.edgeSlack]], which says why.
    */
  protected def assertInkWithinBoundary(
      description: String,
      element: Element,
      snapping: Double = 0.0
  )(using munit.Location): Unit =
    val bounds = boundsOf(element)
    val ink    =
      inkBounds(element).getOrElse(fail(s"$description: the element should paint something"))
    val grown = Bounds(
      bounds.upperLeftCorner.x - snapping,
      bounds.upperLeftCorner.y - snapping,
      bounds.lowerRightCorner.x + snapping,
      bounds.lowerRightCorner.y + snapping
    )
    assertWithin(ink, grown, s"$description: ink past the bounds; ink $ink, bounds $bounds")

  /** Fails unless the box of whole pixels `ink` lies within `boundary` rounded outward to whole
    * pixels. Where a platform has a [[smile.testing.Rasterizer.edgeSlack]], the boundary is grown
    * by it first.
    */
  protected def assertWithin(ink: Bounds, boundary: Bounds, clue: => String)(using
      munit.Location
  ): Unit =
    val slack = Rasterizer.edgeSlack
    assert(
      ink.upperLeftCorner.x >= (boundary.upperLeftCorner.x - slack).floor,
      s"$clue (left edge)"
    )
    assert(ink.upperLeftCorner.y >= (boundary.upperLeftCorner.y - slack).floor, s"$clue (top edge)")
    assert(
      ink.lowerRightCorner.x <= (boundary.lowerRightCorner.x + slack).ceil,
      s"$clue (right edge)"
    )
    assert(
      ink.lowerRightCorner.y <= (boundary.lowerRightCorner.y + slack).ceil,
      s"$clue (bottom edge)"
    )

  /** Fails unless `element`'s ink comes within `slack` pixels of each edge of its boundary, from
    * the inside. The boundary reserves no room the element does not use. The slack covers the pixel
    * grid, where an antialiased sliver below one alpha step leaves the outermost pixel empty.
    */
  protected def assertInkReachesBoundary(
      description: String,
      element: Element,
      slack: Double = 1.0
  )(using munit.Location): Unit =
    val boundary = boundsOf(element)
    val ink      =
      inkBounds(element).getOrElse(fail(s"$description: the element should paint something"))
    def clue(edge: String) =
      s"$description: ink stops short of the $edge edge; ink $ink, boundary $boundary"

    assert(ink.upperLeftCorner.x <= boundary.upperLeftCorner.x + slack, clue("left"))
    assert(ink.upperLeftCorner.y <= boundary.upperLeftCorner.y + slack, clue("top"))
    assert(ink.lowerRightCorner.x >= boundary.lowerRightCorner.x - slack, clue("right"))
    assert(ink.lowerRightCorner.y >= boundary.lowerRightCorner.y - slack, clue("bottom"))

  /** Strict containment and tightness together. The boundary holds exactly what is painted. */
  protected def assertPaintsExactlyItsBoundary(description: String, element: Element)(using
      munit.Location
  ): Unit =
    assertInkWithinBoundary(description, element)
    assertInkReachesBoundary(description, element)

  /** The bounds of `element`, which must have some. */
  protected def boundsOf(element: Element)(using munit.Location): Bounds =
    element.bounds.getOrElse(fail(s"$element should have bounds"))

  protected def assertClose(obtained: Pos, expected: Pos, tolerance: Double, clue: => Any)(using
      munit.Location
  ): Unit =
    assertEqualsDouble(
      obtained.x,
      expected.x,
      tolerance,
      s"x of $obtained, expected $expected: $clue"
    )
    assertEqualsDouble(
      obtained.y,
      expected.y,
      tolerance,
      s"y of $obtained, expected $expected: $clue"
    )

  protected def assertClose(obtained: Bounds, expected: Bounds, tolerance: Double, clue: => Any)(
      using munit.Location
  ): Unit =
    assertClose(obtained.upperLeftCorner, expected.upperLeftCorner, tolerance, clue)
    assertClose(obtained.lowerRightCorner, expected.lowerRightCorner, tolerance, clue)
