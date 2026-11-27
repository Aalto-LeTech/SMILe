package smile.rendering

import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.testing.{Cases, InkAssertions, Labeled}

/** The renderers and the composition DSL both rely on one invariant. An element paints inside its
  * `bounds`, whatever it is and however it has been transformed. The renderers size their buffers
  * from the bounds, so ink outside them is cut off, and the DSL lays elements out by them, so room
  * inside them that nothing paints shows as a gap. Bounds are tight, so shapes also reach them.
  *
  * Runs every fixture of [[smile.testing.Cases]] through every transformation below, on both
  * platforms.
  */
class InkWithinBoundarySuite extends munit.FunSuite with InkAssertions:

  private type Transformation = Element => Element

  private val pivot = Pos(7, -3)

  /** The one transformation that makes a text narrower and taller than its glyphs are, which
    * jsdom's canvas cannot draw (see `stretchesGlyphsFaithfully`).
    */
  private val squeezed = "scaled by (0.5, 1.75) around a pivot"

  private val transformations: Seq[Labeled[Transformation]] = Seq(
    Labeled("as built", identity),
    Labeled("moved", _.moveBy(13.5, -7.25)),
    Labeled("turned 30 around a pivot", _.rotateAround(30, pivot)),
    Labeled("turned 90 around the origin", _.rotateAround(90, Pos.Origin)),
    Labeled("turned -137.5 around itself", _.rotateBy(-137.5)),
    Labeled("scaled by 1.5", _.scaleBy(1.5)),
    Labeled("scaled by (2, 0.5) around itself", _.scaleBy(2, 0.5)),
    Labeled(squeezed, _.scaleAround(0.5, 1.75, pivot)),
    Labeled("mirrored left to right around a pivot", _.scaleAround(-1, 1, pivot)),
    Labeled("mirrored top to bottom", _.scaleBy(1, -1)),
    Labeled("turned half way by scaling", _.scaleAround(-1.25, -1.25, pivot)),
    Labeled("scaled, then turned", _.scaleBy(1.5, 0.75).rotateAround(40, pivot)),
    Labeled("turned, then scaled", _.rotateAround(40, pivot).scaleAround(1.5, 1.5, pivot))
  )

  private val paintingElements = Cases.elements

  /** Whether `element` is a text, which jsdom's canvas cannot squeeze. */
  private def isText(element: Element): Boolean = element match
    case shape: Shape =>
      shape.shape match
        case _: smile.modeling.TextShape => true
        case _                           => false
    case _ => false

  for transformation <- transformations do
    test(s"every element paints inside its boundary: ${transformation.label}") {
      val drawable =
        if transformation.label == squeezed && !stretchesGlyphsFaithfully then
          paintingElements.filterNot(element => isText(element.value))
        else paintingElements
      for element <- drawable do
        // Bitmaps and points are drawn with their corners on whole pixels.
        val snapped = element.label == "point" || Cases.isRaster(element.value)
        assertInkWithinBoundary(
          s"${element.label}, ${transformation.label}",
          transformation.value(element.value),
          if snapped then 0.5 else 0.0
        )
    }

  // A polygon's and an arc's bounds are the box of their outline, so their ink also reaches every
  // edge of it. A text's hold its layout box, which has room for glyphs the content may not have.

  for transformation <- transformations do
    test(s"polygons and arcs reach every edge of their boundary: ${transformation.label}") {
      for shape <- Cases.polygons ++ Cases.arcs do
        assertInkReachesBoundary(
          s"${shape.label}, ${transformation.label}",
          transformation.value(shape.value)
        )
    }

  test("a text keeps painting inside its bounds when its anchor is not in their middle") {
    // The bounds say where the glyphs go, and the anchor is what the text turns around.
    val text = Cases.boxedText().withAnchor(Pos(-10, 12))
    assertInkWithinBoundary("a text anchored outside its box", text)
    assertInkWithinBoundary("the same text, turned", text.rotateBy(60))
  }

  test("a picture paints inside its boundary, and so does each of its elements") {
    val picture = Cases.picture.rotateAround(25, pivot).scaleBy(1.5, 0.8)
    val ink     =
      inkBounds(MaskGroup(Rectangle(400, 400, Pos.Origin, fillWith(Cases.red), None), picture))
        .getOrElse(fail("the picture should paint something"))
    assertWithin(ink, boundsOf(picture), s"ink $ink, bounds ${picture.bounds}")
  }

  test("a stroked text stays inside its boundary, whatever its stroke's joins") {
    // The text is on a box that stretches the glyphs evenly enough for every canvas (see
    // stretchesGlyphsFaithfully). The canvas and SVG draw a boxed text in a scaled frame,
    // and take that scaling out of the stroke's width again.
    for join <- StrokeStyle.Join.values; rotation <- Seq(0.0, 35.0) do
      val text = Text(
        pos = Pos(4, -6),
        customBounds = Some(Bounds(Pos(-56, -26), Pos(64, 14))),
        content = "WAVY",
        font = "sans-serif",
        size = 12,
        weight = 700,
        fill = fillWith(Cases.red),
        line = Cases.stroke(6, join = join),
        rotationAngle = rotation
      )
      if stretchesGlyphsFaithfully then
        assertInkWithinBoundary(s"a stroked text with $join joins, turned by $rotation", text)
      else
        // Where glyphs are sized by one factor only, the text can still be held to the box
        // of its turned layout. No part of it strays far.
        val ink = inkBounds(text).getOrElse(fail("the text should paint something"))
        assert(ink.width <= boundsOf(text).width + 12, s"$ink in ${text.bounds}")
  }
