package smile.infrastructure

import org.scalajs.dom
import smile.colors.Color
import smile.modeling.{Bounds, JSBoundaryCalculator, Pos, TextLayout, TextShape}
import smile.pictures.*
import smile.testing.BrowserCapabilities

import scala.scalajs.js

/** Text where a browser engine lays it out. It is measured by the layout engine, centered on its
  * position, cached, and stretched stably between animation frames.
  *
  * Each test starts from a [[smile.testing.BrowserCapabilities]] probe and `assume`s the capability
  * it needs, so the default jsdom run reports the cases it cannot support as skipped. Run them for
  * real with `sbt -Dsmile.browser=true smileJS/test`.
  */
class BrowserTextSuite extends munit.FunSuite:

  private val red = Color(255, 0, 0)

  /** The layout box of a text where its affine puts it in the world. */
  private def layoutOf(text: Element): Bounds = text match
    case shape: Shape =>
      shape.shape match
        case glyphs: TextShape => Element.mapBox(shape.affine, glyphs.layout.layoutBox)
        case other             => fail(s"$other is not a text")
    case other => fail(s"$other is not a text")

  /** The width of a line of text as the browser measures it. */
  private def width(content: String, font: String, size: Double = 20, weight: Int = 400) =
    JSBoundaryCalculator.textMetrics(content, font, size, weight).width

  test("stretching SVG text keeps glyph advances stable between animation frames") {
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")

    // The caption from Bouncing letters. Only its box width changes, so character positions
    // in the unscaled text frame must stay fixed. Ordinary SVG text layout rounds advances
    // differently at successive scales, even though the font and content never change.
    val content = "stretched to fit a box"
    val frames  = (0 to 100).map: step =>
      val text = Text(
        pos = Pos(0, 96),
        customBounds = Some(Bounds(Pos(0, 96), 245 + step * 0.1, 30)),
        content = content,
        font = "Georgia",
        size = 30,
        weight = 700,
        fill = fillWith(red),
        line = None
      )
      val svg = JSRenderer.createSvgFrom(text)
      val _   = dom.document.body.appendChild(svg)
      try
        val element = svg.querySelector("text").asInstanceOf[js.Dynamic]
        content.indices.map: index =>
          element.getStartPositionOfChar(index).x.asInstanceOf[Double]
      finally
        val _ = dom.document.body.removeChild(svg)

    for frame <- frames.tail; (actual, expected) <- frame.zip(frames.head) do
      assertEqualsDouble(actual, expected, 1e-3)
  }

  test("text without customBounds is measured by the browser's layout engine") {
    // JSBoundaryCalculator.textMetrics measures with SVGElement.getBBox, which jsdom does
    // not implement, so the jsdom suites cannot reach this branch at all.
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")

    val text = Text(
      pos = Pos(0, 0),
      customBounds = None,
      content = "SMILe",
      font = "sans-serif",
      size = 20,
      weight = 400,
      fill = fillWith(red),
      line = None
    )

    val measured = text.bounds.get
    assert(measured.width > 0, s"measured width should be positive: $measured")
    assert(measured.height > 0, s"measured height should be positive: $measured")

    val bitmap        = Picture(text).toBitmap()
    val paintedPixels = bitmap.pixels().colors.count(_.opacity > 0)
    assert(paintedPixels > 0, "measured text should paint pixels")
  }

  test("a browser-measured text's layout box is centered on its position") {
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")

    // Every backend centers the glyphs on the position, so a measured box anchored at it, as
    // Bounds(x, y, x + w, y + h), would leave three quarters of the glyphs outside the buffer the
    // bounds size. Only a real browser reaches the measurement, so this is where the
    // centering is pinned on the JS side.
    val position = Pos(30, 20)
    val text     = Text(
      pos = position,
      customBounds = None,
      content = "SMILe",
      font = "sans-serif",
      size = 20,
      weight = 400,
      fill = fillWith(red),
      line = None
    )

    val measured = layoutOf(text)

    assert(
      measured.upperLeftCorner.x < position.x && position.x < measured.lowerRightCorner.x,
      s"the position should sit inside the layout box horizontally: $measured"
    )
    assert(
      measured.upperLeftCorner.y < position.y && position.y < measured.lowerRightCorner.y,
      s"the position should sit inside the layout box vertically: $measured"
    )
    assertEqualsDouble(measured.center.x, position.x, 1e-9)
    assertEqualsDouble(measured.center.y, position.y, 1e-9)
  }

  test("a cached browser measurement gives the bounds a fresh one does, wherever the text is") {
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")

    // JSBoundaryCalculator remembers a text's metrics without its place, so a moved copy does not
    // lay out a hidden SVG again. The box it gets has to be the one a fresh measurement would
    // give, centered on the new position.
    def label(at: Pos) = Text(
      pos = at,
      customBounds = None,
      content = "Cached label",
      font = "sans-serif",
      size = 17.5,
      weight = 400,
      fill = fillWith(red),
      line = None
    )
    val cache = JSBoundaryCalculator.textMetricsMeasurements
    val _     = label(Pos.Origin).bounds

    for position <- Seq(Pos(0, 0), Pos(-41.25, 13), Pos(1e5, -3e4)) do
      val cached = label(position).bounds
      cache.clear()
      val fresh = label(position).bounds
      assertEquals(cached, fresh, s"at $position")
      assertEqualsDouble(layoutOf(label(position)).center.x, position.x, 1e-9)
      assertEqualsDouble(layoutOf(label(position)).center.y, position.y, 1e-9)
  }

  test("browser-measured texts that differ in content, font, size or weight are measured apart") {
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")

    // Measured after the original, so a cache keyed too coarsely would hand each the original's
    // box. Each of these changes the width in any font.
    val original = width("Wide", "sans-serif")
    for variant <- Seq(
        width("Wider still", "sans-serif"),
        width("Wide", "sans-serif", size = 30),
        width("Wide", "sans-serif", weight = 700)
      )
    do assertNotEquals(variant, original)

    // Narrow letters are as wide as wide ones only in a monospaced font, so a family that keyed
    // the cache too coarsely would show up here whatever fonts the machine has.
    assertNotEquals(width("iiiiiiii", "sans-serif"), width("MMMMMMMM", "sans-serif"))
    assertEqualsDouble(width("iiiiiiii", "monospace"), width("MMMMMMMM", "monospace"), 0.01)
  }

  test("a text's baseline drop is half of what its font's ascent exceeds its descent by") {
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")
    val canvas  = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    context.font = JSTextMetrics.cssFont("serif", 22, 400)
    for content <- Seq("Ag", "x") do
      val metrics = context.measureText(content).asInstanceOf[js.Dynamic]
      val ascent  = metrics.fontBoundingBoxAscent.asInstanceOf[Double]
      val descent = metrics.fontBoundingBoxDescent.asInstanceOf[Double]
      val drop    = TextLayout(content, "serif", 22, 400).baselineDrop
      assertEquals(drop, Some((ascent - descent) / 2), content)
  }

  test("a generic font family is the browser's font of that kind, not a missing named font") {
    assume(BrowserCapabilities.laysOutSVGText, "this JS environment does not lay out SVG text")
    // Quoted, "monospace" would name a font nobody has and fall back to the proportional default.
    assertEqualsDouble(width("iiiiiiii", "monospace"), width("MMMMMMMM", "monospace"), 0.01)
    assertNotEquals(width("iiiiiiii", "sans-serif"), width("MMMMMMMM", "sans-serif"))
    // The SVG output names the family the same way the measurement does. The text is filled, since
    // the draw plan draws nothing for a text with neither a fill nor a stroke.
    val svg = JSRenderer.svgString(
      Text(Pos(0, 0), None, "x", "monospace", 20, 400, fillWith(Color(0, 0, 0)), None)
    )
    assert(svg.contains("font-family=\"monospace\""), svg)
  }
