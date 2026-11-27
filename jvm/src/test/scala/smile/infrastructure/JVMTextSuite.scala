package smile.infrastructure

import smile.colors.{Color, PresetColor}
import smile.modeling.{Bounds, JVMBoundaryCalculator, Pos, TextShape}
import smile.pictures.*

/** Text as AWT renders it, with its weights, strokes, layout boxes and generic font families, and
  * the caches behind measuring and drawing it. Cross-platform text layout lives in the shared
  * `TextLayoutSuite`.
  */
class JVMTextSuite extends munit.FunSuite with smile.testing.InkAssertions:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  /** The layout box of a text, `text`'s line laid out at its natural size, where its affine puts it
    * in the world.
    */
  private def layoutOf(text: Element): Bounds = text match
    case shape: Shape =>
      shape.shape match
        case glyphs: TextShape => Element.mapBox(shape.affine, glyphs.layout.layoutBox)
        case other             => fail(s"$other is not a text")
    case other => fail(s"$other is not a text")

  /** The same text at a given weight, for the two weight tests below. */
  private def renderWeight(weight: Int): Bitmap =
    Text(
      pos = Pos.Origin,
      customBounds = None,
      content = "wide",
      font = "SansSerif",
      size = 12,
      weight = weight,
      fill = fillWith(PresetColor.Black),
      line = None
    ).toBitmap()

  test("Text honors its weight instead of always rendering bold") {
    val normal = renderWeight(400)
    val bold   = renderWeight(700)

    // A hardcoded bold AWT font would render every weight identically.
    assert(
      normal.pixels().colors != bold.pixels().colors ||
        normal.pixels().width != bold.pixels().width,
      "weights 400 and 700 should produce different renderings"
    )
  }

  test("JVM font weights snap to bold at 700") {
    // AWT has only PLAIN and BOLD, so the whole 100-699 band collapses onto the plain face
    // while the browser renders 500-699 bold for most families. TextAttribute.WEIGHT quantizes
    // at the same threshold, so the divergence cannot be removed. The test pins the snap point,
    // because anything that moved it would be a silent cross-platform change either way.
    def pixels(weight: Int) = renderWeight(weight).pixels().colors

    val plain = pixels(400)

    assertEquals(pixels(600), plain, "600 must render as plain, like 400")
    assertEquals(pixels(699), plain, "699 is the last plain weight")
    assertNotEquals(pixels(700), plain, "700 is the first bold weight")
  }

  test("a stroked text's bounds hold its stroke instead of clipping it") {
    // No custom bounds, so the glyphs are drawn at their natural size. Only the JVM can run
    // this measured path in the test environment, because jsdom implements no SVG text layout.
    // Letters with sharp feet and valleys, where a mitered stroke comes to points.
    def text(line: Option[StrokeStyle], content: String = "WAVY M") = Text(
      pos = Pos.Origin,
      customBounds = None,
      content = content,
      font = "SansSerif",
      size = 48,
      weight = 700,
      fill = None,
      line = line
    )
    def stroke(join: StrokeStyle.Join) = Some(StrokeStyle(blue, width = 10, join = join))

    // The stroke reserves its radius around the measured layout box on every side, whatever
    // join it asks for, because glyph outlines are always stroked with round joins.
    val margin = 5.0
    for join <- StrokeStyle.Join.values do
      val stroked       = text(stroke(join))
      val (box, layout) = (boundsOf(stroked), layoutOf(stroked))
      assert(box.upperLeftCorner.x <= layout.upperLeftCorner.x - margin + 1e-9, s"$box, $layout")
      assert(box.upperLeftCorner.y <= layout.upperLeftCorner.y - margin + 1e-9, s"$box, $layout")
      assert(box.lowerRightCorner.x >= layout.lowerRightCorner.x + margin - 1e-9, s"$box, $layout")
      assert(box.lowerRightCorner.y >= layout.lowerRightCorner.y + margin - 1e-9, s"$box, $layout")

    // Rendered alone, a text's buffer has the size of its bounds, and whatever the stroke
    // paints beyond them is cut off unseen. So the text is rendered on a canvas with room to
    // spare, and every pixel it paints has to lie within the bounds. Mitered, the valleys of
    // the M and the W sent spikes nine pixels above them.
    for
      join     <- StrokeStyle.Join.values
      content  <- Seq("WAVY M", "AVE", "Wk4z")
      rotation <- Seq(0.0, 30.0)
    do
      val stroked = text(stroke(join), content).rotateAround(rotation, Pos(3, 4))
      assertInkWithinBoundary(s"'$content' with $join joins, turned by $rotation", stroked)

    // The reserved room is used. A stroke does reach past the layout box at its sides.
    val rounded = text(stroke(StrokeStyle.Join.Round), "HHH")
    val ink     = inkBounds(rounded).get
    assert(ink.upperLeftCorner.x < layoutOf(rounded).upperLeftCorner.x, s"$ink")
    assert(ink.lowerRightCorner.x > layoutOf(rounded).lowerRightCorner.x - 1, s"$ink")
  }

  test("a text stretched onto a layout box stays inside it, whatever its glyphs") {
    // The line of text, its advance width by its font's ascent and descent, is stretched
    // onto the box, as on the canvas and in SVG. The ink may stop short of the box's edges
    // by the glyphs' side bearings and by what the font leaves above and below them, but it
    // never passes them, neither through whole-pixel advances nor through glyphs that hang
    // over their advance, like an italic f or a j. A font derived at a scaled size is laid
    // out anew, and came out up to four pixels wider than the box.
    for
      (width, height) <- Seq((60.0, 20.0), (120.0, 40.0), (33.0, 51.0))
      content         <- Seq("oblong", "WAVY M", "ill", "fjf", "gjpqy")
      font            <- Seq("Dialog", "Serif")
    do
      val box  = Bounds(Pos(70, 90), Pos(70 + width, 90 + height))
      val text = Text(
        pos = box.center,
        customBounds = Some(box),
        content = content,
        font = font,
        size = 12,
        weight = 400,
        fill = fillWith(red),
        line = None
      ).moveBy(-100, -100)
      assertInkWithinBoundary(s"'$content' in $font on a $width x $height box", text)
      assertInkWithinBoundary(
        s"'$content' in $font on a $width x $height box, scaled",
        text.scaleBy(1.5, 0.75)
      )
      // The ink also spans most of the box.
      val ink = inkBounds(text).get
      assert(ink.width > 0.6 * width, s"'$content' on a $width x $height box: $ink")
      assert(ink.height > 0.4 * height, s"'$content' on a $width x $height box: $ink")
  }

  test("where a text sits does not depend on which glyphs it is made of") {
    // In a monospaced font every string of three letters is as wide as any other, so these
    // have one layout box. A text is placed by its layout box, which comes from the font's
    // metrics, so they also share one baseline. The letters that stand on it all end on the
    // same row of pixels, tall or short.
    def text(
        content: String,
        customBounds: Option[Bounds] = None,
        font: String = "Monospaced"
    ) = Text(
      pos = Pos(3, -4),
      customBounds = customBounds,
      content = content,
      font = font,
      size = 40,
      weight = 400,
      fill = fillWith(red),
      line = None
    )
    def baselineOf(text: Element): Double = inkBounds(text).get.lowerRightCorner.y

    val (tall, short, low) = (text("HHH"), text("xxx"), text("ggg"))
    assertEquals(layoutOf(short), layoutOf(tall))
    assertEquals(layoutOf(low), layoutOf(tall))
    assertEquals(layoutOf(tall).center, Pos(3, -4))
    assertEqualsDouble(baselineOf(short), baselineOf(tall), 1.0, "x and H stand on one line")
    assert(baselineOf(low) > baselineOf(tall) + 4, "a g hangs below it")

    // Stretched onto a box, likewise.
    val box = Some(Bounds(Pos(-60, -20), Pos(40, 50)))
    assertEqualsDouble(
      baselineOf(text("xxx", box)),
      baselineOf(text("HHH", box)),
      1.0,
      "x and H stand on one line when stretched"
    )

    // And in any font, a narrow and a wide text are each centered on their position.
    for content <- Seq("ill", "WWW") do
      val proportional = text(content, font = "SansSerif")
      assertClose(layoutOf(proportional).center, Pos(3, -4), 1e-9, content)
      val ink = inkBounds(proportional).get
      assertEqualsDouble(ink.center.x, 3.0, 2.5, s"'$content' is centered by its advance: $ink")
  }

  test("a cached text measurement gives the bounds a fresh one does, wherever the text is") {
    // JVMBoundaryCalculator remembers a text's metrics without its place, so a moved copy is not
    // measured again. That is only sound if the affine applied afterwards lands the box exactly
    // where measuring from scratch would.
    def label(at: Pos) = Text(
      pos = at,
      customBounds = None,
      content = "Cached label",
      font = "SansSerif",
      size = 17.5,
      weight = 400,
      fill = fillWith(red),
      line = None
    )
    val cache = JVMBoundaryCalculator.textMetricsMeasurements
    val _     = label(Pos.Origin).bounds

    for position <- Seq(Pos(0, 0), Pos(-41.25, 13), Pos(1e5, -3e4)) do
      val cached = label(position).bounds
      cache.clear()
      val fresh = label(position).bounds
      assertEquals(cached, fresh, s"at $position")
      assertEquals(layoutOf(label(position)).center, position)
  }

  test("texts that differ in content, font, size or weight are measured apart") {
    def width(content: String, font: String, size: Double, weight: Int): Double =
      JVMBoundaryCalculator.textMetrics(content, font, size, weight).width
    // Measured after the original, so a cache keyed too coarsely would hand each the original's
    // box. Each of these changes the width in any font.
    val original = width("Wide", "SansSerif", 20, 400)
    for variant <- Seq(
        width("Wider still", "SansSerif", 20, 400),
        width("Wide", "Monospaced", 20, 400),
        width("Wide", "SansSerif", 30, 400),
        width("Wide", "SansSerif", 20, 700)
      )
    do assertNotEquals(variant, original)
  }

  test("the JVM's text measurement cache stays bounded") {
    val cache = JVMBoundaryCalculator.textMetricsMeasurements
    for index <- 0 until 2 * cache.capacity do
      val _ = Text(
        pos = Pos(index, 0),
        customBounds = None,
        content = s"label $index",
        font = "SansSerif",
        size = 12,
        weight = 400,
        fill = fillWith(red),
        line = None
      ).bounds
    assertEquals(cache.size, cache.capacity)
  }

  test("a CSS generic font family is AWT's logical font of that kind") {
    def width(content: String, font: String) =
      JVMBoundaryCalculator.textMetrics(content, font, 20, 400).width
    // Unmapped, AWT would turn the unknown name "monospace" into Dialog, which is proportional.
    assertEqualsDouble(width("iiiiiiii", "monospace"), width("MMMMMMMM", "monospace"), 0.01)
    assertEqualsDouble(width("MMMMMMMM", "monospace"), width("MMMMMMMM", "Monospaced"), 0.0)
    assertNotEquals(width("iiiiiiii", "sans-serif"), width("MMMMMMMM", "sans-serif"))
  }

  test("a text drawn from the cached glyph outline matches one laid out afresh") {
    import smile.infrastructure.GlyphCache

    def text(pos: Pos, custom: Option[Bounds], line: Option[StrokeStyle]) = Text(
      pos = pos,
      customBounds =
        custom.map(box => Bounds(box.upperLeftCorner + pos, box.lowerRightCorner + pos)),
      content = "Cached gq",
      font = "Serif",
      size = 21.5,
      weight = 700,
      fill = fillWith(red),
      line = line
    )
    val positions = Seq(Pos(0, 0), Pos(13.25, -7.5), Pos(-40.6, 101.1))
    for
      custom   <- Seq(None, Some(Bounds(Pos(-50, -12), Pos(70, 18))))
      line     <- Seq(None, Some(StrokeStyle(blue, width = 3)))
      rotation <- Seq(0.0, 25.0)
    do
      def render(pos: Pos) = text(pos, custom, line).rotateAround(rotation, pos).toBitmap()
      // Warm the cache with one position. Every other position, and the same one again, is
      // then drawn from that one cached outline.
      GlyphCache.clear()
      val _ = render(Pos(5, 5))
      for pos <- positions do
        val cached = render(pos)
        val again  = render(pos)
        GlyphCache.clear()
        val fresh = render(pos)
        val label = s"at $pos, custom box $custom, line $line, rotated $rotation"
        assertEquals(cached.pixels().width, fresh.pixels().width, label)
        assertEquals(cached.pixels().height, fresh.pixels().height, label)
        assertEquals(cached.pixels().colors, fresh.pixels().colors, label)
        assertEquals(again.pixels().colors, fresh.pixels().colors, label)
        val _ = render(Pos(5, 5))

    // The shared outline cannot be changed by whoever is handed it.
    val outline = GlyphCache.natural("Cached gq", "Serif", 21.5, 700).outline
    assert(!outline.isInstanceOf[java.awt.geom.Path2D], outline.getClass.getName)
  }

  test("the glyph outline cache stays bounded") {
    import smile.infrastructure.GlyphCache

    for i <- 0 until GlyphCache.OutlineCapacity + 100 do
      val _ = GlyphCache.natural(s"distinct text $i", "SansSerif", 12, 400)
      val _ = GlyphCache.font("SansSerif", 12 + i * 0.01, 400)
    assert(
      GlyphCache.outlineCount <= GlyphCache.OutlineCapacity,
      s"${GlyphCache.outlineCount} entries kept"
    )
    assert(GlyphCache.fontCount <= GlyphCache.FontCapacity, s"${GlyphCache.fontCount} fonts kept")
  }
