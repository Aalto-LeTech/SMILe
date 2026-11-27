package smile.infrastructure

import org.scalajs.dom
import smile.RenderSettings
import smile.infrastructure.JSBufferAdapter
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.pictures.{FillStyle, ResamplingHint}
import smile.testing.{BrowserCapabilities, Rasterizer}

import scala.concurrent.{ExecutionContext, Future}

/** The canvas renderer of the shared draw plan. Draws the cases of
  * `smile.infrastructure.PlanRenderCases` at several scales, as the Java2D renderer draws them.
  * Also covers what a canvas makes easy to get wrong, which is clips that outlive their operation,
  * crops that sample outside themselves and buffers that read back.
  */
class CanvasPlanRendererSuite extends munit.FunSuite:

  import PlanRenderCases.*

  private given ExecutionContext = ExecutionContext.global

  private val settings = RenderSettings.Default

  /** The packed ARGB pixels of `e` rendered at `scale` with its plan's nesting bounded by
    * `nesting`, with their width and height.
    */
  private def render(
      e: Element,
      scale: Double = 1.0,
      nesting: Int = DrawPlan.MaxNesting
  ): (Int, Int, Array[Int]) =
    val pixels = CanvasPlanRenderer.render(e, scale, settings, 0L, nesting).buffer(settings, 0L)
    (pixels.width, pixels.height, pixels.argbData)

  private def alpha(argb: Int): Int = argb >>> 24

  private def checkInk(c: Case, scale: Double): Unit =
    val (width, height, argb) = render(framed(c.element), scale)
    val (frame, _)            = DrawPlan.of(framed(c.element), DeviceGrid(scale), settings, 0L)
    assertEquals((width, height), (frame.width, frame.height))
    val problems = inkProblems(c.element, scale, argb, c.tight, settings)
    assert(problems.isEmpty, problems.mkString("; "))

  for scale <- Seq(1.0, 2.0, 1.5); c <- cases do
    test(s"${c.label} draws within its bounds at $scale") {
      assume(!c.measuresText || BrowserCapabilities.laysOutSVGText, "jsdom lays out no text")
      c.element match
        case svg: SvgPicture =>
          svg.ready.map(_ => checkInk(c, scale))
        case _ => Future.successful(checkInk(c, scale))
    }

  test("a turned sprite inks no pixel outside its bounds at any angle or offset") {
    val problems =
      for
        c       <- turnedSweep
        problem <- inkProblems(c.element, 1.0, render(framed(c.element))._3, c.tight, settings)
      yield s"${c.label}: $problem"
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  test("the empty picture renders as an empty bitmap") {
    val bitmap = CanvasPlanRenderer.render(Empty, 1.0, settings)
    assertEquals(bitmap.boundary.width, 0.0)
    assertEquals(bitmap.boundary.height, 0.0)
  }

  test("the buffer is the device box and the bounds are its logical box") {
    val e      = Rectangle(10, 10, Pos(0.25, 0.25), Some(FillStyle(red)), None)
    val bitmap = CanvasPlanRenderer.render(e, 1.5, settings)
    val pixels = bitmap.buffer(settings, 0L)
    // -4.75 to 5.25 logical is -7.125 to 7.875 device, which rounds out to -8 to 8.
    assertEquals((pixels.width, pixels.height), (16, 16))
    assertEqualsDouble(bitmap.boundary.upperLeftCorner.x, -8 / 1.5, 1e-9)
    assertEqualsDouble(bitmap.boundary.lowerRightCorner.y, 8 / 1.5, 1e-9)
  }

  test("a render target is not read back until its pixels are asked for") {
    val bitmap = CanvasPlanRenderer.render(Point(Pos(0.5, 0.5), red), 1.0, settings)
    val buffer = bitmap.buffer(settings, 0L)
    val _      = buffer.readOnlyCanvas
    assertEquals(buffer.readBacks, 0)
    assertEquals(buffer.pixelColor(0, 0), red)
    assertEquals(buffer.readBacks, 1)
  }

  test("a stroked circle scaled 3x along x is 3x thicker at its sides than at its top") {
    val circle                 = Circle(Pos.Origin, 20, None, stroke(4)).scaleBy(3, 1)
    val (width, height, p)     = render(circle)
    def opaque(x: Int, y: Int) = alpha(p(y * width + x)) >= 128
    val middleRow              = height / 2
    val side                   = (0 until width).takeWhile(x => !opaque(x, middleRow)).length
    val sideRun                = (side until width).takeWhile(x => opaque(x, middleRow)).length
    val middleColumn           = width / 2
    val top                    = (0 until height).takeWhile(y => !opaque(middleColumn, y)).length
    val topRun                 = (top until height).takeWhile(y => opaque(middleColumn, y)).length
    assertEquals(topRun, 4)
    assertEquals(sideRun, 12)
  }

  test("a mirrored text's pixels are the mirror of the text's") {
    assume(BrowserCapabilities.laysOutSVGText, "jsdom lays out no text")
    assume(
      BrowserCapabilities.isChromium,
      "WebKit and Firefox antialias mirrored glyphs their own way: of the 800 pixels 285 differed " +
        "on WebKit 26.4, by up to 255"
    )
    val text                                      = boxedText("Mirror")
    val (width, height, ahead)                    = render(text)
    val (mirroredWidth, mirroredHeight, mirrored) = render(text.flipHorizontally)
    assertEquals((mirroredWidth, mirroredHeight), (width, height))
    // Chromium on macOS mirrors the glyphs to within a level. On Linux its antialiasing of a
    // mirrored glyph measured up to 7 levels apart on 68 of 800 pixels, where glyphs half a pixel
    // off would differ by over a hundred.
    val unmirrored = Array.tabulate(width * height): i =>
      mirrored(i / width * width + width - 1 - i % width)
    assert(Rasterizer.sameUpTo(8, ahead, unmirrored), "the mirrored text is not the text's mirror")
    assert(ahead.exists(alpha(_) == 255), "the text draws nothing opaque")
  }

  test("grid-keeping content at 2x is its 1x rendering with every pixel doubled") {
    // The sprites are pixelated. A smooth bitmap doubled is resampled with blending.
    val content = Picture(
      sprite(7, 5).withResampling(ResamplingHint.Pixelated).moveTo(Pos(10.5, 4.5)),
      Rectangle(6, 4, Pos(-3, 2), Some(FillStyle(green)), None),
      Point(Pos(20.5, 0.5), blue),
      sprite(3, 4)
        .withResampling(ResamplingHint.Pixelated)
        .rotateBy(90)
        .flipHorizontally
        .moveTo(Pos(0, -6.5)),
      Rectangle(8, 6, Pos(0, 10), Some(FillStyle(red)), None).crop(Bounds(-2, 8, 3, 12))
    )
    val (width, height, once)    = render(content, 1.0)
    val (width2, height2, twice) = render(content, 2.0)
    assertEquals((width2, height2), (2 * width, 2 * height))
    for y <- 0 until height2; x <- 0 until width2 do
      assertEquals(
        twice(y * width2 + x),
        once((y / 2) * width + x / 2),
        s"pixel ($x, $y) at 2x is not ${(x / 2, y / 2)} at 1x"
      )
  }

  test("a cropped smoothed bitmap's edge pixels are its source's pixels") {
    val smooth =
      Bitmap(12, 8, pattern).withResampling(ResamplingHint.Smooth)
    // Columns 3 to 9 and rows 2 to 6 of the source.
    val cropped = smooth.crop(Bounds(-3, -2, 4, 3))
    // At 1x the crop is copied, so every edge pixel is its source pixel. At 2x it is resampled
    // with blending, and only the corners, which sample a quarter pixel in from the crop's corners,
    // see one source pixel each, as the edges repeat it.
    for scale <- Seq(1, 2) do
      val (w, h, p) = render(cropped, scale.toDouble)
      assertEquals((w, h), (7 * scale, 5 * scale))
      def checked(x: Int, y: Int) =
        val (side, end) = (x == 0 || x == w - 1, y == 0 || y == h - 1)
        if scale == 1 then side || end else side && end
      for y <- 0 until h; x <- 0 until w if checked(x, y) do
        assertEquals(
          p(y * w + x),
          pattern(3 + x / scale, 2 + y / scale).toARGBInt,
          s"pixel ($x, $y) at $scale"
        )
  }

  test("a smoothed crop samples nothing from outside it") {
    for (label, element, scales) <- fencedCrops; scale <- scales do
      val (_, _, p) = render(element, scale)
      val blue      = p.count(argb => (argb & 0xff) != 0)
      assertEquals(blue, 0, s"$label at $scale has $blue pixels with blue from outside it")
      assert(p.exists(alpha(_) == 255), s"$label at $scale draws nothing opaque")
  }

  test("a mask keeps the content where the mask is opaque, and an inverted one where it is not") {
    val square       = Rectangle(30, 30, Pos.Origin, Some(FillStyle(red)), None)
    val disc         = Circle(Pos.Origin, 8, Some(FillStyle(green)), None)
    val (w, _, kept) = render(MaskGroup(disc, square))
    assertEquals(kept((w / 2) * w + w / 2), red.toARGBInt)
    assertEquals(alpha(kept(0)), 0)
    val (wi, _, cut) = render(MaskGroup(disc, square, inverted = true))
    assertEquals(alpha(cut((wi / 2) * wi + wi / 2)), 0)
    assertEquals(cut(0), red.toARGBInt)
  }

  test("an axis-aligned clip is hard on whole device pixels and a turned one is antialiased") {
    val disc         = Circle(Pos.Origin, 40, Some(FillStyle(red)), None)
    val (w, h, hard) = render(disc.crop(Bounds(-10.3, -12.7, 15.2, 8.1)))
    assert(hard.forall(_ == red.toARGBInt), "a fractional axis-aligned clip left partial pixels")
    assertEquals((w, h), (27, 22))
    val (_, _, soft) = render(disc.crop(Bounds(-10, -10, 10, 10)).rotateBy(30))
    assert(soft.exists(p => alpha(p) > 0 && alpha(p) < 255), "the turned clip is not antialiased")
  }

  test("a clip ends with its operation") {
    // The clip of the left square must not keep the right one from being drawn.
    val content = Picture(
      Rectangle(10, 10, Pos(-10, 0), Some(FillStyle(red)), None).crop(Bounds(-15, -5, -5, 5)),
      Rectangle(10, 10, Pos(10, 0), Some(FillStyle(green)), None)
    )
    val (w, h, p) = render(content)
    assertEquals(p((h / 2) * w + w - 3), green.toARGBInt)
  }

  test("a wrap with a fractional period has no seam") {
    val filled = Rectangle(20.5, 10.25, Pos.Origin, Some(FillStyle(red)), None).wrapShift(5.3, 2)
    for scale <- Seq(1.0, 1.5, 2.0) do
      val (w, h, p) = render(filled, scale)
      // Every pixel but the outermost ones, which the period's edges may cover partly, is full.
      for y <- 1 until h - 1; x <- 1 until w - 1 do
        assertEquals(alpha(p(y * w + x)), 255, s"pixel ($x, $y) at $scale")
  }

  test("a whole-pixel wrap shift moves the pixels exactly") {
    val b                 = sprite(12, 8)
    val (w, h, shifted)   = render(b.wrapShift(0.0, 0.0).wrapShift(5, 3))
    val (_, _, reference) = render(b.wrapShift(5, 3))
    assertEquals((w, h), (12, 8))
    assert(shifted.sameElements(reference), "the generic wrap differs from the bitmap's own")
  }

  test("a bitmap of a picture is rendered at the density its scaling needs") {
    val picture   = Circle(Pos(0, 0), 2, Some(FillStyle(red)), None)
    val bitmap    = Bitmap.of(picture).withResampling(ResamplingHint.Pixelated)
    val (w, h, p) = render(Picture(bitmap).scaleBy(4))
    // Drawn from its 1x pixels, every 4x4 block would be one color.
    val blocky = (0 until h / 4).forall: row =>
      (0 until w / 4).forall: column =>
        val first = p(row * 4 * w + column * 4)
        (0 until 4).forall(dy =>
          (0 until 4).forall(dx => p((row * 4 + dy) * w + column * 4 + dx) == first)
        )
    assert(!blocky, "the picture was drawn from its 1x pixels")
  }

  test("a bitmap's pixels under a transform no step expresses are rendered through the plan") {
    val b = sprite(10, 6).scaleBy(0.01).rotateBy(90).scaleBy(100) match
      case bitmap: Bitmap => bitmap
      case other          => fail(s"$other is not a bitmap")
    val pixels = b.pixels(settings)
    assertEquals((pixels.width, pixels.height), (6, 10))
    // The source's pixel at column 3 and row 4 lands at column 4 and row 6.
    assertEquals(b.getColor(4, 6, settings), pattern(3, 4))
  }

  test("ops after a bitmap under a crop keep the crop, not a clip moved by the bitmap's map") {
    for scale <- Seq(1.0, 1.5, 2.0) do
      val (w, h, p) = render(shapeBitmapShape.crop(Bounds(-10, -10, 10, 10)), scale)
      assertEquals((w, h), (math.ceil(20 * scale).toInt, math.ceil(20 * scale).toInt))
      val wrong = p.count(_ != red.toARGBInt)
      assertEquals(wrong, 0, s"$wrong pixels are not red at $scale")
  }

  /** A bitmap of a 5000 x 5000 picture. */
  private def world: Bitmap =
    Bitmap.of(Rectangle(5000, 5000, Pos(2500, 2500), Some(FillStyle(green)), None))

  private def cropOf(b: Bitmap, left: Double, top: Double): Bitmap =
    b.crop(Bounds(left, top, left + 800, top + 600)) match
      case cropped: Bitmap => cropped
      case other           => fail(s"$other is not a bitmap")

  test("a crop of a large picture source renders only the part around what is drawn") {
    val view      = cropOf(world, 1000, 2000)
    val (w, h, p) = render(Picture(view), 2.0)
    assertEquals((w, h), (1600, 1200))
    assert(p.forall(_ == green.toARGBInt), "the crop is not all the picture's green")
    val rendered = view.source.renderedSourcePixelCount
    // The part drawn, rounded out to the tile grid at 2x, is at most 3x the pixels shown.
    assert(rendered <= 3L * 1600 * 1200, s"$rendered source pixels rendered for 1600 x 1200")
  }

  test("scrolling a crop of a picture source by a few pixels reuses its rendering") {
    // Counts on RasterCache.shared keeping what was rendered between the draws, which fits
    // its budget many times over.
    val b      = world
    val first  = cropOf(b, 1030, 2030)
    val _      = render(Picture(first), 2.0)
    val before = first.source.renderedSourcePixelCount
    for step <- 1 to 5 do
      val _ = render(Picture(cropOf(b, 1030 + 3 * step, 2030 + 2 * step)), 2.0)
    assertEquals(first.source.renderedSourcePixelCount, before)
  }

  /** The cases whose colors are opaque and that jsdom can draw, as `renderInto` is checked on. */
  private def opaqueCases: Seq[Case] =
    cases.filter(c =>
      Seq(
        "a mask",
        "a turned clip",
        "a sprite scaled by 1.5",
        "a turned wrap shift",
        "a stroked rectangle turned by 30 degrees"
      )
        .contains(c.label)
    )

  test(
    "renderInto on a canvas draws the bitmap createBitmapFrom gives at its corner, whatever its alpha, compositing, dash and shadow"
  ) {
    for scale <- Seq(1.0, 1.5, 2.0); c <- opaqueCases do
      val expected = JSRenderer.createBitmapFrom(c.element, scale, settings, 0L).pixels(settings)
      val canvas   = JSBufferAdapter.drawn(expected.width, expected.height): ctx =>
        ctx.globalAlpha = 0.5
        ctx.globalCompositeOperation = "xor"
        ctx.setLineDash(scala.scalajs.js.Array(2.0, 3.0))
        ctx.shadowColor = "rgb(0, 0, 255)"
        ctx.shadowOffsetX = 3
        JSRenderer.renderInto(ctx, c.element, scale, settings, 0L)
      assertEquals((canvas.width, canvas.height), (expected.width, expected.height))
      assert(
        Rasterizer.sameUpTo(Rasterizer.layerRoundingLevels, canvas.argbData, expected.argbData),
        s"${c.label} at $scale"
      )
  }

  test(
    "renderInto on a canvas draws the bitmap createBitmapFrom gives, whatever its smoothing quality and text state"
  ) {
    // A smoothed bitmap drawn smaller, which a high smoothing quality filters differently, and a
    // text with spaces and kerning pairs. jsdom lays out no text, so there the text draws nothing.
    val scene = Picture(
      sprite(40, 30).withResampling(ResamplingHint.Smooth).scaleBy(0.37).moveTo(Pos(0, -20)),
      boxedText("AV To. Wavy")
    )
    for scale <- Seq(1.0, 2.0) do
      val expected = JSRenderer.createBitmapFrom(scene, scale, settings, 0L).pixels(settings)
      val canvas   = JSBufferAdapter.drawn(expected.width, expected.height): ctx =>
        val state = ctx.asInstanceOf[scala.scalajs.js.Dynamic]
        state.imageSmoothingQuality = "high"
        state.direction = "rtl"
        state.letterSpacing = "3px"
        state.wordSpacing = "9px"
        state.fontKerning = "none"
        state.fontStretch = "ultra-condensed"
        state.fontVariantCaps = "all-small-caps"
        state.lang = "tr"
        JSRenderer.renderInto(ctx, scene, scale, settings, 0L)
      assert(canvas.argbData.sameElements(expected.argbData), s"at $scale")
  }

  test("the corner of the picture's device box lands on the context's translation, rounded") {
    // The bounds run from (-1, -1) to (1, 1), so the device box from (-1, -1) is 2 wide.
    val square = Rectangle(2, 2, Pos.Origin, Some(FillStyle(red)), None)
    val canvas = JSBufferAdapter.drawn(40, 40): ctx =>
      ctx.translate(10.4, 20.6)
      JSRenderer.renderInto(ctx, square, 1.0, settings, 0L)
    val inked =
      for
        y <- 0 until 40
        x <- 0 until 40
        if canvas.argbData(y * 40 + x) != 0
      yield (x, y)
    assertEquals(inked, Vector((10, 21), (11, 21), (10, 22), (11, 22)))
  }

  test("renderInto leaves the context's transform as it was") {
    val canvas = JSBufferAdapter(40, 40).newCanvas
    val ctx    = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.translate(3.5, 4)
    ctx.scale(2, 2)
    def transform =
      val m = ctx.asInstanceOf[scala.scalajs.js.Dynamic].getTransform()
      Seq(m.a, m.b, m.c, m.d, m.e, m.f).map(_.asInstanceOf[Double])
    val before = transform
    JSRenderer.renderInto(ctx, opaqueCases.head.element, 2.0, settings, 0L)
    assertEquals(transform, before)
  }

  test("a layer takes the smallest kept canvas it fits in") {
    val pool  = new CanvasPlanRenderer.LayerPool(1000000)
    val large = pool.acquire(200, 200)
    val small = pool.acquire(50, 50)
    val wide  = pool.acquire(300, 40)
    pool.release(large)
    pool.release(small)
    pool.release(wide)
    assert(pool.acquire(40, 40) eq small)
    assert(pool.acquire(150, 60) eq large)
    assert(pool.acquire(250, 10) eq wide)
  }

  test("the pool keeps canvases within its budget, the newest first") {
    val pool      = new CanvasPlanRenderer.LayerPool(25000)
    val (a, b, c) = (pool.acquire(100, 100), pool.acquire(100, 100), pool.acquire(100, 100))
    Seq(a, b, c).foreach(pool.release)
    assertEquals(pool.keptCount, 2)
    assert(pool.acquire(100, 100) eq c)
    assert(pool.acquire(100, 100) eq b)
    val huge = new CanvasPlanRenderer.LayerPool(1000000).acquire(200, 200)
    pool.release(huge)
    assertEquals(pool.keptCount, 0)
  }

  for d <- DeepTrees.nested; scale <- Seq(1.0, 1.5) do
    test(s"${d.name} draws the same at any nesting limit at $scale") {
      val tree                  = d.build(40)
      val (width, height, argb) = render(tree, scale, nesting = Int.MaxValue)
      for nesting <- Seq(1, 2, 16) do
        val (w, h, bounded) = render(tree, scale, nesting)
        assertEquals((w, h), (width, height), s"at nesting $nesting")
        assert(
          Rasterizer.sameUpTo(Rasterizer.layerRoundingLevels, bounded, argb),
          s"the pixels differ at nesting $nesting"
        )
    }
end CanvasPlanRendererSuite
