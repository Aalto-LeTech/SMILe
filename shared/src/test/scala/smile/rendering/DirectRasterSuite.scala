package smile.rendering

import smile.{RenderSettings, ResamplingMode, ScalingMethod}
import smile.colors.Color
import smile.infrastructure.Renderer
import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.testing.{Rasterizer, Slow}

/** In `Direct` mode an output draws a bitmap's source pixels once, with its scaling and turning
  * composed into one map, at the output's own scale. In `Stepwise` mode it draws the pixels
  * resampled at every operation, at one buffer pixel per logical pixel.
  */
class DirectRasterSuite extends munit.FunSuite:

  private val direct   = RenderSettings()
  private val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)

  /** The settings of the eager bitmap of SMILe 1 on the JVM, which averaged areas as it scaled. */
  private val areaAveragingStepwise =
    stepwise.copy(bufferScalingMethod = ScalingMethod.AreaAveraging)

  private def render(picture: Element, scale: Double, settings: RenderSettings): Bitmap =
    Renderer().createBitmapFrom(picture, scale, settings)

  private def within(frame: Bounds, elements: Element*): Element =
    Picture(elements*).crop(frame)

  /** How many pixels of a rendering of red and blue mix the two, a measure of its edges' width. */
  private def blurredPixels(bitmap: Bitmap): Int =
    bitmap
      .pixels()
      .argbData
      .count: argb =>
        val red  = (argb >> 16) & 0xff
        val blue = argb & 0xff
        (argb >>> 24) == 255 && red > 40 && blue > 40

  test("a scaled and turned bitmap drawn at twice the scale is sharper than its 1x pixels") {
    // Red and blue squares of eight pixels, shrunk and turned. Drawn directly at 2x, their edges
    // are resampled at 2x. Enlarged from the 1x pixels, they are twice as wide as at 1x.
    val red     = Color(255, 0, 0).toARGBInt
    val blue    = Color(0, 0, 255).toARGBInt
    val checker =
      Bitmap.generateARGB(64, 64, (x, y) => if (x / 8 + y / 8) % 2 == 0 then red else blue)
    val turned   = checker.scaleBy(0.37, 0.37).rotateAround(17, checker.anchor)
    val frame    = Bounds(checker.anchor + (-16.0, -16.0), checker.anchor + (16.0, 16.0))
    val picture  = within(frame, turned)
    val drawn    = render(picture, 2, direct)
    val enlarged = render(within(frame, render(picture, 1, stepwise)), 2, direct)

    assertEquals(drawn.pixels().width, 64)
    assertEquals(enlarged.pixels().width, 64)
    assertNotEquals(drawn.pixels().argbData.toSeq, enlarged.pixels().argbData.toSeq)
    assert(
      blurredPixels(drawn) < blurredPixels(enlarged) * 3 / 4,
      s"direct: ${blurredPixels(drawn)} mixed pixels, enlarged: ${blurredPixels(enlarged)}"
    )
  }

  /** The Grouse: a smooth white disk on a 700 by 700 bitmap, fading out well inside its edges, and
    * scaled every frame to a slightly smaller size around the center of a 600 by 600 view.
    */
  private val grouse: Bitmap =
    Bitmap
      .generateARGB(
        700,
        700,
        (x, y) =>
          val distance = math.hypot(x + 0.5 - 350, y + 0.5 - 350)
          val fade     = (1 - distance / 320).max(0)
          val alpha    = (fade * fade * 255).round.toInt
          (alpha << 24) | 0xffffff
      )
      .moveBy(300, 300)

  private val view = Bounds(Pos(0, 0), Pos(600, 600))

  /** The center of the ink of a rendering, weighted by opacity, in logical pixels. */
  private def centerOfMass(bitmap: Bitmap, scale: Double): Pos =
    val pixels  = bitmap.pixels()
    val argb    = pixels.argbData
    var total   = 0.0
    var xMoment = 0.0
    var yMoment = 0.0
    var index   = 0
    while index < argb.length do
      val alpha = (argb(index) >>> 24).toDouble
      if alpha > 0 then
        total += alpha
        xMoment += alpha * (index % pixels.width + 0.5)
        yMoment += alpha * (index / pixels.width + 0.5)
      index += 1
    Pos(xMoment / total / scale, yMoment / total / scale)

  /** The largest distance the center of mass moves between two frames, as the bitmap shrinks from
    * 400 to 360 pixels in steps of half a pixel.
    */
  private def largestJump(scale: Double, settings: RenderSettings): Double =
    val sizes   = BigDecimal(400) to BigDecimal(360) by BigDecimal(-0.5)
    val centers = sizes.map: size =>
      // Around the grouse's anchor, the middle of the view.
      val frame = grouse.scaleTo(size.toDouble, size.toDouble)
      centerOfMass(render(within(view, frame), scale, settings), scale)
    centers
      .zip(centers.tail)
      .map((from, to) => math.hypot(to.x - from.x, to.y - from.y))
      .max

  test("the Grouse stays steady at 1x when it is drawn directly".tag(Slow)) {
    val jump = largestJump(1, direct)
    assert(jump <= 0.12, s"the center of mass jumps by $jump px")
  }

  test("the Grouse stays steady at 2x when it is drawn directly".tag(Slow)) {
    val jump = largestJump(2, direct)
    assert(jump <= 0.1, s"the center of mass jumps by $jump px")
  }

  test("the Grouse shimmers when it is resampled at every frame, averaging areas".tag(Slow)) {
    // Only the JVM averages areas, which is what the eager bitmap of SMILe 1 did there.
    assume(Rasterizer.averagesAreas, "this platform has no area averaging")
    val jump = largestJump(1, areaAveragingStepwise)
    assert(jump > 0.5, s"the center of mass jumps by only $jump px")
  }

  test("an unscaled bitmap between pixels is copied onto whole pixels at 1x in both modes") {
    val red = Color(255, 0, 0)
    // Centered at the origin, its corner at (-2, -2), and moved to put the corner at (10.4, 3).
    val bitmap = Bitmap(4, 4, Seq.fill(16)(red)).moveBy(12.4, 5)
    val frame  = Bounds(Pos(0, 0), Pos(20, 10))
    for (settings, scale) <- Seq((direct, 1.0), (stepwise, 1.0), (direct, 2.0)) do
      val rendered             = render(within(frame, bitmap), scale, settings)
      def clue(x: Int, y: Int) =
        s"${settings.resamplingMode} at ${scale}x, pixel ($x, $y): ${rendered.getColor(x, y)}"
      for y <- 0 until rendered.pixels().height; x <- 0 until rendered.pixels().width do
        // At 1x the corner snaps to the nearest whole device pixel and the pixels are copied onto
        // the whole pixels from 10 to 14. At 2x the bitmap is resampled where it is, from 20.8 to
        // 28.8 device pixels, which covers the centers of the device pixels from 21 to 28.
        val (left, top) = if scale == 1 then (10, 3) else (21, 6)
        val size        = if scale == 1 then 4 else 8
        val inside      = x >= left && x < left + size && y >= top && y < top + size
        if inside then assertEquals(rendered.getColor(x, y), red, clue(x, y))
        else assertEquals(rendered.getColor(x, y).opacity, 0, clue(x, y))
  }

  test("a directly drawn bitmap is clipped once on an edge its clip and its source share") {
    val square = Bitmap.generateARGB(4, 4, (_, _) => 0xffff0000)
    // Scaled to ten by ten around its middle, the bitmap has its corner at (-5, -5). The move puts
    // its left and upper edges at 0.5, where it is resampled. The crop's left and upper edges are
    // the source's own, so cropping changes no pixel and the edge is cut once.
    val scaled    = square.scaleTo(10, 10).moveBy(5.5, 5.5)
    val frame     = Bounds(0, 0, 12, 12)
    val cropped   = render(within(frame, scaled.crop(Bounds(0.5, 0.5, 12, 12))), 1.0, direct)
    val uncropped = render(within(frame, scaled), 1.0, direct)
    assertEquals(cropped.pixels().argbData.toSeq, uncropped.pixels().argbData.toSeq)
    // The edge pixel, whose center the edge passes through, is drawn whole, as a hard edge on the
    // device's axes is.
    assertEquals(cropped.getColor(0, 5).opacity, 255)
    assertEquals(cropped.getColor(10, 5).opacity, 0)
  }

  test("a bitmap drawn directly has the pixels a 1x rendering of it has") {
    val source = Bitmap.generateARGB(20, 16, (x, y) => 0xff000000 | (x * 12 << 16) | (y * 15 << 8))
    val turned =
      source.scaleBy(1.3, 1.3).rotateAround(20, source.anchor).moveBy(0.3, 0.6).asBitmap
    val pixels = turned.pixels(direct)

    // The pixels are as many as the replayed ones, which crops and pixel reads count with, laid
    // from the upper left corner of the turned buffer.
    val (width, height) = turned.source.pixelSize
    val corner          =
      turned.source.boundary.moveBy(turned.affine.e, turned.affine.f).upperLeftCorner
    val drawn = render(
      turned.moveBy(-corner.x, -corner.y).crop(Bounds(0, 0, width.toDouble, height.toDouble)),
      1,
      direct
    ).pixels()
    assertEquals((pixels.width, pixels.height), (width, height))
    assertEquals((drawn.width, drawn.height), (width, height))
    // The same up to the rounding of the two placements to the 2^-20 grid.
    for (a, b) <- pixels.argbData.zip(drawn.argbData) do
      assert(
        Seq(24, 16, 8, 0).forall(s => (((a >>> s) & 0xff) - ((b >>> s) & 0xff)).abs <= 2),
        f"$a%08x, drawn $b%08x"
      )
    assertNotEquals(pixels.argbData.toSeq, turned.pixels(stepwise).argbData.toSeq)
  }
end DirectRasterSuite
