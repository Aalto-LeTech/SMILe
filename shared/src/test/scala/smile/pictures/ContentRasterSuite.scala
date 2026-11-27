package smile.pictures

import smile.{RenderSettings, ResamplingMode}
import smile.colors.Color
import smile.infrastructure.{NativeBuffer, RasterCache, Renderer}
import smile.modeling.{Affine, Bounds, Pos}
import smile.testing.{Rasterizer, Slow}

/** A raster of a picture keeps the picture as its source through crops and wraparound shifts, so
  * that each output renders the picture at its own scale. Shifts and crops fold, so that a raster
  * shifted every tick stays one step over at most two tiles, and a raster whose tiles multiply is
  * flattened into plain pixels. A raster with a long history keeps its picture.
  */
class ContentRasterSuite extends munit.FunSuite:

  private val direct   = RenderSettings.Default
  private val stepwise = RenderSettings(
    bufferScalingMethod = Rasterizer.replayScaling,
    resamplingMode = ResamplingMode.Stepwise
  )

  /** A vector scene of 100 by 40 pixels: an antialiased circle partly over a translucent box and
    * partly over nothing, above opaque ground.
    */
  private def scene: Element =
    Picture(
      Circle(Pos(30, 15), 11.3, fillWith(Color(250, 200, 20)), None),
      Rectangle(30, 20, Pos(60, 14), fillWith(Color(20, 40, 220, 120)), None),
      Rectangle(100, 8, Pos(50, 36), fillWith(Color(30, 160, 40)), None)
    ).crop(Bounds(Pos(0, 0), Pos(100, 40)))

  /** `e`, which an operation of a bitmap's own gives, as the bitmap it is. */
  private def asBitmap(e: Element): Bitmap = e match
    case bitmap: Bitmap => bitmap
    case other          => fail(s"Expected a bitmap, was $other")

  /** The bitmap's crop of the world rectangle from `(left, top)` to `(right, bottom)`. */
  private def cropOf(b: Bitmap, left: Double, top: Double, right: Double, bottom: Double): Bitmap =
    asBitmap(b.crop(Bounds(left, top, right, bottom)))

  private def render(picture: Element, scale: Double, settings: RenderSettings): NativeBuffer =
    Renderer().createBitmapFrom(picture, scale, settings).nativeBuffer(settings)

  private def argb(buffer: NativeBuffer): Vector[Int] = buffer.argbData.toVector

  /** The pixels of `picture` rendered at `scale` that the whole logical pixels from `(left, top)`
    * to `(right, bottom)` cover, read from a rendering of the whole picture, which starts on a
    * whole logical pixel.
    *
    * A reference for a crop of a picture that does not render the picture cropped. Chromium
    * antialiases a curve that a clip cuts differently from one it draws whole (100 against 112 of
    * 255 three rows above the cut, where the exact coverage is about 112), and a crop of a bitmap
    * of the picture shows the picture drawn whole.
    */
  private def uncut(
      picture: Element,
      left: Int,
      top: Int,
      right: Int,
      bottom: Int,
      scale: Int
  ): Array[Int] =
    val whole    = Renderer().createBitmapFrom(picture, scale, direct)
    val corner   = whole.bounds.get.upperLeftCorner
    val pixels   = whole.pixels(direct)
    val data     = pixels.argbData
    val (x0, y0) = ((left - corner.x.toInt) * scale, (top - corner.y.toInt) * scale)
    val (w, h)   = ((right - left) * scale, (bottom - top) * scale)
    Array.tabulate(w * h)(index => data((y0 + index / w) * pixels.width + x0 + index % w))

  /** The pixels that a bitmap that copied its pixels at every operation would hold. Crops and joins
    * copy pixels exactly, as a JVM buffer's do. A canvas would hold the same pixels, but reading
    * them out and writing them back loses some of a translucent pixel's color each time, so the
    * eager pixels are kept as packed ARGB values between operations.
    */
  private final case class Eager(width: Int, height: Int, data: Vector[Int]):

    def crop(left: Int, top: Int, right: Int, bottom: Int): Eager =
      Eager(
        right - left,
        bottom - top,
        (top until bottom).toVector.flatMap(y => data.slice(y * width + left, y * width + right))
      )

    /** An eager wraparound shift. The pixels are cropped into the pieces that wrap around, and the
      * pieces are joined again edge to edge where the shift takes them.
      */
    def wrapShift(dx: Int, dy: Int): Eager =
      val sx     = Math.floorMod(dx, width)
      val sy     = Math.floorMod(dy, height)
      val joined = new Array[Int](width * height)
      for
        (fromX, toX, pieceWidth)  <- Seq((0, sx, width - sx), (width - sx, 0, sx))
        (fromY, toY, pieceHeight) <- Seq((0, sy, height - sy), (height - sy, 0, sy))
        if pieceWidth > 0 && pieceHeight > 0
      do
        val piece = crop(fromX, fromY, fromX + pieceWidth, fromY + pieceHeight).data
        for row <- 0 until pieceHeight do
          piece
            .slice(row * pieceWidth, (row + 1) * pieceWidth)
            .copyToArray(
              joined,
              (toY + row) * width + toX
            )
      Eager(width, height, joined.toVector)
  end Eager

  private def eager(buffer: NativeBuffer): Eager =
    Eager(buffer.width, buffer.height, argb(buffer))

  /** The same shift of dense pixels, for comparing a rendering at a scale with its source. */
  private def shiftedArray(data: Array[Int], width: Int, height: Int, dx: Int, dy: Int) =
    Array.tabulate(width * height): index =>
      val (x, y) = (index % width, index / width)
      data(Math.floorMod(y - dy, height) * width + Math.floorMod(x - dx, width))

  private def channelsWithin(a: Int, b: Int, tolerance: Int): Boolean =
    Seq(24, 16, 8, 0).forall(shift =>
      (((a >>> shift) & 0xff) - ((b >>> shift) & 0xff)).abs <= tolerance
    )

  test(
    "10,000 wraparound shifts of a picture are one step over two tiles, with eager pixels".tag(Slow)
  ) {
    val base     = Bitmap.of(scene)
    var shifted  = base
    var expected = eager(base.nativeBuffer(stepwise))
    val compared = Set(1, 2, 37, 9999, 10000)
    for tick <- 1 to 10000 do
      shifted = shifted.wrapShift(-2, 0).asBitmap
      expected = expected.wrapShift(-2, 0)
      assert(shifted.source.source.isInstanceOf[BitmapSource.Content], s"tick $tick")
      // Shifts that add up to none, every 50 ticks across the 100 pixels, leave no step.
      assertEquals(shifted.source.steps.length, if tick % 50 == 0 then 0 else 1, s"tick $tick")
      assert(
        shifted.source.plan.tiles.length <= 2,
        s"tick $tick: ${shifted.source.plan.tiles.length} tiles"
      )
      if compared(tick) then
        assertEquals(argb(shifted.nativeBuffer(stepwise)), expected.data, s"tick $tick")
        assertEquals(shifted.bounds, base.bounds)
  }

  test("a wraparound shift in both directions matches the eager shift and folds") {
    val base    = Bitmap.of(scene)
    val shifted =
      base.wrapShift(13, -7).asBitmap.wrapShift(-50, 22).asBitmap.wrapShift(201, 41).asBitmap
    assertEquals(shifted.source.steps.length, 1)
    assert(shifted.source.plan.tiles.length <= 4)
    val expected =
      eager(base.nativeBuffer(stepwise)).wrapShift(13, -7).wrapShift(-50, 22).wrapShift(201, 41)
    assertEquals(argb(shifted.nativeBuffer(stepwise)), expected.data)
  }

  test("consecutive crops fold into their intersection") {
    val cropped =
      cropOf(cropOf(cropOf(Bitmap.of(scene), 10, 5, 90, 38), 15, 6, 85, 37), 20, 8, 80, 36)
    assertEquals(cropped.source.steps.length, 1)
    assert(cropped.source.source.isInstanceOf[BitmapSource.Content], "the crops keep the picture")
    assertEquals(cropped.bounds, Some(Bounds(Pos(20, 8), Pos(80, 36))))
    val expected = eager(Bitmap.of(scene).nativeBuffer(stepwise)).crop(20, 8, 80, 36)
    assertEquals(argb(cropped.nativeBuffer(stepwise)), expected.data)
  }

  test("a crop of a vector picture drawn directly at scale 2 is the picture rendered at scale 2") {
    val circle  = Circle(Pos(20, 20), 15, fillWith(Color(200, 30, 30)), None)
    val cropped = cropOf(Bitmap.of(circle), 5, 5, 25, 35)
    assert(cropped.source.source.isInstanceOf[BitmapSource.Content], "the crop keeps the picture")

    val drawn     = render(cropped, 2, direct)
    val reference = render(circle, 2, direct)
    assertEquals((drawn.width, drawn.height), (40, 60))

    val drawnData     = drawn.argbData
    val referenceData = reference.argbData
    val translucent   = drawnData.count(argb => (argb >>> 24) > 0 && (argb >>> 24) < 255)
    assert(translucent > 20, s"only $translucent antialiased pixels on the circle's edge")
    for y <- 0 until 60; x <- 0 until 40 do
      val (obtained, expected) = (drawnData(y * 40 + x), referenceData(y * 60 + x))
      assert(
        channelsWithin(obtained, expected, 2),
        f"($x, $y): $obtained%08x, expected $expected%08x"
      )
  }

  test(
    "a crop to a small part of a large source keeps the source and is drawn sharply at scale 2"
  ) {
    val circle = Circle(Pos(105, 105), 4.3, fillWith(Color(10, 120, 200)), None)
    val large  = Bitmap.of(circle.crop(Bounds(Pos(0, 0), Pos(1000, 1000))))
    val small  = cropOf(large, 100, 100, 110, 110)
    assert(small.source.source.isInstanceOf[BitmapSource.Content], "the crop keeps the picture")
    assertEquals(small.source.steps.length, 1)
    assertEquals(small.source.pixelSize, (10, 10))
    assertEquals(small.bounds, Some(Bounds(Pos(100, 100), Pos(110, 110))))

    // Its Stepwise pixels are the crop of the picture's pixels.
    val expected = eager(large.nativeBuffer(stepwise)).crop(100, 100, 110, 110)
    assertEquals(argb(small.nativeBuffer(stepwise)), expected.data)

    // Drawn directly at scale 2, it is the picture rendered at scale 2. It is not the picture's
    // 1x pixels enlarged.
    val drawn     = render(small, 2, direct)
    val reference = render(circle.crop(Bounds(Pos(100, 100), Pos(110, 110))), 2, direct)
    assertEquals((drawn.width, drawn.height), (20, 20))
    val drawnData     = drawn.argbData
    val referenceData = reference.argbData
    val translucent   = drawnData.count(argb => (argb >>> 24) > 0 && (argb >>> 24) < 255)
    assert(translucent > 10, s"only $translucent antialiased pixels on the circle's edge")
    for index <- drawnData.indices do
      assert(
        channelsWithin(drawnData(index), referenceData(index), 2),
        f"pixel $index: ${drawnData(index)}%08x, expected ${referenceData(index)}%08x"
      )
  }

  test(
    "a chain of 40 alternating crops and shifts is flattened by the tile cap and keeps the eager pixels"
  ) {
    var raster   = Bitmap.of(scene)
    var expected = eager(raster.nativeBuffer(stepwise))
    for step <- 1 to 40 do
      if step % 2 == 1 then
        val corner = raster.bounds.get.upperLeftCorner
        val lower  = raster.bounds.get.lowerRightCorner
        raster = cropOf(raster, corner.x, corner.y, lower.x - 1, lower.y)
        expected = expected.crop(0, 0, expected.width - 1, expected.height)
      else
        raster = raster.wrapShift(3, 1).asBitmap
        expected = expected.wrapShift(3, 1)
      assert(raster.source.steps.length <= 32, s"step $step: ${raster.source.steps.length} steps")
      assert(
        raster.source.plan.tiles.length <= 8,
        s"step $step: ${raster.source.plan.tiles.length} tiles"
      )
    assert(raster.source.source.isInstanceOf[BitmapSource.Pixels], "the chain is flattened")
    assertEquals(argb(raster.nativeBuffer(stepwise)), expected.data)
  }

  test("a chain of small rotations folds into one turn by their total") {
    val source = Bitmap.generateARGB(
      12,
      8,
      (x, y) => 0xff000000 | (x * 20) << 16 | (y * 30) << 8 | 90
    )
    var raster = source
    for turn <- 1 to 40 do
      raster = raster.rotateBy(1)
      assertEquals(raster.source.steps.length, 1, s"turn $turn")
    // One turn of the source's pixels by the total, resampled once.
    val pixels = source.nativeBuffer(stepwise)
    val turned =
      pixels.transform(Affine.rotation(40, Pos(pixels.width / 2.0, pixels.height / 2.0)), stepwise)
    assertEquals(argb(raster.nativeBuffer(stepwise)), argb(turned))
  }

  test("a chain of small rotations read on every tick still folds into one turn") {
    val source = Bitmap.generateARGB(12, 8, (x, y) => 0xff000000 | (x * 20) << 16 | (y * 30) << 8)
    var raster = source
    for turn <- 1 to 50 do
      raster = raster.rotateBy(3)
      val _ = raster.nativeBuffer(stepwise)
      assertEquals(raster.source.steps.length, 1, s"turn $turn")
    val pixels = source.nativeBuffer(stepwise)
    val turned =
      pixels.transform(Affine.rotation(150, Pos(pixels.width / 2.0, pixels.height / 2.0)), stepwise)
    assertEquals(argb(raster.nativeBuffer(stepwise)), argb(turned))
  }

  test("a picture's bitmap through 40 operations read in Stepwise is still drawn sharply") {
    val start  = Bitmap.of(Circle(Pos(20, 20), 9.3, fillWith(Color(10, 120, 200)), None))
    var raster = start
    for tick <- 0 until 40 do
      raster = tick % 4 match
        case 0 => asBitmap(raster.scaleBy(1.25))
        case 2 => asBitmap(raster.scaleBy(0.8))
        case _ => raster.flipHorizontally
      val _ = raster.nativeBuffer(stepwise)
    assert(raster.source.steps.length <= 32, s"${raster.source.steps.length} steps")
    assert(raster.source.source.isInstanceOf[BitmapSource.Content], "the chain keeps the picture")
    // The operations add up to none, so drawn directly at scale 2 it is the picture at scale 2.
    val drawn     = render(raster, 2, direct)
    val reference = render(start, 2, direct)
    assertEquals((drawn.width, drawn.height), (reference.width, reference.height))
    val obtained  = drawn.argbData
    val expected  = reference.argbData
    val differing =
      obtained.indices.count(index => !channelsWithin(obtained(index), expected(index), 2))
    assertEquals(differing, 0)
  }

  test("a shifted picture drawn at scale 2 is the picture rendered at scale 2 and shifted") {
    // Each platform's rendering meets the same construction from its own rendering of the scene,
    // so the JVM and the canvas agree on it.
    val shifted   = Bitmap.of(scene).wrapShift(-30, 7)
    val drawn     = render(shifted, 2, direct)
    val reference = render(scene, 2, direct)
    assertEquals((drawn.width, drawn.height), (200, 80))
    val expected  = shiftedArray(reference.argbData, 200, 80, -60, 14)
    val obtained  = drawn.argbData
    val differing =
      obtained.indices.count(index => !channelsWithin(obtained(index), expected(index), 2))
    assertEquals(differing, 0)
  }

  test("a directly drawn picture is rendered once per density and settings") {
    // Counts on RasterCache.shared keeping what was rendered between the draws, which fits
    // its budget many times over.
    val raster = Bitmap.of(scene)
    val _      = render(raster, 2, direct)
    val count  = raster.source.renderedSourcePixelCount
    assert(count > 0, "the picture is rendered")
    val _ = render(raster.wrapShift(5, 0).moveBy(3, 4), 2, direct)
    assertEquals(
      raster.source.renderedSourcePixelCount,
      count,
      "the rendering at density 2 is shared by the bitmaps made from it"
    )
  }

  test("a source drawn at five densities keeps its first rendering") {
    // Counts on RasterCache.shared keeping what was rendered between the draws, which fits
    // its budget many times over.
    val raster = Bitmap.of(scene)
    for scale <- Seq(1.0, 1.5, 2, 3, 4) do
      val _ = render(raster, scale, direct)
    val count = raster.source.renderedSourcePixelCount
    val _     = render(raster, 1, direct)
    assertEquals(raster.source.renderedSourcePixelCount, count, "the first rendering is kept")
  }

  test("a picture source draws the same pixels from a warm cache, an emptied one and none") {
    // A source that draws another source, so that both renderings come from the cache.
    val inner = Bitmap.of(scene)
    val outer = Bitmap.of(
      Picture(
        inner.moveBy(3, 2),
        Circle(Pos(70, 20), 9.6, fillWith(Color(200, 30, 90, 140)), None)
      ).crop(Bounds(Pos(0, 0), Pos(100, 40)))
    )
    // A bitmap of the same source with no pixels of its own kept, so that each draw, Stepwise
    // ones too, takes the renderings from the cache.
    def fresh: Bitmap  = Rasters.of(outer.source.withoutCheckpoint)
    def rendered: Long =
      outer.source.renderedSourcePixelCount + inner.source.renderedSourcePixelCount
    for (settings, scale) <- Seq((direct, 1.0), (direct, 2.5), (stepwise, 1.0)) do
      // The pixels of a fresh bitmap of the source, and whether it rendered anything.
      def drawn(): (Vector[Int], Boolean) =
        val before = rendered
        val pixels = argb(render(fresh, scale, settings))
        (pixels, rendered > before)
      RasterCache.shared.withBudget(0)(())
      val (cold, coldRendered) = drawn()
      val (warm, warmRendered) = drawn()
      RasterCache.shared.withBudget(0)(())
      val (again, againRendered) = drawn()
      val (none, noneRendered)   = RasterCache.shared.withBudget(0)(drawn())
      val context                = s"at $scale with $settings"
      assert(coldRendered && againRendered && noneRendered, s"a cold draw renders $context")
      assert(!warmRendered, s"a warm draw renders nothing $context")
      assertEquals(warm, cold, s"warm $context")
      assertEquals(again, cold, s"emptied $context")
      assertEquals(none, cold, s"with nothing kept $context")
  }

  test("the directly drawn pixels have the size of the replayed ones after a fractional scale") {
    val source = Bitmap.generateARGB(3, 3, (x, y) => 0xff000000 | (x * 80) << 16 | (y * 80))
    val turned = asBitmap(source.scaleTo(5.9, 5.9)).rotateBy(45)
    val pixels = turned.pixels(direct)
    assertEquals((pixels.width, pixels.height), turned.source.pixelSize)
    val (width, height) = turned.source.pixelSize
    val recolored       = turned.setColorsByLocation((_, _) => Color(1, 2, 3))
    assertEquals(recolored.source.pixelSize, turned.source.pixelSize)
    // Cropped to its turned buffer, it has those bounds, as any element's crop does.
    val corner = turned.source.boundary.upperLeftCorner
    val buffer = Bounds(corner.x, corner.y, corner.x + width, corner.y + height)
    assertEquals(turned.crop(buffer).bounds, Some(buffer))
  }

  test("a scaled, turned and flipped bitmap drawn directly lies where its replay lies") {
    // Red rises and falls by 40 per source pixel across, and green by 40 per source pixel down,
    // 20 per world pixel once scaled by 2, so a pixel misplaced by one shows as a large difference.
    def wave(index: Int): Int = 40 * (6 - (Math.floorMod(index, 12) - 6).abs)
    val source                = Bitmap
      .generateARGB(36, 28, (x, y) => 0xff000000 | wave(x) << 16 | wave(y + 3) << 8 | 128)
      .moveBy(40, 30)
    val pivot  = Pos(45, 35)
    val placed = asBitmap(source.scaleBy(2, 2)).rotateAround(30, pivot).flipHorizontally
    val frame  = placed.crop(Bounds(Pos(0, 0), Pos(160, 160)))
    val drawn  = render(frame, 1, direct).argbData
    val replay =
      render(frame, 1, RenderSettings(resamplingMode = ResamplingMode.Stepwise)).argbData

    def opaqueAround(data: Array[Int], x: Int, y: Int): Boolean =
      (-2 to 2).forall(dy => (-2 to 2).forall(dx => (data((y + dy) * 160 + x + dx) >>> 24) == 255))

    val compared =
      for
        y <- 2 until 158; x <- 2 until 158
        if opaqueAround(drawn, x, y) && opaqueAround(replay, x, y)
      yield (x, y)
    assert(compared.length > 1000, s"only ${compared.length} pixels to compare")
    // The two resample differently by up to 10. Drawn half a pixel off, they differ by 14.
    for (x, y) <- compared do
      val (a, b) = (drawn(y * 160 + x), replay(y * 160 + x))
      assert(
        channelsWithin(a, b, Rasterizer.replayDifference),
        f"($x, $y): $a%08x directly, $b%08x replayed"
      )
  }

  test("a picture with fractional bounds is laid on the whole pixels that cover them") {
    // The circle's bounds reach from 8.7 to 31.3, so the 24 whole pixels from 8 to 32 cover them,
    // and the crop keeps those from 8 to 28 across and from 8 to 31 down. A crop off those whole
    // pixels has the bounds asked for, as any element's crop does.
    val circle = Circle(Pos(20, 20), 11.3, fillWith(Color(200, 30, 30)), None)
    val bitmap = Bitmap.of(circle)
    assertEquals(bitmap.bounds, Some(Bounds(8, 8, 32, 32)))
    assertEquals(bitmap.source.pixelSize, (24, 24))
    val cropped = cropOf(bitmap, 8, 8, 28, 31)
    assertEquals(cropped.source.pixelSize, (20, 23))
    assertEquals(
      bitmap.crop(Bounds(8.7, 8.7, 28.7, 31.3)).bounds,
      Some(Bounds(8.7, 8.7, 28.7, 31.3))
    )
    for scale <- Seq(1, 2) do
      val drawn     = render(cropped, scale, direct).argbData
      val reference = uncut(Picture(circle, blank(40, 40).moveTo(Pos(20, 20))), 8, 8, 28, 31, scale)
      assertEquals(drawn.length, reference.length)
      // A canvas antialiases the circle a little differently where it is rendered moved.
      for index <- drawn.indices do
        assert(
          channelsWithin(drawn(index), reference(index), 4),
          f"scale $scale, pixel $index: ${drawn(index)}%08x, expected ${reference(index)}%08x"
        )
  }

  test("a small crop of a large picture renders only the part of the picture it draws") {
    val large = Picture(
      Circle(Pos(1503, 1004), 7.3, fillWith(Color(250, 200, 20)), None),
      Rectangle(2990, 1990, Pos(1500, 1000), fillWith(Color(20, 40, 220, 120)), None)
    ).crop(Bounds(Pos(0, 0), Pos(3000, 2000)))
    val crop  = cropOf(Bitmap.of(large), 1497.0, 998.0, 1507.0, 1008.0)
    val drawn = render(crop, 2, direct)
    // The crop's part of the picture out to the grid of 256 source pixels around it, at density
    // 2. That is far less than the 6000 by 4000 pixels of the whole picture.
    val rendered = crop.source.renderedSourcePixelCount
    assert(rendered <= 512L * 512, s"rendered $rendered pixels")
    val reference = uncut(large.crop(Bounds(1480, 980, 1530, 1030)), 1497, 998, 1507, 1008, 2)
    assertEquals((drawn.width, drawn.height), (20, 20))
    val (obtained, expected) = (drawn.argbData, reference)
    val differing            =
      obtained.indices.count(index => !channelsWithin(obtained(index), expected(index), 2))
    assertEquals(differing, 0)
  }
end ContentRasterSuite
