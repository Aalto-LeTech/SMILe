package smile.pictures

import smile.{RenderSettings, ResamplingMode, ScalingMethod}
import smile.colors.Color
import smile.infrastructure.{BufferAdapter, ImageOperations, NativeBuffer, Renderer}
import smile.modeling.{Affine, Bounds, Pos}
import smile.testing.Rasterizer

/** A bitmap records its operations and computes its pixels by replaying them. In `Stepwise` mode
  * the replay must give exactly the pixels the eager bitmap of SMILe 1 gave, which resampled the
  * buffer at every operation. The oracle here is that eager code, applied to the same source
  * buffer.
  */
class StepwiseReplaySuite extends munit.FunSuite:

  private val settings = RenderSettings(
    bufferScalingMethod = Rasterizer.replayScaling,
    resamplingMode = ResamplingMode.Stepwise
  )

  /** Eight by six lopsided pixels, some translucent, with the upper left corner at the origin. */
  private def source: Bitmap =
    Bitmap(
      8,
      6,
      Seq.tabulate(48): index =>
        val (x, y) = (index % 8, index / 8)
        Color((x * 37) % 256, (y * 53) % 256, ((x + y) * 29) % 256, if x == 7 then 128 else 255)
    ).moveBy(4, 3)

  private val pivot = Pos(3, -2)

  /** `e`, which an operation of a bitmap's own gives, as the bitmap it is. */
  private def asBitmap(e: Element): Bitmap = e match
    case bitmap: Bitmap => bitmap
    case other          => fail(s"Expected a bitmap, was $other")

  /** Where the pixels of `bitmap`, which only its own operations have placed, lie in the world. */
  private def boundaryOf(bitmap: Bitmap): Bounds =
    assertEquals(
      (bitmap.affine.a, bitmap.affine.b, bitmap.affine.c, bitmap.affine.d),
      (1.0, 0.0, 0.0, 1.0)
    )
    bitmap.source.boundary.moveBy(bitmap.affine.e, bitmap.affine.f)

  /** The turn of a buffer of `width` by `height` pixels around its middle. */
  private def rotation(angle: Double, width: Int, height: Int): Affine =
    Affine.rotation(angle, Pos(width / 2.0, height / 2.0))

  /** The eager bitmap, a buffer resampled at every operation as `Bitmap.scala` did it before its
    * operations were recorded.
    */
  private final case class Eager(buffer: NativeBuffer, boundary: Bounds):
    def position: Pos = boundary.center

    def scaleBy(factor: Double): Eager =
      scaleTo(factor * boundary.width, factor * boundary.height, position)

    def scaleTo(targetWidth: Double, targetHeight: Double, relativityPoint: Pos): Eager =
      val newCenter = boundary.center.scaleBy(
        targetWidth / boundary.width,
        targetHeight / boundary.height,
        relativityPoint
      )
      val mirrored = (targetWidth < 0, targetHeight < 0) match
        case (false, false) => this
        case (true, false)  => flipHorizontally
        case (false, true)  => flipVertically
        case (true, true)   => flipDiagonally
      Eager(
        mirrored.buffer.scaleTo(targetWidth.abs, targetHeight.abs, settings),
        Bounds(newCenter, targetWidth.abs, targetHeight.abs)
      )

    def rotateAround(angle: Double, centerOfRotation: Pos): Eager =
      val newBuffer     = buffer.transform(rotation(angle, buffer.width, buffer.height), settings)
      val logicalWidth  = newBuffer.width * boundary.width / buffer.width
      val logicalHeight = newBuffer.height * boundary.height / buffer.height
      Eager(
        newBuffer,
        Bounds(boundary.center.rotateAround(angle, centerOfRotation), logicalWidth, logicalHeight)
      )

    def flipHorizontally: Eager =
      copy(buffer = buffer.transform(Affine(-1, 0, 0, 1, -buffer.width.toDouble, 0)))

    def flipVertically: Eager =
      copy(buffer = buffer.transform(Affine(1, 0, 0, -1, 0, -buffer.height.toDouble)))

    def flipDiagonally: Eager =
      copy(buffer =
        buffer.transform(
          Affine(-1, 0, 0, -1, -buffer.width.toDouble, -buffer.height.toDouble)
        )
      )

    def crop(left: Double, top: Double, right: Double, bottom: Double): Eager =
      val originX = boundary.upperLeftCorner.x
      val originY = boundary.upperLeftCorner.y
      val xs      = Seq((left - originX).floor, (right - originX).floor)
      val ys      = Seq((top - originY).floor, (bottom - originY).floor)
      Eager(
        buffer.copyPortionXYXY(xs.min, ys.min, xs.max, ys.max),
        Bounds(Pos(xs.min + originX, ys.min + originY), Pos(xs.max + originX, ys.max + originY))
      )
  end Eager

  private def eagerSource: Eager =
    val bitmap = source
    Eager(bitmap.nativeBuffer(), boundaryOf(bitmap))

  private def assertReplays(label: String, recorded: Bitmap, eager: Eager): Unit =
    val replayed = recorded.pixels(settings)
    assertEquals(
      (replayed.width, replayed.height),
      (eager.buffer.width, eager.buffer.height),
      s"$label: size"
    )
    assertEquals(replayed.argbData.toSeq, eager.buffer.argbData.toSeq, s"$label: pixels")
    assertBoxesClose(boundaryOf(recorded), eager.boundary, s"$label: boundary")

  private def upperLeftOf(bounds: Bounds): Pos = bounds.upperLeftCorner

  /** `obtained` is `expected` up to the rounding of a turned bitmap's arithmetic. */
  private def assertBoxesClose(obtained: Bounds, expected: Bounds, clue: String): Unit =
    for (a, b) <- Seq(
        (obtained.upperLeftCorner.x, expected.upperLeftCorner.x),
        (obtained.upperLeftCorner.y, expected.upperLeftCorner.y),
        (obtained.lowerRightCorner.x, expected.lowerRightCorner.x),
        (obtained.lowerRightCorner.y, expected.lowerRightCorner.y)
      )
    do assertEqualsDouble(a, b, 1e-9, s"$clue: $obtained, expected $expected")

  test("each recorded operation replays to the pixels the eager bitmap had") {
    assertReplays("scaleBy(2.5)", asBitmap(source.scaleBy(2.5, 2.5)), eagerSource.scaleBy(2.5))
    assertReplays(
      "scaleTo(-7, 3)",
      asBitmap(source.withAnchor(pivot).scaleTo(-7, 3)),
      eagerSource.scaleTo(-7, 3, pivot)
    )
    assertReplays(
      "rotateAround(30)",
      source.rotateAround(30, pivot),
      eagerSource.rotateAround(30, pivot)
    )
    assertReplays("flipHorizontally", source.flipHorizontally, eagerSource.flipHorizontally)
    assertReplays("flipVertically", source.flipVertically, eagerSource.flipVertically)
    assertReplays("flipDiagonally", source.flipDiagonally, eagerSource.flipDiagonally)
    assertReplays(
      "crop",
      asBitmap(source.crop(Bounds(1, 0, 6, 4))),
      eagerSource.crop(1, 0, 6, 4)
    )
  }

  test("a flip replays with the default settings, whatever the output's transform method") {
    // The eager flips always drew with the default settings. Bicubic is the method furthest from
    // the default, and on a canvas it asks for a smoothing quality of its own.
    val bicubic = settings.copy(bufferTransformMethod = smile.TransformMethod.Bicubic)
    def assertFlipReplays(label: String, recorded: Bitmap, eager: Eager): Unit =
      val replayed = recorded.pixels(bicubic)
      assertEquals(
        (replayed.width, replayed.height),
        (eager.buffer.width, eager.buffer.height),
        s"$label: size"
      )
      assertEquals(replayed.argbData.toSeq, eager.buffer.argbData.toSeq, s"$label: pixels")
      assertBoxesClose(boundaryOf(recorded), eager.boundary, s"$label: boundary")
    assertFlipReplays("flipHorizontally", source.flipHorizontally, eagerSource.flipHorizontally)
    assertFlipReplays("flipVertically", source.flipVertically, eagerSource.flipVertically)
    assertFlipReplays("flipDiagonally", source.flipDiagonally, eagerSource.flipDiagonally)
  }

  test("a chain of operations replays to the pixels of the eager chain") {
    val eagerTurned = eagerSource.scaleBy(2.5).rotateAround(30, pivot)
    val corner      = upperLeftOf(eagerTurned.boundary)
    // Whole pixels well inside the turned image, which its own crop keeps as pixels.
    val (l, t, r, b) = (corner.x + 9, corner.y + 8, corner.x + 15, corner.y + 14)
    assertReplays(
      "scaleBy(2.5).rotateAround(30).crop",
      asBitmap(asBitmap(source.scaleBy(2.5, 2.5)).rotateAround(30, pivot).crop(Bounds(l, t, r, b))),
      eagerTurned.crop(l, t, r, b)
    )
    assertReplays(
      "flipHorizontally.scaleBy(0.5, 0.5)",
      asBitmap(source.flipHorizontally.scaleBy(0.5, 0.5)),
      eagerSource.flipHorizontally.scaleBy(0.5)
    )
  }

  test("a picture's bitmap replays on the picture's pixels rendered with the output's settings") {
    val picture = Picture(source)
    val bitmap  = Bitmap.of(picture)
    val content = Renderer().createBitmapFrom(picture, 1.0, settings)
    assertEquals(boundaryOf(bitmap), boundaryOf(content))
    assertEquals(bitmap.pixels(settings).argbData.toSeq, content.pixels(settings).argbData.toSeq)

    val eager =
      Eager(content.nativeBuffer(settings), boundaryOf(content))
        .scaleBy(2.5)
        .rotateAround(30, pivot)
    // Scaled around the middle of its pixels, as the eager bitmap scaled around its center.
    val scaled = asBitmap(bitmap.withAnchor(boundaryOf(bitmap).center).scaleBy(2.5, 2.5))
    assertReplays("Bitmap.of(picture).scaleBy.rotateAround", scaled.rotateAround(30, pivot), eager)
  }

  test("a rotated bitmap's size is known before its pixels, and is the turned buffer's") {
    for
      (width, height) <- Seq((8, 6), (1, 1), (3, 17), (20, 15), (101, 37))
      angle           <- Seq(0.0, 30.0, 45.0, 90.0, -37.0, 137.5, 180.0, 89.999999, 1e-9)
    do
      val buffer = BufferAdapter(width, height)
      val turn   = rotation(angle, width, height)
      val turned = buffer.transform(turn)
      assertEquals(
        ImageOperations.transformedSize(width, height, turn),
        (turned.width, turned.height),
        s"${width}x$height by $angle"
      )
  }

  test("pixels are computed once per settings, and the last two settings are kept") {
    val bitmap  = asBitmap(source.scaleBy(2.5, 2.5))
    val smooth  = settings
    val nearest = settings.copy(bufferScalingMethod = ScalingMethod.NearestNeighbor)
    val fast    = settings.copy(bufferScalingMethod = ScalingMethod.Fast)

    val first = bitmap.nativeBuffer(smooth)
    assert(bitmap.nativeBuffer(smooth) eq first)
    val second = bitmap.nativeBuffer(nearest)
    assert(bitmap.nativeBuffer(smooth) eq first)
    assert(bitmap.nativeBuffer(nearest) eq second)
    val _ = bitmap.nativeBuffer(fast)
    assert(bitmap.nativeBuffer(nearest) eq second, "the second newest stays")
    assert(!(bitmap.nativeBuffer(smooth) eq first), "the oldest is dropped")
  }

  test("a moved bitmap shares the pixels already computed") {
    val bitmap = asBitmap(source.scaleBy(2.5, 2.5))
    val pixels = bitmap.nativeBuffer(settings)
    assert(bitmap.moveBy(3, 4).nativeBuffer(settings) eq pixels)
    assert(bitmap.moveTo(Pos(-10, 2)).nativeBuffer(settings) eq pixels)
  }

  test("the output's settings reach the resampling") {
    val opaque  = Bitmap.fromARGB(2, 1, Array(0xffff0000, 0xff0000ff))
    val bitmap  = asBitmap(opaque.scaleBy(3, 3))
    val nearest = bitmap.pixels(settings.copy(bufferScalingMethod = ScalingMethod.NearestNeighbor))
    val colors  = opaque.pixels().argbData.toSet
    assert(nearest.argbData.forall(colors.contains), "nearest neighbor invents no colors")
    val smooth = bitmap.pixels(settings.copy(bufferScalingMethod = ScalingMethod.Smooth))
    assert(!smooth.argbData.forall(colors.contains), "smooth scaling blends")
  }

  test("operations are validated when they are called, not when the pixels are") {
    val _ = intercept[IllegalArgumentException](source.scaleBy(Double.NaN, 2))
    val _ = intercept[IllegalArgumentException](source.rotateBy(Double.PositiveInfinity))
    val _ = intercept[IllegalArgumentException](source.crop(Bounds(0, 0, Double.NaN, 4)))
  }

  test(
    "a crop that reaches past a bitmap's pixels is the generic crop, with the requested bounds"
  ) {
    for (bitmap, rect) <- Seq(
        source            -> Bounds(-1, 0, 4, 4),
        Bitmap.of(source) -> Bounds(0, 0, 9, 4)
      )
    do
      val cropped = bitmap.crop(rect)
      assert(!cropped.isInstanceOf[Bitmap], s"$rect of $bitmap is $cropped")
      assertEquals(cropped.bounds, Some(rect))
      assertEquals(cropped.anchor, rect.center)
  }

  test("a stepwise rendering is at one buffer pixel per logical pixel") {
    val _ = intercept[IllegalArgumentException](Renderer().createBitmapFrom(source, 2.0, settings))
  }

  test("the color and merge operations compute the pixels with the settings they are given") {
    val scaled = asBitmap(asBitmap(source.scaleBy(1.7, 1.3)).rotateAround(20, pivot))
    val direct = RenderSettings.Default
    // Made opaque, because a canvas does not read translucent pixels back exactly.
    val opaque                            = (argb: Int) => argb | 0xff000000
    val opaqueColor                       = (color: Color) => new Color(opaque(color.toARGBInt))
    def argb(bitmap: Bitmap): Vector[Int] = bitmap.pixels().argbData.toVector
    for chosen <- Seq(settings, direct) do
      val expected = scaled.pixels(chosen).argbData.toVector.map(opaque)
      assertEquals(argb(scaled.transformARGB(opaque, chosen)), expected)
      assertEquals(
        argb(scaled.mergeARGBWith(scaled, (these, _) => opaque(these), chosen)),
        expected
      )
      assertEquals(argb(scaled.transformColorToColor(opaqueColor, chosen)), expected)
      assertEquals(
        argb(scaled.mergeWith(scaled, (these, _) => opaqueColor(these), chosen)),
        expected
      )
    assertNotEquals(
      scaled.pixels(settings).argbData.toVector,
      scaled.pixels(direct).argbData.toVector
    )
  }

  test("operations compute nothing until the pixels are asked for") {
    val circle  = Circle(Pos(10, 10), 8, fillWith(Color(10, 120, 200)), None)
    val start   = Bitmap.of(circle)
    val counted = start.source.renderedSourcePixelCount
    val chained = (1 to 30).foldLeft(start: Element): (element, tick) =>
      if tick % 2 == 0 then element.scaleBy(1.05) else element.flipHorizontally
    val bitmap = asBitmap(chained)
    assertEquals(bitmap.source.steps.length, 30)
    assertEquals(bitmap.source.stepsLastReplayed, 0)
    assertEquals(bitmap.source.renderedSourcePixelCount, counted)
  }
  test("a bitmap scaled on every tick replays one operation per tick") {
    val start = Bitmap.generateARGB(
      40,
      30,
      (x, y) => 0xff000000 | ((x * 6) << 16) | ((y * 8) << 8) | ((x * y) % 256)
    )
    val factors = Vector.tabulate(32)(tick => if tick % 2 == 0 then 1.07 else 0.95)
    val ticked  = factors.foldLeft(start): (bitmap, factor) =>
      val next = asBitmap(bitmap.scaleBy(factor, factor))
      val _    = next.pixels(settings)
      assertEquals(next.source.stepsLastReplayed, 1)
      next
    assertEquals(ticked.source.steps.length, 32, "each keeps the steps before it up to the cap")
    val unticked =
      factors.foldLeft(start)((bitmap, factor) => asBitmap(bitmap.scaleBy(factor, factor)))
    val replayed = unticked.pixels(settings)
    assertEquals(unticked.source.stepsLastReplayed, 32, "a bitmap made before any read")
    assertEquals(ticked.pixels(settings).argbData.toVector, replayed.argbData.toVector)
  }

  test("a bitmap scaled and read on every tick keeps at most 32 steps and replays one") {
    var bitmap = source
    var eager  = eagerSource
    for tick <- 1 to 200 do
      val factor = if tick % 2 == 0 then 1.07 else 0.95
      val before = bitmap.pixels(settings)
      val next   = asBitmap(bitmap.scaleBy(factor, factor))
      eager = eager.scaleBy(factor)
      assertReplays(s"tick $tick", next, eager)
      assert(next.source.steps.length <= 32, s"tick $tick: ${next.source.steps.length} steps")
      assertEquals(next.source.stepsLastReplayed, 1, s"tick $tick")
      if tick > 1 then
        assert(
          next.source.checkpoint.exists(_.pixels.argbData.sameElements(before.argbData)),
          s"tick $tick starts from the pixels of the tick before"
        )
      bitmap = next
  }

  test("40 operations with no read after a checkpoint keep at most 32 steps and replay exactly") {
    // Past 32 steps after a checkpoint, the cap takes a new one for the checkpoint's settings.
    val read   = asBitmap(source.scaleBy(1.1, 1.1))
    val _      = read.pixels(settings)
    val scaled = (1 to 40).foldLeft((read, eagerSource.scaleBy(1.1))): (pair, tick) =>
      val factor          = if tick % 2 == 0 then 1.05 else 0.96
      val (bitmap, eager) = pair
      (asBitmap(bitmap.scaleBy(factor, factor)), eager.scaleBy(factor))
    assert(scaled._1.source.steps.length <= 32, s"${scaled._1.source.steps.length} steps")
    assertReplays("scalings", scaled._1, scaled._2)
  }

  test("a read with one settings does not change a later read with another") {
    val smooth  = settings.copy(bufferScalingMethod = ScalingMethod.Smooth)
    val nearest = settings.copy(bufferScalingMethod = ScalingMethod.NearestNeighbor)
    val read    = asBitmap(source.scaleBy(0.5, 0.5))
    val _       = read.pixels(smooth)
    val after   = asBitmap(read.scaleBy(2, 2)).pixels(nearest)
    val unread  = asBitmap(asBitmap(source.scaleBy(0.5, 0.5)).scaleBy(2, 2)).pixels(nearest)
    assertEquals(after.argbData.toSeq, unread.argbData.toSeq)
  }

  test("40 unread scalings replay exactly on their first Stepwise read") {
    // A bitmap no output has read keeps its steps, so the first reader's settings replay them all.
    val (scaled, eager) = (1 to 40).foldLeft((source, eagerSource)): (pair, tick) =>
      val factor           = if tick % 2 == 0 then 1.06 else 0.95
      val (bitmap, oracle) = pair
      (asBitmap(bitmap.scaleBy(factor, factor)), oracle.scaleBy(factor))
    assertEquals(scaled.source.steps.length, 40)
    assert(scaled.source.complete, "it keeps every step since its source")
    assertReplays("40 scalings", scaled, eager)
  }

  test("a bitmap flipped and read with the default settings on every tick replays at most 32") {
    var bitmap = source
    for tick <- 1 to 2000 do
      bitmap = bitmap.flipHorizontally
      val _ = bitmap.pixels()
      assert(
        bitmap.source.stepsLastReplayed <= 32,
        s"tick $tick: ${bitmap.source.stepsLastReplayed}"
      )
    // Each flip draws the pixels, which moves the half transparent ones a little on a canvas.
    // The tolerance is sized for alpha 128, the only translucent alpha that the source bitmap has.
    val flipped = bitmap.pixels().argbData
    for (expected, index) <- source.pixels().argbData.zipWithIndex do
      val tolerance = if expected >>> 24 == 255 then 0 else Rasterizer.halfTransparentRounding
      val channels  = Seq(24, 16, 8, 0).map(shift => (flipped(index) >>> shift) & 255)
      val wanted    = Seq(24, 16, 8, 0).map(shift => (expected >>> shift) & 255)
      assert(
        channels.zip(wanted).forall((a, b) => (a - b).abs <= tolerance),
        f"pixel $index: ${flipped(index)}%08x, expected $expected%08x"
      )
  }
end StepwiseReplaySuite
