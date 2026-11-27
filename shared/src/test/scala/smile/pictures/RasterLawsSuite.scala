package smile.pictures

import smile.{RenderSettings, ResamplingMode}
import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.pictures.{Bitmap, FillStyle}

/** The laws of bitmaps as elements: bounds from the pixel box under the transform, steps for the
  * pixels, and the bitmap's own crops, flips and wrap shifts pixel for pixel as today's.
  */
class RasterLawsSuite extends munit.FunSuite:

  private val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)

  private def pattern(x: Int, y: Int): Color =
    Color((x * 37) % 256, (y * 53) % 256, ((x + y) * 11) % 256)

  private def sprite(width: Int, height: Int): Bitmap =
    Bitmap(width, height, pattern)

  /** The pixel source a bitmap of the pattern has, whose own operations record the steps that the
    * buffer algorithms replay, as the bitmap before the swap did.
    */
  private def today(width: Int, height: Int): PixelSource = PixelSource(width, height, pattern)

  private def asBitmap(e: Element): Bitmap = e match
    case b: Bitmap => b
    case other     => fail(s"$other is not a bitmap")

  private def boundsOf(e: Element): Bounds = e.bounds.getOrElse(fail(s"$e has no bounds"))

  private def assertNear(actual: Pos, expected: Pos, tolerance: Double = 1e-9): Unit =
    assert(
      (actual.x - expected.x).abs <= tolerance && (actual.y - expected.y).abs <= tolerance,
      s"$actual is not within $tolerance of $expected"
    )

  private def samePixels(actual: Bitmap, expected: PixelSource, settings: RenderSettings) =
    val (mine, theirs) = (actual.pixels(settings), expected.buffer(settings, 0L))
    assertEquals((mine.width, mine.height), (theirs.width, theirs.height))
    assert(mine.argbData.sameElements(theirs.argbData), "the pixels differ from today's")

  test("a bitmap is placed as today's, centered at the origin with its anchor there") {
    val b = sprite(40, 30)
    assertEquals(b.bounds, Some(Bounds(-20, -15, 20, 15)))
    assertEquals(b.anchor, Pos.Origin)
    assertEquals(b.getColor(3, 4), pattern(3, 4))
    samePixels(b, today(40, 30), RenderSettings.Default)
  }

  test("a bitmap turned by 45 degrees is as wide as its turned corners, not a floored buffer") {
    val turned = sprite(50, 50).rotateBy(45)
    assertEqualsDouble(turned.width, 50 * math.sqrt(2), 1e-9)
    assertEqualsDouble(turned.height, 50 * math.sqrt(2), 1e-9)
    // Each composed translation lies on the grid of 2^-20 pixel.
    assertNear(boundsOf(turned).center, Pos.Origin, 1e-6)
    assertEquals(turned.anchor, Pos.Origin)
  }

  test("a bitmap's own turns fold into one step, with the bounds of one turn by the total") {
    val ship           = sprite(40, 30)
    val diagonal       = math.hypot(40, 30)
    var turned: Bitmap = ship
    for turn <- 1 to 2000 do
      turned = turned.rotateBy(3)
      assert(turned.width <= diagonal + 1e-9, s"turn $turn: ${turned.width} > $diagonal")
      assert(turned.source.steps.length <= 1, s"turn $turn: ${turned.source.steps}")
    // 2000 turns by 3 degrees are 6000 degrees, 240 past whole turns. Every turn rounds the
    // translation it composes to the grid of 2^-20 pixel, so 2000 of them may drift by 2000 steps.
    val once  = ship.rotateBy(240)
    val drift = 2000.0 / 1048576
    assertNear(boundsOf(turned).upperLeftCorner, boundsOf(once).upperLeftCorner, drift)
    assertNear(boundsOf(turned).lowerRightCorner, boundsOf(once).lowerRightCorner, drift)
    assertEquals(turned.source.steps, once.source.steps)
    assertNear(turned.anchor, ship.anchor)
  }

  test("Stepwise pixels replay the bitmap's own steps as today's bitmap does") {
    val b      = sprite(30, 20)
    val center = Pos.Origin
    samePixels(b.rotateBy(30), today(30, 20).rotateAround(30, center), stepwise)
    samePixels(b.rotateBy(10).rotateBy(20), today(30, 20).rotateAround(30, center), stepwise)
    samePixels(asBitmap(b.scaleBy(2)), today(30, 20).scaleBy(2, 2, center), stepwise)
  }

  test("a bitmap's own crop keeps today's pixels and is anchored in the middle of the crop") {
    val b       = sprite(40, 30).moveBy(100, 50)
    val cropped = asBitmap(b.crop(Bounds(85, 37, 105, 57)))
    assertEquals(cropped.bounds, Some(Bounds(85, 37, 105, 57)))
    assertEquals(cropped.anchor, Pos(95, 47))
    samePixels(cropped, today(40, 30).crop(-15, -13, 5, 7), RenderSettings.Default)
    // A crop of a crop is one crop.
    val again = asBitmap(cropped.crop(Bounds(90, 40, 100, 45)))
    assertEquals(again.source.steps.length, 1)
    samePixels(again, today(40, 30).crop(-15, -13, 5, 7).crop(-10, -10, 0, -5), stepwise)
  }

  test("a crop of a bitmap turned by its own steps crops its turned pixels") {
    val turned  = sprite(30, 20).rotateBy(90)
    val box     = boundsOf(turned)
    val corner  = box.upperLeftCorner
    val cropped =
      asBitmap(turned.crop(Bounds(corner.x + 2, corner.y + 3, corner.x + 12, corner.y + 13)))
    assertEquals(cropped.width, 10.0)
    assertEquals(cropped.height, 10.0)
    val expected = today(30, 20).rotateAround(90, Pos.Origin)
    val start    = expected.boundary.upperLeftCorner
    samePixels(
      cropped,
      expected.crop(start.x + 2, start.y + 3, start.x + 12, start.y + 13),
      RenderSettings.Default
    )
  }

  test("a bitmap's own crop off its whole pixels is the generic crop of the requested size") {
    val b = sprite(40, 30).moveBy(100, 50)
    for rect <- Seq(Bounds(90, 40, 90.5, 50), Bounds(90, 40, 90.3, 40.3), Bounds(85.5, 40, 95, 50))
    do
      val cropped = b.crop(rect)
      assertEquals(cropped, (b: Element).crop(rect))
      assertEquals(cropped.bounds, Some(rect))
      assertNear(cropped.anchor, rect.center, 1.0 / 1048576)
    // With no width at all, it is blank space of the requested size.
    b.crop(Bounds(90, 40, 90, 50)) match
      case clip: Clip => assertEquals(clip.child, Empty)
      case other      => fail(s"$other is not blank space")
  }

  test("a bitmap's own crop has exactly the generic crop's bounds and anchor") {
    // A 10 x 10 bitmap centered at the origin, cropped past its pixels and off them.
    val b = sprite(10, 10)
    for rect <- Seq(Bounds(-20, -20, 0, 0), Bounds(0.5, 0.5, 8.5, 8.5), Bounds(-3, -2, 4, 3)) do
      for cropped <- Seq(b.crop(rect), (b: Element).crop(rect)) do
        assertEquals(cropped.bounds, Some(rect), s"$rect")
        assertNear(cropped.anchor, rect.center, 1.0 / 1048576)
    assertEquals(b.crop(Bounds(-20, -20, 0, 0)).width, 20.0)
    assertEquals(b.crop(Bounds(-20, -20, 0, 0)).anchor, Pos(-10, -10))
    // Only on whole pixels inside the bitmap is the crop a bitmap of the cropped pixels.
    assert(b.crop(Bounds(-3, -2, 4, 3)).isInstanceOf[Bitmap])
    assert(b.crop(Bounds(0.5, 0.5, 8.5, 8.5)).isInstanceOf[Clip])
    assert(b.crop(Bounds(-20, -20, 0, 0)).isInstanceOf[Clip])
  }

  test("a bitmap's own crop on whole pixels is exactly the requested rectangle, not nearly") {
    // Course tests compare widths exactly, so float error in the pixel box would fail them.
    val b     = sprite(300, 300)
    val rnd   = new scala.util.Random(7)
    val rects = Bounds(-150, -150, -96, 150) +: Seq.fill(200) {
      val (x1, x2) = (rnd.between(-150, 149), rnd.between(-150, 149))
      val (y1, y2) = (rnd.between(-150, 149), rnd.between(-150, 149))
      Bounds(x1 min x2, y1 min y2, (x1 max x2) + 1, (y1 max y2) + 1)
    }
    for rect <- rects do
      val cropped = b.crop(rect)
      assert(cropped.isInstanceOf[Bitmap], s"$rect")
      assertEquals(cropped.bounds, Some(rect), s"$rect")
      assertEquals(cropped.width, rect.width, s"$rect")
  }

  test("a crop of a turned bitmap to its own bounds keeps those bounds") {
    val turned  = sprite(50, 50).rotateBy(45)
    val cropped = turned.crop(boundsOf(turned))
    assertEquals(cropped.bounds, Some(boundsOf(turned)))
    assertEquals(cropped, (turned: Element).crop(boundsOf(turned)))
  }

  test("a bitmap's own wrap shift agrees with the generic one") {
    val b = sprite(12, 8).moveBy(3, 2)
    for (dx, dy) <- Seq((5, 3), (-2, 0), (12, 8)) do
      val own = b.wrapShift(dx, dy)
      assert(own.isInstanceOf[Bitmap], s"$own")
      val generic = (b: Element).wrapShift(dx.toDouble, dy.toDouble)
      assertEquals(own.bounds, generic.bounds)
      assertEquals(own.anchor, generic.anchor)
      assertEquals(
        own.toBitmap().pixels().argbData.toSeq,
        generic.toBitmap().pixels().argbData.toSeq
      )
    // A scaled bitmap, or one off whole pixels, has no whole pixels to shift by, so its shift is
    // the generic one.
    val scaled = asBitmap(b.scaleBy(1.5))
    assertEquals(scaled.wrapShift(2, 1), (scaled: Element).wrapShift(2, 1))
    val halfway = b.moveBy(0.5, 0)
    assertEquals(halfway.wrapShift(2, 1), (halfway: Element).wrapShift(2, 1))
  }

  test("a bitmap's own turns that add up to none give back the bitmap") {
    val b = sprite(30, 20)
    assertEquals(b.rotateBy(90).rotateBy(-90), b)
    assertEquals(b.rotateBy(90).rotateBy(90).rotateBy(180), b)
    // A turn by an angle that is not right, and back, is exact too.
    for
      bitmap <- Seq(
        b,
        sprite(20, 20),
        sprite(7, 5).moveBy(10.5, 3),
        sprite(20, 20).moveBy(0.3, 7.1)
      )
      angle <- Seq(30.0, 1.0, 45.0, 137.0)
    do assertEquals(bitmap.rotateBy(angle).rotateBy(-angle), bitmap, s"$bitmap by $angle")
    var turned = b
    for _ <- 1 to 3600 do turned = turned.rotateBy(0.1)
    assertEquals(turned.source.steps, Vector())
    var quarter = b
    for _ <- 1 to 900 do quarter = quarter.rotateBy(0.1)
    quarter.source.steps match
      case Vector(smile.pictures.BitmapStep.Rotate(angle)) => assertEquals(angle, 90.0)
      case other                                           => fail(s"$other is not one right angle")
  }

  test("pixels of a bitmap turned by a transform no step expresses are rendered turned") {
    val b = sprite(100, 20).scaleBy(0.01).rotateBy(90).scaleBy(100)
    assertEqualsDouble(b.width, 20, 1e-9)
    assertEqualsDouble(b.height, 100, 1e-9)
    val pixels = asBitmap(b).pixels()
    assertEquals((pixels.width, pixels.height), (20, 100))
    // The source's pixel at column 3 and row 4 lands at column 4 and row 96.
    assertEquals(asBitmap(b).getColor(4, 96), pattern(3, 4))
  }

  test("a bitmap's own flips mirror through its anchor and keep today's pixels") {
    val b = sprite(40, 30)
    samePixels(b.flipHorizontally, today(40, 30).flipHorizontally, RenderSettings.Default)
    samePixels(b.flipVertically, today(40, 30).flipVertically, RenderSettings.Default)
    samePixels(b.flipDiagonally, today(40, 30).flipDiagonally, RenderSettings.Default)
    val off     = b.withAnchor(Pos(-20, -15))
    val flipped = off.flipHorizontally
    assertEquals(flipped.bounds, Some(Bounds(-60, -15, -20, 15)))
    assertEquals(flipped.anchor, Pos(-20, -15))
    assertEquals(flipped.source.steps.length, 1)
  }

  test("a bitmap's own wrap shift keeps today's pixels, its bounds and its anchor") {
    val b       = sprite(40, 30).moveBy(7, 9)
    val shifted = asBitmap(b.wrapShift(5, -3))
    assertEquals(shifted.bounds, b.bounds)
    assertEquals(shifted.anchor, b.anchor)
    samePixels(shifted, today(40, 30).wrapShift(5, -3), RenderSettings.Default)
    samePixels(asBitmap(shifted.wrapShift(2, 1)), today(40, 30).wrapShift(7, -2), stepwise)
    // A fractional shift is the generic one, which wraps the bitmap in a wrap.
    b.wrapShift(1.5, 0.0) match
      case wrap: Wrap => assertEquals(wrap.child, b)
      case other      => fail(s"$other is not a wrap")
  }

  test("transforms carry the anchor and scale the pixel box") {
    val b     = sprite(10, 8)
    val moved = b.moveTo(Pos(100, 100))
    assertEquals(moved.bounds, Some(Bounds(95, 96, 105, 104)))
    val scaled = moved.scaleBy(3, 2)
    assertEquals(scaled.bounds, Some(Bounds(85, 92, 115, 108)))
    assertEquals(scaled.anchor, Pos(100, 100))
    val around = b.rotateAround(90, Pos(5, 4))
    assertEquals(around.bounds, Some(Bounds(-3, 4, 5, 14)))
    assertEquals(around.anchor, Pos(1, 9))
    // A scale to under a pixel still gives a bitmap.
    val tiny = b.scaleBy(0.05)
    assertEqualsDouble(tiny.width, 0.5, 1e-12)
    val _ = asBitmap(tiny)
  }

  test("bitmaps are equal by their source, transform and anchor") {
    val b = sprite(10, 8)
    assertEquals(b.moveBy(3, 4).moveBy(-3, -4), b)
    assertEquals(b.moveBy(3, 4).hashCode, b.moveBy(3, 4).hashCode)
    assertNotEquals(b, sprite(10, 8))
    assertNotEquals(b, b.withAnchor(Pos(1, 1)))
    assertNotEquals[Element, Element](
      b,
      Rectangle(10, 8, Pos(5, 4), Some(FillStyle(Color(0, 0, 0))), None)
    )
  }

  test("a bitmap inside a turned picture keeps its steps and is measured through the turn") {
    val b      = sprite(50, 50)
    val turned = Picture(b).rotateBy(45)
    assertEqualsDouble(turned.width, 50 * math.sqrt(2), 1e-9)
    val inside = turned match
      case p: Picture => p.children.head
      case other      => fail(s"$other is not a picture")
    assertEquals(inside, b)
  }
end RasterLawsSuite
