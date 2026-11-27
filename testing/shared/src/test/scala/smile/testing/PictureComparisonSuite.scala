package smile.testing

import smile.colors.PresetColor.{Red, White}

class PictureComparisonSuite extends munit.FunSuite:

  private val red   = Red.toARGBInt
  private val white = White.toARGBInt

  /** A white raster with red filled into the given half-open pixel rectangles. */
  private def scene(width: Int, height: Int, reds: (Int, Int, Int, Int)*): Raster =
    val argb = Array.fill(width * height)(white)
    for
      (left, top, right, bottom) <- reds
      y                          <- top until bottom
      x                          <- left until right
    do argb(y * width + x) = red
    Raster(width, height, argb)

  test("identical rasters pass with no regions") {
    val picture = scene(40, 40, (10, 10, 20, 20))
    val diff    = PictureComparison.compare(picture, picture)
    assert(diff.passed)
    assertEquals(diff.regions, Seq.empty)
    assertEquals(diff.sizeMismatch, None)
  }

  test("a red square moved 3 px fails with a region on each side, named by color") {
    val expected = scene(40, 40, (10, 10, 20, 20))
    val actual   = scene(40, 40, (13, 10, 23, 20))
    val diff     = PictureComparison.compare(actual, expected)
    assert(!diff.passed)
    assertEquals(diff.regions.size, 2)
    val byLeft = diff.regions.sortBy(_.bounds.upperLeftCorner.x)
    assertEquals(byLeft(0).expected.toARGBInt, red)
    assertEquals(byLeft(0).found.toARGBInt, white)
    assertEquals(byLeft(1).expected.toARGBInt, white)
    assertEquals(byLeft(1).found.toARGBInt, red)
    assert(byLeft(0).bounds.upperLeftCorner.x <= 11, byLeft(0).bounds.toString)
    assert(byLeft(1).bounds.lowerRightCorner.x >= 22, byLeft(1).bounds.toString)
  }

  test("a softened edge pixel passes under Antialiased and fails under Strict") {
    val expected = scene(40, 40, (10, 10, 20, 20))
    val argb     = expected.argb.clone()
    argb(15 * 40 + 20) = 0xffff8080 // the pixel right of the square's edge, half red
    val actual = Raster(40, 40, argb)
    assert(PictureComparison.compare(actual, expected, Tolerance.Antialiased).passed)
    assert(!PictureComparison.compare(actual, expected, Tolerance.Strict).passed)
  }

  test("a loose tolerance forgives flat colors that are a little off") {
    val expected = scene(20, 20, (0, 0, 20, 20))
    val actual   = Raster(20, 20, Array.fill(400)(0xfff80404))
    assert(!PictureComparison.compare(actual, expected).passed)
    assert(PictureComparison.compare(actual, expected, Tolerance.Loose(8, 0.001)).passed)
  }

  test("different sizes fail with both sizes and no regions") {
    val diff = PictureComparison.compare(scene(30, 20), scene(40, 40))
    assert(!diff.passed)
    assertEquals(diff.sizeMismatch, Some(((30, 20), (40, 40))))
    assertEquals(diff.regions, Seq.empty)
  }

  test("transparent pixels with different color channels compare equal") {
    val a = Raster(10, 10, Array.fill(100)(0x00ff0000))
    val b = Raster(10, 10, Array.fill(100)(0x0000ff00))
    assert(PictureComparison.compare(a, b).passed)
  }

  test("two separate mismatches give two regions, the larger first") {
    val expected = scene(60, 40)
    val actual   = scene(60, 40, (5, 5, 10, 10), (30, 10, 50, 30))
    val regions  = PictureComparison.compare(actual, expected).regions
    assertEquals(regions.size, 2)
    assert(regions(0).pixels > regions(1).pixels)
    assertEquals(regions(0).bounds.upperLeftCorner.x.toInt, 30)
  }

  /** A white raster with a red disc of the given radius whose center is at (cx, cy), drawn without
    * antialiasing. A pixel is red when its center is inside the disc.
    */
  private def disc(cx: Double, cy: Double, radius: Double): Raster =
    val argb = Array.tabulate(60 * 40): index =>
      val (x, y) = (index % 60 + 0.5, index / 60 + 0.5)
      if math.hypot(x - cx, y - cy) <= radius then red else white
    Raster(60, 40, argb)

  test("a shape one pixel off fails under Antialiased, naming its color and the shift") {
    val diff = PictureComparison.compare(disc(30, 19, 7.5), disc(30, 20, 7.5))
    assert(!diff.passed)
    val red = diff.colorMismatches.find(_.color.toARGBInt == this.red).get
    assert((red.centroidShift._2 + 1).abs < 0.1, red.toString)
  }

  test("a shape one pixel too small fails under Antialiased, naming how many pixels it covers") {
    val diff = PictureComparison.compare(disc(30, 20, 6.5), disc(30, 20, 7.5))
    assert(!diff.passed)
    val red = diff.colorMismatches.find(_.color.toARGBInt == this.red).get
    assert(red.foundPixels < red.expectedPixels, red.toString)
  }
