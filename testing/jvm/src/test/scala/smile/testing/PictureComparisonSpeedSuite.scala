package smile.testing

class PictureComparisonSpeedSuite extends munit.FunSuite:

  test("a 1000x400 comparison with one region takes under a second") {
    val expected = Raster(1000, 400, Array.fill(400000)(0xffffffff))
    val argb     = expected.argb.clone()
    for y <- 100 until 150; x <- 500 until 560 do argb(y * 1000 + x) = 0xffff0000
    val actual = Raster(1000, 400, argb)
    PictureComparison.compare(actual, expected) // warm up
    // Up to five tries, so that other processes competing for the CPU, such as several test runs
    // at once, cannot fail it. A comparison that is slow in itself is slow in every try.
    def seconds =
      val start = System.nanoTime()
      val diff  = PictureComparison.compare(actual, expected)
      assertEquals(diff.regions.size, 1)
      (System.nanoTime() - start) / 1e9
    val tries = Iterator.continually(seconds).take(5).toList
    assert(tries.exists(_ < 1.0), f"took ${tries.min}%.2f s at best")
  }
