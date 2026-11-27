package smile.testing

class RasterPngSuite extends munit.FunSuite:

  test("a raster encodes as PNG") {
    val bytes =
      RasterPng.encode(Raster(2, 2, Array(0xffff0000, 0xff00ff00, 0xff0000ff, 0x00000000)))
    assertEquals(bytes.take(8).toSeq.map(_ & 0xff), Seq(137, 80, 78, 71, 13, 10, 26, 10))
  }
