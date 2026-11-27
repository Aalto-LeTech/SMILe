package smile.testing

import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

class RasterPngSuite extends munit.FunSuite:

  test("a PNG decodes back to the same pixels, translucent ones included") {
    val raster =
      Raster(3, 2, Array(0xffff0000, 0xff00ff00, 0x800000ff, 0x00000000, 0xffffffff, 0x40102030))
    val decoded = ImageIO.read(ByteArrayInputStream(RasterPng.encode(raster)))
    assertEquals((decoded.getWidth, decoded.getHeight), (3, 2))
    val pixels = for y <- 0 until 2; x <- 0 until 3 yield decoded.getRGB(x, y)
    assertEquals(pixels, raster.argb.toSeq)
  }
