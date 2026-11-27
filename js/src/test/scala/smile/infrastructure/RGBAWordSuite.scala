package smile.infrastructure

class RGBAWordSuite extends munit.FunSuite:
  test("ARGB words encode RGBA bytes in either native byte order") {
    val buffer = JSBufferAdapter(1, 1)
    for pixel <- Seq(0xff123456, 0x80123456, 0x00123456, 0x01010203, 0x7ffedcba, 0xffffffff) do
      val expected = Seq((pixel >>> 16) & 255, (pixel >>> 8) & 255, pixel & 255, pixel >>> 24)
      val little   = buffer.rgbaWord(pixel, true)
      val big      = buffer.rgbaWord(pixel, false)
      assertEquals(
        Seq(little & 255, (little >>> 8) & 255, (little >>> 16) & 255, little >>> 24),
        expected
      )
      assertEquals(Seq(big >>> 24, (big >>> 16) & 255, (big >>> 8) & 255, big & 255), expected)
  }
