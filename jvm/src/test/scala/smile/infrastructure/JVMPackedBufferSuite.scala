package smile.infrastructure

import java.awt.image.BufferedImage
import smile.colors.Color

class JVMPackedBufferSuite extends munit.FunSuite:
  private val pixels = Array(0xff123456, 0x80123456, 0x00123456, 0xffffffff)

  test("packed writes match setRGB for loaded image formats and premultiplication") {
    for format <- Seq(
        BufferedImage.TYPE_INT_ARGB,
        BufferedImage.TYPE_INT_ARGB_PRE,
        BufferedImage.TYPE_INT_RGB,
        BufferedImage.TYPE_3BYTE_BGR,
        BufferedImage.TYPE_BYTE_GRAY
      )
    do
      for coerce <- Seq(false, true) do
        val image     = new BufferedImage(2, 2, format)
        val reference = new BufferedImage(2, 2, format)
        if coerce then
          image.coerceData(true)
          reference.coerceData(true)
        val adapter = JVMBufferAdapter(image)
        adapter.setARGBData(pixels)
        reference.setRGB(0, 0, 2, 2, pixels, 0, 2)
        assertEquals(adapter.argbData.toSeq, reference.getRGB(0, 0, 2, 2, null, 0, 2).toSeq)
        assertEquals(adapter.argbData(1, 2).toSeq, reference.getRGB(0, 0, 1, 2, null, 0, 1).toSeq)
  }

  test("packed writes and region reads honor translated subimage storage") {
    val parent    = new BufferedImage(6, 5, BufferedImage.TYPE_INT_ARGB)
    val reference = new BufferedImage(6, 5, BufferedImage.TYPE_INT_ARGB)
    val adapter   = JVMBufferAdapter(parent.getSubimage(2, 1, 2, 2))
    adapter.setARGBData(pixels)
    reference.setRGB(2, 1, 2, 2, pixels, 0, 2)
    assertEquals(
      parent.getRGB(0, 0, 6, 5, null, 0, 6).toSeq,
      reference.getRGB(0, 0, 6, 5, null, 0, 6).toSeq
    )
    assertEquals(adapter.argbData(1, 2).toSeq, Seq(pixels(0), pixels(2)))
  }

  test("sequence writes clear null entries and preserve translucent RGB") {
    val adapter = JVMBufferAdapter(2, 1)
    adapter.setARGBData(Array(0xffffffff, 0xffffffff))
    adapter.setColorsFromSeq(Seq(Color(12, 34, 56, 78), null))
    assertEquals(adapter.argbData.toSeq, Seq(0x4e0c2238, 0))
  }

  test("invalid pixel or sequence lengths fail before modifying the image") {
    val adapter = JVMBufferAdapter(2, 2)
    adapter.setARGBData(pixels)
    for length <- Seq(3, 5) do
      val _ = intercept[IllegalArgumentException] { adapter.setARGBData(Array.fill(length)(0)) }
      val _ = intercept[IllegalArgumentException] {
        adapter.setColorsFromSeq(Seq.fill(length)(Color(0, 0, 0)))
      }
      assertEquals(adapter.argbData.toSeq, pixels.toSeq)
  }
