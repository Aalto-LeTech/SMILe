package smile.pictures

import smile.colors.Color

import java.awt.image.BufferedImage

class JVMBitmapConversionsSuite extends munit.FunSuite:

  private val colors =
    Seq(Color(255, 0, 0), Color(0, 255, 0, 128), Color(0, 0, 255), Color(10, 20, 30, 0))
  private val bitmap = Bitmap(2, 2, colors)

  test("toAWTImage holds the bitmap's pixels") {
    val image = bitmap.toAWTImage()

    assertEquals(image.getType, BufferedImage.TYPE_INT_ARGB)
    assertEquals((image.getWidth, image.getHeight), (2, 2))
    assertEquals(image.getRGB(0, 0, 2, 2, null, 0, 2).toSeq, bitmap.pixels().argbData.toSeq)
  }

  test("toAWTImage is a copy that drawing into leaves the bitmap unchanged") {
    val before = bitmap.pixels().argbData.toSeq
    bitmap.toAWTImage().setRGB(0, 0, 0xff000000)

    assertEquals(bitmap.pixels().argbData.toSeq, before)
  }

  test("writing into toAWTImage changes neither the bitmap nor another toAWTImage") {
    val before = bitmap.pixels().argbData.toSeq
    val first  = bitmap.toAWTImage()
    first.setRGB(1, 1, 0xff000000)
    first.getRaster.setDataElements(0, 0, Array(0xff00ff00))

    assertEquals(bitmap.pixels().argbData.toSeq, before)
    assertEquals(bitmap.toAWTImage().getRGB(0, 0, 2, 2, null, 0, 2).toSeq, before)
  }

  test("toAWTImage keeps the color of a fully transparent pixel") {
    val image = Bitmap.fromARGB(1, 1, Array(0x00123456)).toAWTImage()

    assertEquals(image.getRGB(0, 0), 0x00123456)
  }

  test("toSwingIcon is the bitmap's size and holds its pixels") {
    val icon = bitmap.toSwingIcon()

    assertEquals((icon.getIconWidth, icon.getIconHeight), (2, 2))
    val image = icon.getImage.asInstanceOf[BufferedImage]
    assertEquals(image.getRGB(0, 0, 2, 2, null, 0, 2).toSeq, bitmap.pixels().argbData.toSeq)
  }
