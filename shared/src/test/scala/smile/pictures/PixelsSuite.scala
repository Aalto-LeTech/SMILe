package smile.pictures

import smile.{RenderSettings, ResamplingMode}
import smile.colors.Color
import smile.modeling.{Bounds, Pos}

/** A bitmap's pixels are handed out as a read-only view. Writing into what the view gives changes
  * neither the bitmap nor the bitmaps that share its pixels.
  */
class PixelsSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  private def redBlue = Bitmap(2, 1, Seq(red, blue))

  test("pixels are a copy") {
    val bitmap = redBlue
    val argb   = bitmap.pixels().argbData
    argb(0) = 0

    assertEquals(bitmap.getColor(0, 0), red)
    assertEquals(bitmap.pixels().argbData(0), red.toARGBInt)
  }

  test("shared pixels stay shared and unchanged") {
    val bitmap = redBlue
    // A translation shares the pixels of the bitmap it moves.
    val moved = bitmap.moveBy(3, 4)
    assert(moved.nativeBuffer() eq bitmap.nativeBuffer())
    val argb = moved.pixels().argbData
    argb(1) = 0

    assertEquals(bitmap.getColor(1, 0), blue)
    assertEquals(moved.getColor(1, 0), blue)
  }

  test("the view answers like the bitmap") {
    val pixels = redBlue.pixels()

    assertEquals(pixels.colorAt(1, 0), Some(blue))
    assertEquals(pixels.colorAt(2, 0), None)
    assertEquals(pixels.colorAt(-1, 0), None)
    assertEquals(pixels.colors, Vector(red, blue))
    assertEquals(pixels.width, 2)
    assertEquals(pixels.height, 1)
    assertEquals(pixels.toString, "Pixels(2 x 1)")
  }

  test("a content bitmap's view is computed when asked") {
    val content = Bitmap.of(Rectangle(4, 3, Pos(2, 1.5), fillWith(red), None))
    val pixels  = content.pixels()

    assertEquals((pixels.width, pixels.height), (4, 3))
    assertEquals(pixels.colorAt(0, 0), Some(red))
    assert(content.pixels().argbData.sameElements(pixels.argbData))
  }

  test("asking for the view computes the pixels before the view is read") {
    // A caller may ask for the pixels only to have them computed.
    val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)
    val scaled   = redBlue.scaleBy(2, 2).asInstanceOf[Bitmap]
    assertEquals(scaled.source.stepsLastReplayed, 0)

    val _ = scaled.pixels(stepwise)

    assertEquals(scaled.source.stepsLastReplayed, 1)
  }

  test("translucent pixels read back exactly through every pixel operation") {
    val argb = Array(
      0x80123456, 0x01020304, 0x7f336699, 0xff000000, 0x80ff8000, 0x00123456, 0x7ffedcba, 0x01010203
    )
    val bitmap = Bitmap.fromARGB(4, 2, argb)
    // Stepwise shifts and crops copy pixels without resampling.
    val settings                     = RenderSettings(resamplingMode = ResamplingMode.Stepwise)
    def argbOf(e: Element): Seq[Int] = e.asBitmap.pixels(settings).argbData.toSeq
    def rows(pick: Seq[Int] => Seq[Int]): Seq[Int] = argb.toSeq.grouped(4).flatMap(pick).toSeq

    assertEquals(argbOf(bitmap), argb.toSeq)
    assertEquals(bitmap.colorAt(1, 0), Some(Color(2, 3, 4, 1)))
    assertEquals(argbOf(bitmap.transformARGB(identity, settings)), argb.toSeq)
    assertEquals(argbOf(bitmap.wrapShift(1, 0)), rows(row => row.last +: row.init))
    assertEquals(argbOf(bitmap.mergeARGBWith(bitmap, (a, _) => a, settings)), argb.toSeq)
    // The crop is in world coordinates, where the bitmap's bounds are (-2,-1)-(2,1).
    assertEquals(argbOf(bitmap.crop(Bounds(-2, -1, 1, 1))), rows(_.take(3)))
  }

  test("a whole-pixel crop of translucent pixels reads back exactly with the default settings") {
    val argb    = Array(0x80123456, 0x01020304, 0x7f336699, 0xff000000)
    val cropped = Bitmap.fromARGB(2, 2, argb).crop(Bounds(-1, -1, 0, 1))
    assertEquals(cropped.asBitmap.pixels().argbData.toSeq, Seq(0x80123456, 0x7f336699))
  }
end PixelsSuite
