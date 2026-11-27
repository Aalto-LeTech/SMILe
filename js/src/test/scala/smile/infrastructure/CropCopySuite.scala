package smile.infrastructure

/** A crop of a canvas buffer copies its stored pixels exactly, translucent ones included. Reading
  * the pixels out and writing them back would lose some of a translucent pixel's color at every
  * crop, so that a picture cropped again and again would fade.
  */
class CropCopySuite extends munit.FunSuite:

  test("cropping keeps translucent pixels exactly, crop after crop") {
    val (width, height) = (12, 9)
    val buffer          = JSBufferAdapter(width, height)
    buffer.setARGBData(Array.tabulate(width * height): index =>
      val (x, y) = (index % width, index / width)
      val alpha  = Seq(1, 3, 60, 128, 200, 255)((x + y) % 6)
      (alpha << 24) | ((x * 23 + 7) << 16) | ((y * 29 + 11) << 8) | ((x * y * 5) % 256))
    val stored = buffer.argbData

    def slice(left: Int, top: Int, right: Int, bottom: Int): Vector[Int] =
      (top until bottom).toVector.flatMap(y => stored.slice(y * width + left, y * width + right))

    val cropped = buffer.copyPortionXYWH(2, 1, 9, 7)
    assertEquals(cropped.argbData.toVector, slice(2, 1, 11, 8))
    // Each crop of a crop copies the pixels again, and none of them change.
    val again = (1 to 5).foldLeft(cropped)((previous, _) =>
      previous.copyPortionXYWH(0, 0, previous.width, previous.height)
    )
    assertEquals(again.argbData.toVector, slice(2, 1, 11, 8))
    val inner = cropped.copyPortionXYWH(3, 2, 4, 3)
    assertEquals(inner.argbData.toVector, slice(5, 3, 9, 6))
  }
