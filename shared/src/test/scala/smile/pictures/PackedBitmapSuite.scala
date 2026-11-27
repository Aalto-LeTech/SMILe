package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos}

class PackedBitmapSuite extends munit.FunSuite:

  private val pixels = Array(
    0xff123456, 0x80123456, 0x00123456, 0x01010203, 0x7ffedcba, 0xffffffff
  )

  test("packed construction agrees with Color construction, including translucent pixels") {
    val packed = Bitmap.fromARGB(3, 2, pixels)
    val colors = Bitmap(3, 2, pixels.toSeq.map(pixel => new Color(pixel)))

    assertEquals(packed.pixels().argbData.toSeq, pixels.toSeq)
    assertEquals(colors.pixels().argbData.toSeq, pixels.toSeq)
    assertEquals(packed.bounds.get, colors.bounds.get)
  }

  test("packed construction rejects both missing and extra pixels") {
    for count <- Seq(5, 7) do
      val error = intercept[IllegalArgumentException] {
        Bitmap.fromARGB(3, 2, Array.fill(count)(0xff123456))
      }
      assert(error.getMessage.contains("width * height"), error.getMessage)
  }

  test("packed factories reject invalid dimensions before native allocation or callbacks") {
    val invalid = Seq((0, 2), (2, 0), (-1, 2), (2, -1), (-1, -1), (65536, 65536))
    for (width, height) <- invalid do
      val _ = intercept[IllegalArgumentException] {
        Bitmap.fromARGB(width, height, Array.emptyIntArray)
      }
      var calls = 0
      val _     = intercept[IllegalArgumentException] {
        Bitmap.generateARGB(
          width,
          height,
          (_, _) =>
            calls += 1; 0xff123456
        )
      }
      assertEquals(calls, 0)
  }

  test("packed construction copies the supplied array and allocates independent buffers") {
    val supplied = Array(0xff123456, 0xffabcdef)
    val first    = Bitmap.fromARGB(2, 1, supplied)
    val second   = Bitmap.fromARGB(2, 1, supplied)

    supplied(0) = 0xffffffff
    first.nativeBuffer().setRGBA(1, 0, Color(255, 0, 0))

    assertEquals(first.getColor(0, 0).toARGBInt, 0xff123456)
    assertEquals(second.pixels().argbData.toSeq, Seq(0xff123456, 0xffabcdef))
    assertEquals(supplied.toSeq, Seq(0xffffffff, 0xffabcdef))
  }

  test("packed factories preserve the requested dimensions and center at the origin") {
    val supplied  = Bitmap.fromARGB(3, 2, pixels)
    val generated = Bitmap.generateARGB(3, 2, (x, y) => pixels(y * 3 + x))

    for bitmap <- Seq(supplied, generated) do
      assertEquals(bitmap.anchor, Pos.Origin)
      assertEquals(bitmap.bounds.get, Bounds(Pos.Origin, 3, 2))
      assertEquals(bitmap.pixels().width, 3)
      assertEquals(bitmap.pixels().height, 2)
  }

  test("packed generation calls the generator once per pixel in row-major order") {
    val visited   = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    val generated = Bitmap.generateARGB(
      3,
      2,
      (x, y) =>
        visited += ((x, y))
        pixels(y * 3 + x)
    )

    assertEquals(visited.toSeq, Seq((0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)))
    assertEquals(
      generated.pixels().argbData.toSeq,
      Bitmap.fromARGB(3, 2, pixels).pixels().argbData.toSeq
    )
    val colors = Bitmap(3, 2, (x, y) => new Color(pixels(y * 3 + x)))
    assertEquals(generated.pixels().argbData.toSeq, colors.pixels().argbData.toSeq)
  }

  test("Color generation keeps callback order and produces independent buffers") {
    val visited   = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    val generated = Bitmap(
      3,
      2,
      (x, y) =>
        visited += ((x, y))
        new Color(pixels(y * 3 + x))
    )
    val second = Bitmap(3, 2, (x, y) => new Color(pixels(y * 3 + x)))

    assertEquals(visited.toSeq, Seq((0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)))
    val expected = Bitmap(3, 2, pixels.toSeq.map(pixel => new Color(pixel)))
    assertEquals(generated.pixels().argbData.toSeq, expected.pixels().argbData.toSeq)
    generated.nativeBuffer().setRGBA(0, 0, Color(255, 0, 0))
    assertEquals(second.pixels().argbData.toSeq, expected.pixels().argbData.toSeq)
  }
