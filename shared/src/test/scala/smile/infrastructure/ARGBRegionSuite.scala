package smile.infrastructure

import smile.RenderSettings
import smile.colors.Color
import smile.modeling.Affine

class ARGBRegionSuite extends munit.FunSuite:

  private def populated(width: Int, height: Int): NativeBuffer =
    val result = BufferAdapter(width, height)
    result.setARGBData(Array.tabulate(width * height)(i => 0xff000000 | (i + 1)))
    result

  test("region reads pack rows using the requested width and own their storage") {
    val buffer = populated(4, 3)
    val region = buffer.argbData(2, 3)
    assertEquals(region.toSeq, Seq(1, 2, 5, 6, 9, 10).map(_ | 0xff000000))
    region(0) = 0
    assertEquals(buffer.argbData(1, 1).toSeq, Seq(0xff000001))
    assertEquals(buffer.argbData(4, 3).toSeq, buffer.argbData.toSeq)
  }

  test("region reads reject invalid dimensions and accept empty rectangles") {
    val buffer = populated(4, 3)
    for (w, h) <- Seq((-1, 2), (2, -1), (5, 2), (2, 4), (Int.MaxValue, 1)) do
      intercept[IllegalArgumentException](buffer.argbData(w, h))
    assertEquals(buffer.argbData(0, 3).length, 0)
    assertEquals(buffer.argbData(4, 0).length, 0)
    assertEquals(buffer.argbData(0, 0).length, 0)
  }

  test("the default region implementation preserves compact rows and independent storage") {
    val pixels = Array.tabulate(12)(identity)
    val buffer = new ArrayBuffer(4, 3, pixels)
    val region = buffer.argbData(2, 3)
    assertEquals(region.toSeq, Seq(0, 1, 4, 5, 8, 9))
    region(0) = -1
    assertEquals(buffer.argbData.toSeq, pixels.toSeq)
    assertEquals(buffer.argbData(0, 3).length, 0)
    intercept[IllegalArgumentException](buffer.argbData(5, 1))
  }

  test("packed writes reject wrong pixel counts before changing the buffer") {
    val buffer = populated(2, 2)
    val before = buffer.argbData.toSeq
    val _      = intercept[IllegalArgumentException](buffer.setARGBData(Array(0)))
    val _      = intercept[IllegalArgumentException](buffer.setARGBData(Array.fill(5)(0)))
    assertEquals(buffer.argbData.toSeq, before)
  }

  test("region reads preserve the source's observed alpha and channel values") {
    val buffer = BufferAdapter(3, 2)
    // Compare readback, because canvas may round channels when storing premultiplied alpha.
    buffer.setARGBData(Array(0x00112233, 0x80112233, 0xff445566, 0x40123456, 0xabcdef12, 0))
    val whole = buffer.argbData
    assertEquals(buffer.argbData(2, 2).toSeq, Seq(whole(0), whole(1), whole(3), whole(4)))
  }

  test("merge uses only compact overlap reads and calls back in source and row-major order") {
    val left   = new RegionOnlyBuffer(4, 3, Array.tabulate(12)(i => 0xff000000 | (i + 1)))
    val right  = new RegionOnlyBuffer(2, 4, Array.tabulate(8)(i => 0xff000000 | (100 + i)))
    val calls  = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    val merged = left.mergeARGBWith(
      right,
      (a, b) =>
        calls += ((a, b))
        a
    )
    assertEquals((merged.width, merged.height), (2, 3))
    assertEquals(left.reads.toSeq, Seq((2, 3)))
    assertEquals(right.reads.toSeq, Seq((2, 3)))
    assertEquals(
      calls.map((a, b) => (a & 255, b & 255)).toSeq,
      Seq((1, 100), (2, 101), (5, 102), (6, 103), (9, 104), (10, 105))
    )
    assertEquals(merged.argbData.toSeq, calls.map(_._1).toSeq)
  }

  test("merge keeps both source buffers unchanged on success and callback failure") {
    val left        = populated(4, 3)
    val right       = populated(2, 4)
    val beforeLeft  = left.argbData.toSeq
    val beforeRight = right.argbData.toSeq
    val _           = left.mergeARGBWith(right, (_, b) => b)
    var count       = 0
    val failure     = new IllegalStateException("callback failed")
    val caught      = intercept[IllegalStateException] {
      left.mergeARGBWith(
        right,
        (a, _) =>
          count += 1
          if count == 3 then throw failure
          a
      )
    }
    assert(caught eq failure)
    assertEquals(count, 3)
    assertEquals(left.argbData.toSeq, beforeLeft)
    assertEquals(right.argbData.toSeq, beforeRight)
  }

  test("merge preserves alpha from observed source pixels") {
    val left  = BufferAdapter(2, 2)
    val right = populated(1, 2)
    left.setARGBData(Array(0x80112233, 0xff445566, 0x40123456, 0))
    val expected = left.argbData(1, 2).toSeq
    val merged   = left.mergeARGBWith(right, (a, _) => a)
    assertEquals(merged.argbData.map(_ >>> 24).toSeq, expected.map(_ >>> 24))
  }

  // Whole-image reads throw, so a test with this buffer fails unless only regions are read. It
  // merges, so it has the image operations. It never scales, crops or transforms.
  private class RegionOnlyBuffer(sourceWidth: Int, sourceHeight: Int, pixels: Array[Int])
      extends ArrayBuffer(sourceWidth, sourceHeight, pixels),
        ImageOperations:
    val reads                         = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    override def argbData: Array[Int] = throw new AssertionError("unexpected whole-image read")
    override def argbData(w: Int, h: Int): Array[Int] =
      requireARGBRegion(w, h)
      reads += ((w, h))
      Array.tabulate(w * h)(i => pixels((i / w) * width + i % w))
    def scaleTo(w: Double, h: Double, settings: RenderSettings): NativeBuffer =
      throw new UnsupportedOperationException
    def copyPortionXYWH(x: Double, y: Double, w: Double, h: Double): NativeBuffer =
      throw new UnsupportedOperationException
    def transform(
        transformation: Affine,
        settings: RenderSettings
    ): NativeBuffer = throw new UnsupportedOperationException

  private class ArrayBuffer(val width: Int, val height: Int, pixels: Array[Int])
      extends BufferAdapter:
    def argbData: Array[Int]                        = pixels.clone()
    def imageData: Seq[Color]                       = throw new UnsupportedOperationException
    def setColorsFromSeq(colors: Seq[Color]): Unit  = throw new UnsupportedOperationException
    def pixelColor(x: Int, y: Int): Color           = throw new UnsupportedOperationException
    def setRGBA(x: Int, y: Int, color: Color): Unit = throw new UnsupportedOperationException
    def setARGBData(data: Array[Int]): Unit         = throw new UnsupportedOperationException
