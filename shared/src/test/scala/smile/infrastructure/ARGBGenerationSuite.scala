package smile.infrastructure

import smile.colors.Color

class ARGBGenerationSuite extends munit.FunSuite:
  private val pixels = Array(0xff123456, 0x80123456, 0x00123456, 0x01010203, 0x7ffedcba, 0xffffffff)

  test("native generation preserves row-major calls and packed upload pixel behavior") {
    val output   = BufferAdapter(3, 2)
    val expected = BufferAdapter(3, 2)
    expected.setARGBData(pixels)
    val visited = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    output.setARGBByLocation { (x, y) =>
      visited += ((x, y))
      pixels(y * 3 + x)
    }
    assertEquals(visited.toSeq, Seq((0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)))
    assertEquals(output.argbData.toSeq, expected.argbData.toSeq)
  }

  test("generator exceptions leave existing destination pixels intact") {
    val output = BufferAdapter(3, 2)
    output.setARGBData(pixels)
    val before = output.argbData.toSeq
    var calls  = 0
    val _      = intercept[IllegalStateException] {
      output.setARGBByLocation { (_, _) =>
        calls += 1
        if calls == 4 then throw new IllegalStateException("stop")
        0xffffffff
      }
    }
    assertEquals(calls, 4)
    assertEquals(output.argbData.toSeq, before)
  }

  test("Color generation uses the same pixel semantics and stays atomic on callback failure") {
    val output = BufferAdapter(3, 2)
    output.setColorsByLocation((x, y) => new Color(pixels(y * 3 + x)))
    val expected = BufferAdapter(3, 2)
    expected.setARGBData(pixels)
    assertEquals(output.argbData.toSeq, expected.argbData.toSeq)
    val before = output.argbData.toSeq
    val _      = intercept[IllegalStateException] {
      output.setColorsByLocation { (x, y) =>
        if x == 1 && y == 1 then throw new IllegalStateException("stop")
        Color(255, 0, 0)
      }
    }
    assertEquals(output.argbData.toSeq, before)
  }
