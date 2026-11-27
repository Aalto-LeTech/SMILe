package smile.infrastructure

import smile.colors.Color

/** Pixel tests of the packed ARGB transform. */
class NoiseBufferSuite extends munit.FunSuite:
  private def legacy(source: JSBufferAdapter, f: Int => Int): JSBufferAdapter =
    val data = source.argbData
    var i    = 0
    while i < data.length do
      data(i) = f(data(i))
      i += 1
    val result = JSBufferAdapter(source.width, source.height)
    result.setARGBData(data)
    result

  test("direct transform matches packed path for alpha and row-major callback order") {
    val source = JSBufferAdapter(4, 2)
    source.setARGBData(
      Array(0xff123456, 0x80123456, 0x010000ff, 0x00000000, 0xffffffff, 0xff000000, 0xfeabcdef,
        0x4000ff00)
    )
    val before = source.argbData.toSeq
    var seen   = Vector.empty[Int]
    val actual = source.transformARGB { p =>
      seen :+= p
      p ^ 0x7fffffff
    }
    assertEquals(seen, before.toVector)
    assertEquals(actual.argbData.toSeq, legacy(source, _ ^ 0x7fffffff).argbData.toSeq)
    assertEquals(source.argbData.toSeq, before)
    actual.setRGBA(0, 0, Color(255, 0, 0))
    assertEquals(source.argbData.toSeq, before)
  }

  test("callback failure leaves source untouched") {
    val source = JSBufferAdapter(3, 1)
    source.setARGBData(Array(0xff112233, 0xff445566, 0xff778899))
    val before = source.argbData.toSeq
    var calls  = 0
    val _      = intercept[IllegalStateException] {
      source.transformARGB { p =>
        calls += 1
        if calls == 2 then throw new IllegalStateException("stop")
        p ^ 0x00ffffff
      }
    }
    assertEquals(calls, 2)
    assertEquals(source.argbData.toSeq, before)
  }
