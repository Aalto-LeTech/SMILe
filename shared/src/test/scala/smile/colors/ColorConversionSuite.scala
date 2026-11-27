package smile.colors

/** Converting a color between its representations (packed integers and HSI) and deriving tints,
  * shades, and the preset colors' aliases from it. [[smile.colors.ColorLawsSuite]] walks the whole
  * color cube.
  */
class ColorConversionSuite extends munit.FunSuite:

  private val red         = Color(255, 0, 0)
  private val green       = Color(0, 255, 0)
  private val blue        = Color(0, 0, 255)
  private val translucent = Color(12, 34, 56, 78)

  test("ARGB and RGBA conversions preserve every channel") {
    assertEquals(new Color(translucent.toARGBInt), translucent)
    assertEquals(Color.fromRgbaInt(translucent.toRGBAInt), translucent)
  }

  test("HSI conversion reproduces primary colors") {
    assertEquals(Color.fromHSI(0, 1, 127.5), red)
    assertEquals(Color.fromHSI(120, 1, 127.5), green)
    assertEquals(Color.fromHSI(240, 1, 127.5), blue)
  }

  test("tinting and shading preserve opacity") {
    assertEquals(translucent.tintByPercentage(0), translucent)
    assertEquals(translucent.shadeByPercentage(0), translucent)
    assertEquals(translucent.tintByPercentage(100), Color(255, 255, 255, 78))
    assertEquals(translucent.shadeByPercentage(100), Color(0, 0, 0, 78))

    // Between the two ends, where the arithmetic is. (12, 34, 56) goes a quarter of the way to
    // white, rounded up, and a quarter of the way to black, rounded down.
    assertEquals(translucent.tintByPercentage(25), Color(73, 90, 106, 78))
    assertEquals(translucent.shadeByPercentage(25), Color(9, 25, 42, 78))
    assertEquals(translucent.tintByFactor(0.25), translucent.tintByPercentage(25))
    val _ = intercept[IllegalArgumentException](translucent.tintByFactor(1.01))
    val _ = intercept[IllegalArgumentException](translucent.shadeByFactor(-0.01))
  }

  test("Color.fromHSI preserves the requested opacity") {
    assertEquals(Color.fromHSI(0, 1, 127.5, opacity = 37), Color(255, 0, 0, 37))
  }

  test("HSI values round-trip through toHSI and fromHSI") {
    val color                        = Color(200, 100, 50)
    val (hue, saturation, intensity) = color.toHSI

    assertEquals(Color.fromHSI(hue, saturation, intensity), color)

    // The cube's corners, and the grays, which have no hue, are where a conversion goes wrong.
    val corners =
      for r <- Seq(0, 255); g <- Seq(0, 255); b <- Seq(0, 255) yield Color(r, g, b)
    val grays = Seq(0, 1, 128, 254, 255).map(level => Color(level, level, level))
    for sample <- corners ++ grays do
      val (h, s, i) = sample.toHSI
      assertEquals(Color.fromHSI(h, s, i), sample, s"$sample as ($h, $s, $i)")
  }

  test("PresetColor.DarkGrey is found by its canonical name") {
    assertEquals(PresetColor.byName("dark grey"), PresetColor.DarkGrey)
    assertEquals(PresetColor.DarkGrey, PresetColor.DarkGray)
  }
