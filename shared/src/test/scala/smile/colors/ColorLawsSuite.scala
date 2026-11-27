package smile.colors

import smile.modeling.Pos

/** Conversions that have to undo each other, checked over the whole color cube: its corners, its
  * gray diagonal, and a grid through its inside.
  */
class ColorLawsSuite extends munit.FunSuite:

  private val levels = Seq(0, 1, 51, 102, 128, 153, 204, 254, 255)

  private val colors: Seq[Color] =
    for red <- levels; green <- levels; blue <- levels yield Color(red, green, blue)

  private val opacities = Seq(0, 1, 78, 254, 255)

  test("packed ARGB and RGBA integers round-trip every channel, extremes included") {
    for color <- colors.take(200) ++ colors.takeRight(200); opacity <- opacities do
      val translucent = color.copy(opacity = opacity)
      assertEquals(new Color(translucent.toARGBInt), translucent)
      assertEquals(Color.fromRgbaInt(translucent.toRGBAInt), translucent)
      assertEquals(
        Color.argbIntFrom(color.red, color.green, color.blue, opacity),
        translucent.toARGBInt
      )
      assertEquals(
        Color.rgbaIntFrom(color.red, color.green, color.blue, opacity),
        translucent.toRGBAInt
      )
  }

  test("the packed layouts put each channel in its documented byte") {
    val color = Color(0x12, 0x34, 0x56, 0x78)
    assertEquals(color.toARGBInt, 0x78123456)
    assertEquals(color.toRGBAInt, 0x12345678)
    assertEquals(Color(255, 255, 255, 255).toARGBInt, -1)
  }

  test("every color round-trips through HSI to within one step per channel") {
    for color <- colors do
      val (hue, saturation, intensity) = color.toHSI
      val back                         = Color.fromHSI(hue, saturation, intensity)
      def close(a: Int, b: Int)        = (a - b).abs <= 1
      assert(
        close(back.red, color.red) && close(back.green, color.green) && close(
          back.blue,
          color.blue
        ),
        s"$color became ($hue, $saturation, $intensity) and then $back"
      )
  }

  test("a gray has saturation zero and hue zero, and comes back the same gray") {
    for level <- levels do
      val gray                         = Color(level, level, level)
      val (hue, saturation, intensity) = gray.toHSI

      // A gray has no hue of its own. Zero stands in for it, so that what toHSI returns is
      // always something fromHSI takes.
      assertEquals(hue, 0.0, s"hue of $gray")
      assertEquals(saturation, 0.0, s"saturation of $gray")
      assertEquals(intensity, level.toDouble)
      assertEquals(Color.fromHSI(hue, saturation, intensity), gray)
      assert(Color.isGray(level, level, level))
    assert(Color.isBlack(0, 0, 0) && !Color.isBlack(0, 0, 1))
    assert(!Color.isGray(10, 10, 11))
  }

  test("without saturation the hue does not matter") {
    for hue <- Seq(0.0, 90.0, 240.0, 360.0) do
      assertEquals(Color.fromHSI(hue, 0, 100), Color(100, 100, 100))
  }

  test("hues are degrees around the color circle, and a whole turn is zero again") {
    assertEquals(Color.fromHSI(360, 1, 85), Color.fromHSI(0, 1, 85))
    assertEquals(Color.fromHSI(0, 1, 85), Color(255, 0, 0))
    assertEquals(Color.fromHSI(120, 1, 85), Color(0, 255, 0))
    assertEquals(Color.fromHSI(240, 1, 85), Color(0, 0, 255))
    assertEqualsDouble(Color(255, 255, 0).toHSI._1, 60.0, 1e-9)
    assertEqualsDouble(Color(0, 255, 255).toHSI._1, 180.0, 1e-9)
    assertEqualsDouble(Color(255, 0, 255).toHSI._1, 300.0, 1e-9)
  }

  test("HSI values outside their ranges are refused by name") {
    def messageOf(body: => Any): String = intercept[IllegalArgumentException](body).getMessage
    assert(messageOf(Color.fromHSI(-1, 0.5, 100)).contains("Hue"))
    assert(messageOf(Color.fromHSI(361, 0.5, 100)).contains("Hue"))
    assert(messageOf(Color.fromHSI(Double.NaN, 0.5, 100)).contains("Hue"))
    assert(messageOf(Color.fromHSI(10, 1.5, 100)).contains("Saturation"))
    assert(messageOf(Color.fromHSI(10, 0.5, 256)).contains("Intensity"))
    assert(messageOf(Color.fromHSI(10, 0.5, 100, opacity = 256)).contains("Opacity"))
  }

  test("tinting moves every channel toward white and shading toward black, steadily") {
    val factors = Seq(0.0, 0.1, 0.25, 0.5, 0.9, 1.0)
    for color <- colors.grouped(37).map(_.head).toSeq do
      val tints  = factors.map(color.tintByFactor)
      val shades = factors.map(color.shadeByFactor)
      for (lighter, evenLighter) <- tints.zip(tints.tail) do
        assert(
          evenLighter.red >= lighter.red && evenLighter.green >= lighter.green &&
            evenLighter.blue >= lighter.blue,
          s"$color: $tints"
        )
      for (darker, evenDarker) <- shades.zip(shades.tail) do
        assert(
          evenDarker.red <= darker.red && evenDarker.green <= darker.green &&
            evenDarker.blue <= darker.blue,
          s"$color: $shades"
        )
      assertEquals(tints.head, color)
      assertEquals(shades.head, color)
      assertEquals(tints.last, Color(255, 255, 255))
      assertEquals(shades.last, Color(0, 0, 0))
  }

  test("tinting and shading keep the opacity, by factor, by percentage, and by default") {
    val color = Color(100, 150, 200, 78)

    assertEquals(color.tintByFactor(0.5), Color(178, 203, 228, 78))
    assertEquals(color.shadeByFactor(0.5), Color(50, 75, 100, 78))
    assertEquals(color.tintByPercentage(50), color.tintByFactor(0.5))
    assertEquals(color.shadeByPercentage(50), color.shadeByFactor(0.5))
    assertEquals(color.lighter, color.tintByFactor(0.1))
    assertEquals(color.darker, color.shadeByFactor(0.1))
    assert(!color.isOpaque && color.copy(opacity = 255).isOpaque)
  }

  test("a tinting or shading factor outside zero to one is refused as such") {
    // Without a check of the factor itself, the refusal surfaces as "Green must be between 0 and
    // 255", a complaint about a channel the caller never gave.
    for (name, body) <- Seq[(String, Double => Color)](
        "Tinting factor" -> Color(100, 150, 200).tintByFactor,
        "Shading factor" -> Color(100, 150, 200).shadeByFactor
      )
    do
      for factor <- Seq(-0.1, 1.5, Double.NaN) do
        val message = intercept[IllegalArgumentException](body(factor)).getMessage
        assert(message.contains(name) && message.contains(factor.toString), message)
    val _ = intercept[IllegalArgumentException](Color(1, 2, 3).tintByPercentage(101))
    val _ = intercept[IllegalArgumentException](Color(1, 2, 3).shadeByPercentage(-1))
  }

  test("a channel outside 0 to 255 is refused with the channel's name and the value") {
    def messageOf(body: => Any): String = intercept[IllegalArgumentException](body).getMessage
    assert(
      messageOf(Color(256, 0, 0)).contains("Red") && messageOf(Color(256, 0, 0)).contains("256")
    )
    assert(
      messageOf(Color(0, -1, 0)).contains("Green") && messageOf(Color(0, -1, 0)).contains("-1")
    )
    assert(messageOf(Color(0, 0, 300)).contains("Blue"))
    assert(messageOf(Color(0, 0, 0, 256)).contains("Opacity"))
  }

  test("a color prints as the hexadecimal notation the renderers hand to CSS") {
    assertEquals(Color(255, 0, 128).toString, "#FF0080")
    assertEquals(Color(1, 2, 3, 4).toString, "#01020304")
    assertEquals(Color(0, 0, 0, 0).toString, "#00000000")
  }

  test("a preset color is the color with its channels: equal to it, hashed like it") {
    val red = Color(255, 0, 0)

    assertEquals(PresetColor.Red: Color, red)
    assertEquals(red, PresetColor.Red: Color)
    assertEquals(PresetColor.Red.hashCode, red.hashCode)
    assertEquals(Set[Color](PresetColor.Red, red).size, 1)
    assertEquals(PresetColor.Red.averageColor, red)
    assertEquals(PresetColor.Transparent.opacity, 0)
  }

  test("a gradient averages its stops by opacity, and a color is its own average") {
    val gradient = LinearGradient(
      Pos(0, 0),
      Pos(10, 0),
      Seq(
        0.0 -> Color(0, 30, 60, 90),
        0.5 -> Color(30, 60, 90, 120),
        1.0 -> Color(60, 90, 120, 150)
      ),
      LinearGradient.CycleMethod.Repeat
    )
    assertEquals(gradient.averageColor, Color(33, 63, 93, 120))
    assertEquals(Color(1, 2, 3, 4).averageColor, Color(1, 2, 3, 4))
  }
