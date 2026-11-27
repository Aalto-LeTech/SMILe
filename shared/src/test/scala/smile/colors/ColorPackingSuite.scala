package smile.colors

class ColorPackingSuite extends munit.FunSuite:

  test("instance packing matches both validated helpers across channel boundaries") {
    val levels = Seq(0, 1, 127, 128, 254, 255)
    for red <- levels; green <- levels; blue <- levels; opacity <- levels do
      val color = Color(red, green, blue, opacity)
      assertEquals(color.toARGBInt, Color.argbIntFrom(red, green, blue, opacity))
      assertEquals(color.toRGBAInt, Color.rgbaIntFrom(red, green, blue, opacity))
  }

  test("raw channel helpers still reject invalid components in both layouts") {
    val helpers = Seq[(Int, Int, Int, Int) => Int](Color.argbIntFrom, Color.rgbaIntFrom)
    for pack <- helpers; value <- Seq(-1, 256) do
      for (name, channels) <- Seq(
          "Red"     -> (value, 2, 3, 4),
          "Green"   -> (1, value, 3, 4),
          "Blue"    -> (1, 2, value, 4),
          "Opacity" -> (1, 2, 3, value)
        )
      do
        val (red, green, blue, opacity) = channels
        val error = intercept[IllegalArgumentException](pack(red, green, blue, opacity))
        assert(error.getMessage.contains(name))
        assert(error.getMessage.contains(value.toString))
  }
