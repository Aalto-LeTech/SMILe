package smile.colors

class PresetColorSuite extends munit.FunSuite:

  test("a preset equals the plain color with the same channels") {
    assertEquals(PresetColor.Red, Color(255, 0, 0))
    assertEquals(PresetColor.Red.hashCode, Color(255, 0, 0).hashCode)
    assertEquals(Map(Color(255, 0, 0) -> 1)(PresetColor.Red), 1)
  }

  test("a preset is a plain Color") {
    assert(PresetColor.Black.getClass eq classOf[Color])
    val channels = PresetColor.Red match
      case Color(r, g, b, a) => (r, g, b, a)
    assertEquals(channels, (255, 0, 0, 255))
  }

  test("byName finds every preset under its canonical name") {
    assertEquals(PresetColor.byName("dark grey"), PresetColor.DarkGrey)
    assertEquals(PresetColor.byName("Hungarian red"), PresetColor.HungarianRed)
    assertEquals(PresetColor.byName("black10"), PresetColor.Black10)
    assertEquals(PresetColor.byName.size, 219)
  }

  test("byName keeps declaration order") {
    assertEquals(
      PresetColor.byName.keys.take(3).toList,
      List("alice blue", "alizarin crimson", "amethyst")
    )
    assertEquals(PresetColor.byName.keys.last, "transparent")
  }
