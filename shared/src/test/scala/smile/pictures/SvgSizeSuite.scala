package smile.pictures

import smile.modeling.Bounds

import java.io.IOException

/** The size of an SVG document from its root's `width`, `height` and `viewBox`, which every
  * platform resolves alike: absolute units at 96 dpi, the viewBox's ratio for a missing side, and
  * 300 by 150 when nothing says otherwise.
  */
class SvgSizeSuite extends munit.FunSuite:

  private def size(
      width: String = "",
      height: String = "",
      viewBox: String = ""
  ): (Double, Double) =
    SvgReading.size(
      Option(width).filter(_.nonEmpty),
      Option(height).filter(_.nonEmpty),
      Option(viewBox).filter(_.nonEmpty)
    )

  private def assertSize(actual: (Double, Double), width: Double, height: Double): Unit =
    assertEqualsDouble(actual(0), width, 1e-3)
    assertEqualsDouble(actual(1), height, 1e-3)

  test("absolute units resolve at 96 dpi") {
    assertSize(size("210mm", "297mm"), 793.7008, 1122.5197)
    assertSize(size("1in", "72pt"), 96, 96)
    assertSize(size("6pc", "2.54cm"), 96, 96)
    assertSize(size("2em", "2ex"), 32, 16)
    assertSize(size("12", "12px"), 12, 12)
    assertSize(size(" 1.5E1PX ", "+.5e1"), 15, 5)
  }

  test("percentages take the viewBox's size") {
    assertSize(size("100%", "100%", "0 0 40 30"), 40, 30)
    assertSize(size("50%", "", "0,0,40,30"), 40, 30)
  }

  test("auto, as SVG 2 allows, leaves a side unspecified") {
    assertSize(size("auto", "auto", "0 0 40 30"), 40, 30)
    assertSize(size("80", " AUTO ", "0 0 40 30"), 80, 60)
    assertSize(size(width = "auto"), 300, 150)
  }

  test("one missing side keeps the viewBox's ratio") {
    assertSize(size(width = "80", viewBox = "0 0 40 30"), 80, 60)
    assertSize(size(height = "60", viewBox = "0 0 40 30"), 80, 60)
    assertSize(size("80", "100%", "0 0 40 30"), 80, 60)
  }

  test("no size takes the viewBox, and 300 by 150 without one") {
    assertSize(size(viewBox = "10 20 40 30"), 40, 30)
    assertSize(size(viewBox = "-10,-20, 40 , 30"), 40, 30)
    assertSize(size(), 300, 150)
    assertSize(size(width = "80"), 80, 150)
    assertSize(size(height = "60"), 300, 60)
    assertSize(size("  ", "", "   "), 300, 150)
  }

  test("bad sizes are refused with the value in the message") {
    for (value, message) <- Seq(
        "0"     -> "its width '0' is not a positive length",
        "-5"    -> "its width '-5' is not a positive length",
        "abc"   -> "its width 'abc' is not a positive length",
        "5vw"   -> "its width '5vw' is not a positive length",
        "12 px" -> "its width '12 px' is not a positive length",
        "1e400" -> "its width '1e400' is not a positive length",
        "-5%"   -> "its width '-5%' is not a positive length",
        "0x10"  -> "its width '0x10' is not a positive length",
        "NaN"   -> "its width 'NaN' is not a positive length"
      )
    do assertEquals(intercept[IOException](size(width = value)).getMessage, message)
    assertEquals(
      intercept[IOException](size(height = "2q")).getMessage,
      "its height '2q' is not a positive length"
    )
    for viewBox <- Seq("bad 0 2 2", "0 0 0 5", "0 0 5 -1", "0 0 5", "0 0 5 5 5") do
      assertEquals(
        intercept[IOException](size(viewBox = viewBox)).getMessage,
        s"its viewBox '$viewBox' is not four numbers with a positive width and height"
      )
  }

  test("a side over the maximum is refused, and one at it is not") {
    assertEquals(SvgReading.MaxSide, 16384)
    assertSize(size("16384", "1"), 16384, 1)
    assertEquals(
      intercept[IOException](size(width = "20000")).getMessage,
      "it is 20000 x 150 pixels, and a side may be at most 16384"
    )
    assertEquals(
      intercept[IOException](size(width = "100", viewBox = "0 0 1 200")).getMessage,
      "it is 100 x 20000 pixels, and a side may be at most 16384"
    )
    assertEquals(
      intercept[IOException](size("16384.5", "2.25")).getMessage,
      "it is 16384.5 x 2.25 pixels, and a side may be at most 16384"
    )
  }

  test("an Inkscape size in mm is the same picture on every platform") {
    val bounds = SvgPicture
      .fromString(
        """<svg xmlns="http://www.w3.org/2000/svg" width="210mm" height="297mm" viewBox="0 0 210 297"/>"""
      )
      .bounds
      .get
    assertEqualsDouble(bounds.upperLeftCorner.x, -396.85, 0.01)
    assertEqualsDouble(bounds.upperLeftCorner.y, -561.26, 0.01)
    assertEqualsDouble(bounds.lowerRightCorner.x, 396.85, 0.01)
    assertEqualsDouble(bounds.lowerRightCorner.y, 561.26, 0.01)
  }

  test("a document with no size or a percentage size loads at the viewBox's size or 300 by 150") {
    def bounds(attributes: String) =
      SvgPicture.fromString(s"""<svg xmlns="http://www.w3.org/2000/svg" $attributes/>""").bounds
    assertEquals(bounds(""), Some(Bounds(-150, -75, 150, 75)))
    assertEquals(
      bounds("""width="100%" height="100%" viewBox="0 0 40 30""""),
      Some(Bounds(-20, -15, 20, 15))
    )
    assertEquals(bounds("""width="80" viewBox="0 0 40 30""""), Some(Bounds(-40, -30, 40, 30)))
  }

  test("a document with a side over 16384 pixels is refused") {
    val error = intercept[IOException](
      SvgPicture.fromString("""<svg xmlns="http://www.w3.org/2000/svg" width="20000"/>""")
    )
    assertEquals(
      error.getMessage,
      "SVG could not be read: it is 20000 x 150 pixels, and a side may be at most 16384"
    )
  }
end SvgSizeSuite
