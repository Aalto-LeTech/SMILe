package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.testing.Cases.stroke as strokeOf
import smile.testing.TextFixtures.boxedText
import smile.testing.{InkAssertions, PointAssertions}

/** Moving, scaling, and rotating elements. Transformations that undo each other restore the
  * element, a rotation around an element's own center keeps it there, and every element kind
  * (vertices, arcs, texts, bitmaps' pixels) turns counterclockwise for a positive angle.
  */
// Turns away from right angles place elements to within a few steps of the 2^-20 pixel grid
// their translations lie on, hence the tolerance of 4e-6 below.
class ElementTransformSuite extends munit.FunSuite with InkAssertions with PointAssertions:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  test("moveBy accumulates and moveTo sets an absolute center") {
    val rectangle = Rectangle(4, 2, Pos(10, 20), fillWith(red), line = None)

    assertEquals(rectangle.moveBy(5, -3).moveBy(5, -3).anchor, Pos(20, 14))
    assertEquals(rectangle.moveTo(Pos(70, 80)).anchor, Pos(70, 80))
  }

  test("scaling up and back down restores a shape's boundary") {
    val rectangle = Rectangle(10, 4, Pos(10, 20), fillWith(red), line = None)

    assertEquals(rectangle.scaleBy(2, 2).scaleBy(0.5, 0.5).bounds.get, rectangle.bounds.get)

    // A plain upright rectangle would come back from a good many wrong scalings too. A
    // stroked, lopsided triangle, around a point of its own choosing:
    val triangle = Triangle(30, 18, 26, Pos(5, -3), fillWith(red), strokeOf(4))
    val pivot    = Pos(-7, 11)
    assertClose(
      triangle.scaleAround(3, 3, pivot).scaleAround(1.0 / 3, 1.0 / 3, pivot).bounds.get,
      triangle.bounds.get,
      4e-6,
      "a stroked scalene triangle"
    )
    // Unequal factors are undone only by an unstroked shape, because a stroke's one width follows
    // the mean of the two factors.
    val unstroked = Triangle(30, 18, 26, Pos(5, -3), fillWith(red), None)
    assertClose(
      unstroked.scaleAround(4, 0.5, pivot).scaleAround(0.25, 2, pivot).bounds.get,
      unstroked.bounds.get,
      4e-6,
      "an unstroked scalene triangle"
    )
    assertEquals(unstroked.scaleBy(4, 0.5).bounds.get.width, 4 * 30.0)
  }

  test("rotating a shape forth and back around a pivot restores its boundary") {
    val rectangle = Rectangle(8, 4, Pos(10, 20), fillWith(red), line = None)
    val pivot     = Pos(-3, 7)

    val roundTripped = rectangle.rotateAround(37, pivot).rotateAround(-37, pivot)

    assertEqualsDouble(roundTripped.bounds.get.center.x, 10.0, 4e-6)
    assertEqualsDouble(roundTripped.bounds.get.center.y, 20.0, 4e-6)
    assertEqualsDouble(roundTripped.bounds.get.width, 8.0, 4e-6)
    assertEqualsDouble(roundTripped.bounds.get.height, 4.0, 4e-6)
  }

  test("rotating a Polygon around its own center keeps that center fixed") {
    val rectangle = Rectangle(8, 4, Pos(10, 20), fillWith(red), line = None)

    val rotated = rectangle.rotateAround(90, rectangle.anchor)

    assertEquals(rotated.bounds.get.center, rectangle.anchor)
    assertEquals(rotated.bounds.get.width, 4.0)
    assertEquals(rotated.bounds.get.height, 8.0)

    // A rectangle looks the same turned either way. A scalene triangle does not. Its apex,
    // up and to the left of its position, goes down and to the left in a counterclockwise quarter
    // turn.
    val triangle = Triangle(10, 6, 8, Pos(10, 20), fillWith(red), line = None)
    val turned   = triangle.rotateAround(90, triangle.anchor)
    assertEquals(turned.anchor, triangle.anchor)
    assertPoints(
      turned.vertices.map(_ - triangle.anchor),
      Seq((-2.4, 1.4), (2.4, 5.0), (2.4, -5.0))
    )
    for angle <- Seq(30.0, 137.5, -211.5) do
      assertEquals(triangle.rotateAround(angle, triangle.anchor).anchor, triangle.anchor)
  }

  test("rotating an Arc around its own center keeps bounds centered there") {
    val ellipse = Arc(
      pos = Pos(10, 20),
      width = 8,
      height = 4,
      fill = fillWith(red),
      line = None
    )

    val rotated = ellipse.rotateAround(90, ellipse.anchor)

    assertEquals(rotated.bounds.get.center, ellipse.anchor)
    assertEquals(rotated.bounds.get.width, 4.0)
    assertEquals(rotated.bounds.get.height, 8.0)
  }

  test("an Arc rotated around a pivot turns the same way it travels") {
    // A flat ellipse lying along the x axis, pointing at the origin from the right.
    val ellipse = Arc(pos = Pos(20, 0), width = 30, height = 4, fill = fillWith(red), line = None)

    val rotated = ellipse.rotateAround(45, Pos.Origin)
    val bitmap  = rotated.toBitmap()

    // A positive angle moves the center counterclockwise on screen, up and to the right.
    assert(rotated.anchor.x > 0 && rotated.anchor.y < 0)
    // A rigid rotation keeps the ellipse pointing at the pivot, so the painted content runs from
    // the lower left to the upper right. If the position travels one way while the renderers turn
    // the shape the other, the ellipse is painted across the boundary computed for it.
    val (quarterWidth, quarterHeight) = (bitmap.pixels().width / 4, bitmap.pixels().height / 4)
    assert(bitmap.getColor(3 * quarterWidth, quarterHeight).opacity > 0)
    assert(bitmap.getColor(quarterWidth, 3 * quarterHeight).opacity > 0)
    assertEquals(bitmap.getColor(quarterWidth, quarterHeight).opacity, 0)
    assertEquals(bitmap.getColor(3 * quarterWidth, 3 * quarterHeight).opacity, 0)
  }

  test("every element kind travels counterclockwise around a pivot for a positive angle") {
    val start  = Pos(20, 0)
    val square = Rectangle(4, 4, start, fillWith(red), line = None)

    // Each element starts centered at three o'clock. A quarter turn around the origin takes
    // it to twelve o'clock, and one around the point below it takes it to that point's left.
    def check(kind: String, aroundOrigin: Bounds, aroundPivot: Bounds): Unit =
      assertEqualsDouble(aroundOrigin.center.x, 0.0, 4e-6, kind)
      assertEqualsDouble(aroundOrigin.center.y, -20.0, 4e-6, kind)
      assertEqualsDouble(aroundPivot.center.x, 0.0, 4e-6, kind)
      assertEqualsDouble(aroundPivot.center.y, 20.0, 4e-6, kind)

    val pivot   = Pos(20, 20)
    val ellipse = Arc(pos = start, width = 6, height = 4, fill = fillWith(red), line = None)
    val text    = boxedText("L", 0).moveTo(start)
    val bitmap  = Bitmap(4, 4, Seq.fill(16)(red)).moveBy(start.x, start.y)
    val picture = square
    val masked  = MaskGroup(square, picture)

    check(
      "Polygon",
      square.rotateAround(90, Pos.Origin).bounds.get,
      square.rotateAround(90, pivot).bounds.get
    )
    check(
      "Arc",
      ellipse.rotateAround(90, Pos.Origin).bounds.get,
      ellipse.rotateAround(90, pivot).bounds.get
    )
    check(
      "Text",
      text.rotateAround(90, Pos.Origin).bounds.get,
      text.rotateAround(90, pivot).bounds.get
    )
    check(
      "Bitmap",
      bitmap.rotateAround(90, Pos.Origin).bounds.get,
      bitmap.rotateAround(90, pivot).bounds.get
    )
    check(
      "Picture",
      picture.rotateAround(90, Pos.Origin).bounds.get,
      picture.rotateAround(90, pivot).bounds.get
    )
    check(
      "MaskGroup",
      masked.rotateAround(90, Pos.Origin).bounds.get,
      masked.rotateAround(90, pivot).bounds.get
    )
    assertEquals(Point(start, red).rotateAround(90, Pos.Origin).anchor, Pos(0, -20))
    assertEquals(Point(start, red).rotateAround(90, pivot).anchor, Pos(0, 20))

    // A quarter turn takes an exact path of its own in every element. 30 degrees takes the
    // trigonometric one. Three o'clock at radius 20 goes to (17.32, -10), above the x axis.
    val expected = Pos(10 * math.sqrt(3), -10)
    val turned   = Seq[(String, Element)](
      "Polygon"   -> square.rotateAround(30, Pos.Origin),
      "Arc"       -> ellipse.rotateAround(30, Pos.Origin),
      "Text"      -> text.rotateAround(30, Pos.Origin),
      "Bitmap"    -> bitmap.rotateAround(30, Pos.Origin),
      "MaskGroup" -> masked.rotateAround(30, Pos.Origin),
      "Point"     -> Point(start, red).rotateAround(30, Pos.Origin)
    )
    for (kind, element) <- turned do
      assertClose(element.bounds.get.center, expected, 4e-6, s"$kind around the origin")
    assertClose(picture.rotateAround(30, Pos.Origin).bounds.get.center, expected, 4e-6, "Picture")
  }

  test("a rotated Polygon's vertices turn counterclockwise for a positive angle") {
    val rectangle = Rectangle(8, 4, Pos(10, 20), fillWith(red), line = None)

    val turned = rectangle.rotateAround(90, Pos(10, 20))

    assertEquals(turned.anchor, Pos(10, 20))
    // The upper left vertex turns to the lower left, and the others follow it round.
    assertEquals(
      turned.vertices.map(_ - Pos(10, 20)),
      Seq(Pos(-2, 4), Pos(-2, -4), Pos(2, -4), Pos(2, 4))
    )
  }

  test("a Bitmap's pixels turn counterclockwise with its position for a positive angle") {
    // Red on the left half, blue on the right.
    val colors  = Seq.fill(4)(Seq.fill(4)(red) ++ Seq.fill(4)(blue)).flatten
    val bitmap  = Bitmap(8, 4, colors).moveBy(20, 0)
    val rotated = bitmap.rotateAround(90, Pos.Origin)

    def isMostlyRed(color: Color): Boolean  = color.red > color.blue
    def isMostlyBlue(color: Color): Boolean = color.blue > color.red

    assertEquals(rotated.bounds.get, Bounds(Pos(-2, -24), Pos(2, -16)))
    assertEquals((rotated.pixels().width, rotated.pixels().height), (4, 8))
    // The left half turns to the bottom, the right half to the top.
    assert(isMostlyBlue(rotated.getColor(1, 1)), s"top: ${rotated.getColor(1, 1)}")
    assert(isMostlyRed(rotated.getColor(1, 6)), s"bottom: ${rotated.getColor(1, 6)}")

    // Around another pivot the pixels turn the same way, and a negative angle the other.
    val pivoted = bitmap.rotateAround(-90, Pos(20, 20))
    assertEquals(pivoted.bounds.get, Bounds(Pos(38, 16), Pos(42, 24)))
    assert(isMostlyRed(pivoted.getColor(1, 1)), s"top: ${pivoted.getColor(1, 1)}")
    assert(isMostlyBlue(pivoted.getColor(1, 6)), s"bottom: ${pivoted.getColor(1, 6)}")
  }
