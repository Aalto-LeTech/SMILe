package smile.colors

import smile.modeling.{Bounds, Pos}
import smile.pictures.*
import smile.testing.Cases.stroke as strokeOf
import smile.testing.InkAssertions

/** Where a gradient lands on the element it fills. Its points are element-local, so it follows the
  * element when it moves, turns counterclockwise with it, stretches with it, and on an arc is
  * anchored to the arc's own box (issue #9). [[smile.colors.GradientRigiditySuite]] states the same
  * as one law over every element. These are the worked cases, checked on the pixels.
  */
class GradientPlacementSuite extends munit.FunSuite with InkAssertions:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  private val redToBlue = LinearGradient(Pos(0, 0), Pos(40, 0), red, blue)

  private def isMostlyRed(color: Color): Boolean =
    color.opacity > 0 && color.red > color.blue + 60
  private def isMostlyBlue(color: Color): Boolean =
    color.opacity > 0 && color.blue > color.red + 60
  private def isBlended(color: Color): Boolean =
    color.opacity > 0 && (color.red - color.blue).abs <= 60

  /** Asserts which color `element`, centered on `center` and about 40 pixels across, shows near
    * each of its four sides.
    */
  private def assertSides(
      description: String,
      element: Element,
      center: Pos,
      left: Color => Boolean,
      top: Color => Boolean,
      right: Color => Boolean,
      bottom: Color => Boolean
  ): Unit =
    val colorAt = colorsOf(element)
    def check(side: String, expected: Color => Boolean, dx: Double, dy: Double): Unit =
      val color = colorAt(center.x + dx, center.y + dy)
      assert(expected(color), s"$description: $side side is $color")
    check("left", left, -15, 0)
    check("top", top, 0, -15)
    check("right", right, 15, 0)
    check("bottom", bottom, 0, 15)

  test("a gradient turns counterclockwise with the ellipse it fills") {
    // Red at the ellipse's left end, blue at its right end.
    val gradient                               = LinearGradient(Pos(0, 0), Pos(40, 0), red, blue)
    def ellipse(rotationAngle: Double): Bitmap =
      Arc(pos = Pos.Origin, width = 40, height = 10, fill = fillWith(gradient), line = None)
        .rotateAround(rotationAngle, Pos.Origin)
        .toBitmap()

    def isMostlyBlue(color: Color): Boolean = color.opacity > 0 && color.blue > color.red
    def isMostlyRed(color: Color): Boolean  = color.opacity > 0 && color.red > color.blue

    val upright = ellipse(0)
    assert(isMostlyRed(upright.getColor(4, 5)), s"left end: ${upright.getColor(4, 5)}")
    assert(isMostlyBlue(upright.getColor(35, 5)), s"right end: ${upright.getColor(35, 5)}")

    // +90 degrees carries the right end to the top, and the gradient goes with it.
    val turnedLeft = ellipse(90)
    assertEquals((turnedLeft.pixels().width, turnedLeft.pixels().height), (10, 40))
    assert(isMostlyBlue(turnedLeft.getColor(5, 4)), s"top end: ${turnedLeft.getColor(5, 4)}")
    assert(isMostlyRed(turnedLeft.getColor(5, 35)), s"bottom end: ${turnedLeft.getColor(5, 35)}")

    // -90 degrees carries it to the bottom.
    val turnedRight = ellipse(-90)
    assert(isMostlyRed(turnedRight.getColor(5, 4)), s"top end: ${turnedRight.getColor(5, 4)}")
    assert(isMostlyBlue(turnedRight.getColor(5, 35)), s"bottom: ${turnedRight.getColor(5, 35)}")

    // At +45 degrees the blue end is the upper right one.
    val diagonal = ellipse(45)
    val last     = diagonal.pixels().width - 1
    assert(isMostlyRed(diagonal.getColor(5, last - 5)), s"${diagonal.getColor(5, last - 5)}")
    assert(
      isMostlyBlue(diagonal.getColor(last - 5, 5)),
      s"${diagonal.getColor(last - 5, 5)}"
    )
  }

  test("a gradient turns with the square, the circle, and the pentagon it fills") {
    val shapes = Seq[(String, Element)](
      "square"   -> Rectangle(40, 40, Pos.Origin, fillWith(redToBlue), line = None),
      "circle"   -> Circle(Pos.Origin, 20, fillWith(redToBlue), line = None),
      "pentagon" -> Pentagon(40, 40, Pos.Origin, fillWith(redToBlue), line = None)
    )

    for (name, shape) <- shapes do
      assertSides(name, shape, Pos.Origin, isMostlyRed, isBlended, isMostlyBlue, isBlended)
      // A counterclockwise quarter turn carries the red left edge to the bottom.
      assertSides(
        s"$name turned by 90",
        shape.rotateAround(90, Pos.Origin),
        Pos.Origin,
        isBlended,
        isMostlyBlue,
        isBlended,
        isMostlyRed
      )
      // A half turn reverses the gradient.
      assertSides(
        s"$name turned by 180",
        shape.rotateAround(180, Pos.Origin),
        Pos.Origin,
        isMostlyBlue,
        isBlended,
        isMostlyRed,
        isBlended
      )
      // A clockwise quarter turn carries the red edge to the top.
      assertSides(
        s"$name turned by -90",
        shape.rotateAround(-90, Pos.Origin),
        Pos.Origin,
        isBlended,
        isMostlyRed,
        isBlended,
        isMostlyBlue
      )
      // Turns add up, also when the shape travels around a pivot as it turns.
      assertSides(
        s"$name turned by 30 and then 60 around a pivot",
        shape.rotateAround(30, Pos(20, 0)).rotateAround(60, Pos(20, 0)),
        Pos(20, 20),
        isBlended,
        isMostlyBlue,
        isBlended,
        isMostlyRed
      )

    // Those gradients run through the middle of shapes that are their own mirror images, and
    // the color bands are so wide that a gradient ten pixels out of place would pass. A short
    // one, off to one side of a square, has to land within a couple of pixels. It runs across
    // the top-left corner, from red at 4 pixels in to blue at 12.
    val corner  = LinearGradient(Pos(4, 4), Pos(12, 12), red, blue)
    val square  = Rectangle(40, 40, Pos.Origin, fillWith(corner), line = None)
    val colorAt = colorsOf(square.rotateAround(90, Pos.Origin))
    // Turned a quarter counterclockwise, the top-left corner is the bottom-left one.
    assert(isMostlyRed(colorAt(-15, 15)), s"at the corner: ${colorAt(-15, 15)}")
    assert(isBlended(colorAt(-12, 12)), s"halfway along: ${colorAt(-12, 12)}")
    assert(isMostlyBlue(colorAt(-8, 8)), s"past the gradient's end: ${colorAt(-8, 8)}")
    assert(isMostlyBlue(colorAt(-15, -15)) && isMostlyBlue(colorAt(15, 15)), "the other corners")
  }

  test("a rotated circle keeps its exact boundary") {
    val plain    = Circle(Pos(10, 20), 15, fillWith(red), strokeOf(4))
    val gradient = Circle(Pos(10, 20), 15, fillWith(redToBlue), strokeOf(4))

    for circle <- Seq(plain, gradient); angle <- Seq(37.0, 90.0, -211.5) do
      assertClose(circle.rotateBy(angle).bounds.get, circle.bounds.get, 1e-9, s"by $angle")
      assertClose(
        circle.rotateAround(90, Pos.Origin).bounds.get,
        Bounds(Pos(3, -27), Pos(37, 7)),
        1e-9,
        "a quarter turn around the origin"
      )
  }

  test("a gradient's points stay element-local through a rotation and back") {
    val rectangle = Rectangle(40, 40, Pos(30, 10), fillWith(redToBlue), strokeOf(2))

    def gradientOf(element: Element): LinearGradient = element match
      case shape: Shape =>
        shape.fill.map(_.paint).collect { case gradient: LinearGradient => gradient }.get
      case other => fail(s"$other is not a shape")

    // The gradient lives in the rectangle's own frame, which a turn only places differently, so
    // its points stay as they were, and the turned rectangle paints it up its height.
    assertEquals(gradientOf(rectangle.rotateBy(90)), redToBlue)
    val pivot = Pos(-5, 60)
    assertEquals(gradientOf(rectangle.rotateAround(37, pivot).rotateAround(-37, pivot)), redToBlue)
    assertSides(
      "a square turned by 90",
      rectangle.rotateBy(90),
      Pos(30, 10),
      isBlended,
      isMostlyBlue,
      isBlended,
      isMostlyRed
    )
  }

  test("a gradient stretches with the shape it fills") {
    val across20Pixels = fillWith(LinearGradient(Pos(0, 0), Pos(20, 0), red, blue))
    val shapes         = Seq[(String, Element)](
      "square" -> Rectangle(20, 20, Pos.Origin, across20Pixels, None),
      "circle" -> Circle(Pos.Origin, 10, across20Pixels, None)
    )

    // Twice as wide, the gradient still runs from the left edge to the right one. A gradient that
    // keeps its 20 pixels leaves the right half plain blue.
    for (name, shape) <- shapes do
      assertSides(
        s"$name scaled",
        shape.scaleBy(2, 2),
        Pos.Origin,
        isMostlyRed,
        isBlended,
        isMostlyBlue,
        isBlended
      )
      assertSides(
        s"$name scaled around a point",
        shape.scaleAround(2, 2, Pos(-20, -20)),
        Pos(20, 20),
        isMostlyRed,
        isBlended,
        isMostlyBlue,
        isBlended
      )
      // With unequal factors, three times as wide and twice as high, the gradient still spans the
      // width. It is sampled at the same 15 pixels from the center, a quarter of the way in from
      // either end of a 60 pixel run.
      assertSides(
        s"$name widened",
        shape.scaleBy(3, 2),
        Pos.Origin,
        isMostlyRed,
        isBlended,
        isMostlyBlue,
        isBlended
      )
      // A negative factor mirrors the shape and the gradient on it, so red goes to the right.
      assertSides(
        s"$name mirrored",
        shape.scaleBy(-2, 2),
        Pos.Origin,
        isMostlyBlue,
        isBlended,
        isMostlyRed,
        isBlended
      )
      assertSides(
        s"$name mirrored top to bottom, which leaves a level gradient as it was",
        shape.scaleBy(2, -2),
        Pos.Origin,
        isMostlyRed,
        isBlended,
        isMostlyBlue,
        isBlended
      )
  }

  test("a picture rotated as a whole looks like its elements rotated one by one") {
    val square  = Rectangle(40, 40, Pos(5, 0), fillWith(redToBlue), line = None)
    val circle  = Circle(Pos(-30, 10), 12, fillWith(redToBlue), line = None)
    val picture = Picture(square, circle)
    val pivot   = Pos(3, 4)

    val asAWhole = picture.rotateAround(90, pivot).toBitmap()
    val oneByOne =
      Picture(square.rotateAround(90, pivot), circle.rotateAround(90, pivot)).toBitmap()
    assertEquals(asAWhole.pixels().colors, oneByOne.pixels().colors)

    // That much repeats how a picture's rotation is written. The stronger check compares it with
    // a route that shares none of its code. The picture is rasterized first and its pixels
    // turned. The two agree on where the red and the blue ends of both gradients went.
    val turnedPixels = picture.toBitmap().rotateAround(90, pivot)
    assertClose(
      turnedPixels.bounds.get,
      picture.rotateAround(90, pivot).bounds.get,
      1.0,
      "the same place"
    )
    val probes = for
      x <- 3 until asAWhole.pixels().width by 6; y <- 3 until asAWhole.pixels().height by 6
    yield (x, y)
    val disagreeing = probes.count: (x, y) =>
      val (vector, raster) = (asAWhole.getColor(x, y), turnedPixels.getColor(x, y))
      isMostlyRed(vector) != isMostlyRed(raster) || isMostlyBlue(vector) != isMostlyBlue(raster)
    assert(probes.size > 40 && disagreeing <= probes.size / 20, s"$disagreeing of ${probes.size}")

    // A rasterized square turns its pixels, and the vector square turns its gradient the same way.
    val rasterized = square.toBitmap().rotateAround(90, square.anchor)
    val vector     = square.rotateAround(90, square.anchor).toBitmap()
    for (x, y) <- Seq((20, 5), (20, 35)) do
      assertEquals(isMostlyRed(vector.getColor(x, y)), isMostlyRed(rasterized.getColor(x, y)))
      assertEquals(isMostlyBlue(vector.getColor(x, y)), isMostlyBlue(rasterized.getColor(x, y)))
    assert(isMostlyBlue(vector.getColor(20, 5)) && isMostlyRed(vector.getColor(20, 35)))

    // A mask group turns its mask and its content together.
    val masked = MaskGroup(square, square)
    assertSides(
      "a mask group turned by 90",
      masked.rotateAround(90, square.anchor),
      square.anchor,
      isBlended,
      isMostlyBlue,
      isBlended,
      isMostlyRed
    )
  }

  test("#9 a point-defined gradient spans the element it fills") {
    // The gradient's points are element-local. (0, 0) is the boundary's top-left corner, so
    // (0, 0) -> (width, 0) is a horizontal gradient across the whole shape. The rectangle sits
    // away from the origin to catch renderers that resolve the points in world coordinates.
    val gradient  = LinearGradient(Pos(0, 0), Pos(40, 0), red, blue)
    val rectangle = Rectangle(40, 10, Pos(120, -60), fillWith(gradient), line = None)

    val bitmap = rectangle.toBitmap()

    assertEquals(bitmap.pixels().width, 40)
    assertEquals(bitmap.pixels().height, 10)

    val middleRow  = bitmap.pixels().height / 2
    val leftPixel  = bitmap.getColor(1, middleRow)
    val rightPixel = bitmap.getColor(bitmap.pixels().width - 2, middleRow)

    // The backends interpolate gradients slightly differently, so only dominant channels count.
    assert(
      leftPixel.red > leftPixel.blue,
      s"the left edge should be predominantly red, was $leftPixel"
    )
    assert(
      rightPixel.blue > rightPixel.red,
      s"the right edge should be predominantly blue, was $rightPixel"
    )
  }

  test("#9 a point-defined gradient follows the element that it fills") {
    // The same gradient on the same shape must paint the same pixels wherever the shape is.
    val gradient = LinearGradient(Pos(0, 0), Pos(40, 0), red, blue)

    val atOrigin = Rectangle(40, 10, Pos.Origin, fillWith(gradient), line = None).toBitmap()
    val faraway  = Rectangle(40, 10, Pos(300, 210), fillWith(gradient), line = None).toBitmap()

    assertEquals(faraway.pixels().colors, atOrigin.pixels().colors)
  }

  test("#9 a gradient's points stay in its element's frame through a turn, a stretch and a move") {
    // Turned a quarter counterclockwise, the bar stands upright with its left end, the red one,
    // at the bottom. Points in world coordinates would run across the 10 pixel wide bar instead,
    // and leave it red from top to bottom.
    val bar = Rectangle(40, 10, Pos.Origin, fillWith(redToBlue), line = None)
      .rotateAround(90, Pos.Origin)
      .scaleBy(1, 2)
      .moveBy(200, 150)
    val bitmap = bar.toBitmap()

    assertEquals((bitmap.pixels().width, bitmap.pixels().height), (10, 80))
    val top    = bitmap.getColor(5, 2)
    val middle = bitmap.getColor(5, 40)
    val bottom = bitmap.getColor(5, 77)
    assert(isMostlyBlue(top), s"the top should be blue, was $top")
    assert(isBlended(middle), s"the middle should be blended, was $middle")
    assert(isMostlyRed(bottom), s"the bottom should be red, was $bottom")
  }

  /** The corner, in `shape`'s local frame, that its gradient's points are measured from, as the
    * renderers measure them.
    */
  private def gradientOrigin(element: Element): Pos = element match
    case shape: Shape =>
      smile.infrastructure.DrawPlan.paintOrigin(shape.shape.localBox, shape.stroke)
    case other => fail(s"$other is not a shape")

  test("#9 a partial Arc anchors its gradient to its own box, not the ellipse box") {
    val quarterArc = Arc(
      pos = Pos(70, 40),
      width = 100,
      height = 80,
      startAngle = 0,
      arcAngle = 90,
      fill = fillWith(red),
      line = None
    )

    // The gradient's origin is the corner of the arc's own local box, which the arc's affine
    // takes to the corner of its bounds. The ellipse box's corner sits a whole radius away from
    // the visible arc for a quarter sweep. A gradient that spans the bounds and is measured from
    // that corner lands entirely off the shape.
    assertEquals(
      quarterArc.affine(gradientOrigin(quarterArc)),
      quarterArc.bounds.get.upperLeftCorner
    )
    // For the upper right quarter the box's corner lies a radius up from the ellipse's center.
    assertEquals(gradientOrigin(quarterArc), Pos(0, -40))
  }

  test("#9 a rotated Arc's gradient origin rotates with the arc") {
    // A gradient rotates rigidly with the element it fills. Its origin is in the arc's own
    // frame, which the rotation only places differently, so it does not depend on the angle.
    def originOf(rotationAngle: Double) =
      gradientOrigin(
        Arc(
          pos = Pos(70, 40),
          width = 100,
          height = 80,
          startAngle = 0,
          arcAngle = 90,
          fill = fillWith(red),
          line = None
        ).rotateAround(rotationAngle, Pos(70, 40))
      )

    assertEquals(originOf(30), originOf(0))
    assertEquals(originOf(-125), originOf(0))
  }

  test("#9 a stroked Arc's gradient origin keeps the stroke room its boundary reserves") {
    val circle = Arc(
      pos = Pos(10, 10),
      width = 16,
      height = 16,
      startAngle = 0,
      arcAngle = 360,
      fill = None,
      line = Some(StrokeStyle(blue, width = 4))
    )

    assertEquals(gradientOrigin(circle), Pos(-10, -10))
    assertEquals(circle.affine(gradientOrigin(circle)), circle.bounds.get.upperLeftCorner)
  }
