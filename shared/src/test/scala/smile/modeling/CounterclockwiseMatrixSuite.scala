package smile.modeling

import smile.colors.Color
import smile.pictures.*
import smile.testing.{Cases, InkAssertions}

/** Every public angle in the library is positive counterclockwise as seen on screen, where the y
  * axis points down. One row per angle. Each is checked at 90 degrees, which takes the exact path,
  * and at 30, which takes the trigonometric one. Each check follows a marked part of the element,
  * which a clockwise turn or a mirror image moves elsewhere even when the center lands in the right
  * place.
  */
class CounterclockwiseMatrixSuite extends munit.FunSuite with InkAssertions:

  private val red  = Cases.red
  private val blue = Cases.blue

  /** Three o'clock, at radius 20, turned counterclockwise by `angle` degrees. */
  private def counterclockwiseFromThree(angle: Double): Pos =
    Pos(20 * math.cos(math.toRadians(angle)), -20 * math.sin(math.toRadians(angle)))

  private def isInked(element: Element): Pos => Boolean =
    val bitmap = onInkCanvas(element)
    spot =>
      bitmap
        .getColor(
          (spot.x - inkCanvas.upperLeftCorner.x).floor.toInt,
          (spot.y - inkCanvas.upperLeftCorner.y).floor.toInt
        )
        .opacity > 127

  test("twelve o'clock is a quarter turn counterclockwise of three o'clock") {
    assertClose(counterclockwiseFromThree(90), Pos(0, -20), 1e-9, "the fixture's own convention")
  }

  for angle <- Seq(90.0, 30.0, -30.0, 200.0) do
    test(s"positions turn counterclockwise by $angle degrees") {
      val expected = counterclockwiseFromThree(angle)
      assertClose(
        Pos(20, 0).rotateAround(angle, Pos.Origin),
        expected,
        1e-9,
        "Pos.rotateAroundOrigin"
      )
      assertClose(Pos(20, 0).rotateAround(angle, Pos.Origin), expected, 1e-9, "Pos.rotateAround")
      assertClose(Pos(20, 0).rotateAroundOrigin(angle), expected, 1e-9, "Pos.rotateAroundOrigin")
      assertClose(
        Pos(27, -3).rotateAround(angle, Pos(7, -3)),
        expected + Pos(7, -3),
        1e-9,
        "Pos.rotateAround a pivot"
      )
    }

    test(s"a polygon's vertices turn counterclockwise by $angle degrees") {
      // A needle from the center to three o'clock. Its tip is the marked part.
      val needle = Polygon(Pos.Origin, Seq(Pos(0, -1), Pos(20, 0), Pos(0, 1)), fillWith(red), None)
      // The tip in the needle's local frame, where the turned needle's affine takes it.
      val tip    = Pos(10, 0)
      val turned = needle.rotateAround(angle, Pos.Origin)
      // The affine's translation lies on the 2^-20 pixel grid.
      assertClose(turned.affine(tip), counterclockwiseFromThree(angle), 1e-6, "rotateAround")
    }

    test(s"an arc turned by $angle degrees turns counterclockwise") {
      // A thin ellipse reaching from the center to three o'clock and beyond.
      val inked = isInked(
        Arc(Pos.Origin, 60, 4, 0, 360, fillWith(red), None).rotateAround(angle, Pos.Origin)
      )
      assert(
        inked(counterclockwiseFromThree(angle)),
        "the long axis should point where the angle says"
      )
      // A quarter turn's mirror image is the same ellipse.
      if angle % 90 != 0 then
        assert(!inked(counterclockwiseFromThree(-angle)), "and not its mirror image")
    }

    test(s"an arc's start angle runs counterclockwise from three o'clock: $angle degrees") {
      // A sliver of a circle, 10 degrees wide, starting at the angle.
      val inked =
        isInked(Arc(Pos.Origin, 56, 56, angle, 10, None, Cases.stroke(8, StrokeStyle.Cap.Butt)))
      val middle = counterclockwiseFromThree(angle + 5) * 1.4
      val mirror = counterclockwiseFromThree(-(angle + 5)) * 1.4
      assert(inked(middle), s"the sliver should lie at $middle")
      assert(!inked(mirror), s"and not at its mirror image $mirror")
    }

    test(
      s"an arc's sweep runs counterclockwise for a positive extent: from zero by $angle degrees"
    ) {
      val inked =
        isInked(Arc(Pos.Origin, 56, 56, 0, angle, None, Cases.stroke(8, StrokeStyle.Cap.Butt)))
      val middle = counterclockwiseFromThree(angle / 2) * 1.4
      val mirror = counterclockwiseFromThree(-angle / 2) * 1.4
      assert(inked(middle), s"the sweep should pass $middle")
      // More than half a turn covers its own mirror image.
      if angle.abs < 180 then assert(!inked(mirror), s"and not $mirror")
    }

    test(s"a text's rotation angle turns it counterclockwise by $angle degrees") {
      // A wide, flat text, inked along the direction it reads in. A quarter turn's mirror
      // image lies along the same line, and TextLayoutSuite tells those apart by the glyphs.
      def flat(rotation: Double) = Text(
        pos = Pos.Origin,
        customBounds = Some(Bounds(Pos(-30, -4), Pos(30, 4))),
        content = "HHHHHH",
        font = "SansSerif",
        size = 12,
        weight = 700,
        fill = fillWith(red),
        line = None,
        rotationAngle = rotation
      )

      def inkAround(text: Element, spot: Pos): Int =
        val bitmap = onInkCanvas(text)
        val (x, y) = (
          (spot.x - inkCanvas.upperLeftCorner.x).floor.toInt,
          (spot.y - inkCanvas.upperLeftCorner.y).floor.toInt
        )
        (for dx <- -4 to 4; dy <- -4 to 4
        yield bitmap.getColor(x + dx, y + dy)).count(_.opacity > 127)

      for (how, text) <- Seq(
          "built"  -> flat(angle),
          "turned" -> flat(0).rotateAround(angle, Pos.Origin)
        )
      do
        assert(
          inkAround(text, counterclockwiseFromThree(angle)) > 0,
          s"$how at $angle: ink along the angle"
        )
        if angle % 90 != 0 then
          assertEquals(
            inkAround(text, counterclockwiseFromThree(-angle)),
            0,
            s"$how at $angle: mirror image"
          )
        // Up to the 2^-20 grid the text's translation lies on.
        assertClose(text.bounds.get.center, Pos.Origin, 1e-6, how)
        assertInkWithinBoundary(s"$how at $angle", text)
    }

  test(
    "a bitmap's pixels turn counterclockwise with its position, at a right angle and at an oblique one"
  ) {
    // Red on the left, blue on the right, lying to the right of the origin.
    val bitmap = Cases.twoColorBitmap.moveTo(Pos(20, 0))

    for angle <- Seq(90.0, 30.0, -30.0, 200.0) do
      val turned = bitmap.rotateAround(angle, Pos.Origin)
      assertClose(turned.anchor, counterclockwiseFromThree(angle), 1.0, s"position at $angle")

      // The red half lies nearer the origin than the blue half does, as before the turn.
      val picture                   = onInkCanvas(turned)
      def colorAt(spot: Pos): Color = picture.getColor(
        (spot.x - inkCanvas.upperLeftCorner.x).floor.toInt,
        (spot.y - inkCanvas.upperLeftCorner.y).floor.toInt
      )
      val (inner, outer) =
        (
          colorAt(counterclockwiseFromThree(angle) * 0.9),
          colorAt(counterclockwiseFromThree(angle) * 1.1)
        )
      assert(inner.red > inner.blue, s"at $angle the inner half should be red, was $inner")
      assert(outer.blue > outer.red, s"at $angle the outer half should be blue, was $outer")
  }

  test("a text turned by its rotation angle stays where it was") {
    val text = Text(
      pos = Pos.Origin,
      customBounds = Some(Bounds(Pos(-30, -4), Pos(30, 4))),
      content = "HHHHHH",
      font = "SansSerif",
      size = 12,
      weight = 700,
      fill = fillWith(blue),
      line = None,
      rotationAngle = 30
    )
    assertClose(boundsOf(text).center, Pos.Origin, 1e-6, "the turned text stays where it was")
  }

  test("a mask group and a picture turn their parts counterclockwise together") {
    // A bar from the center out to three o'clock.
    val needle =
      Polygon(Pos.Origin, Seq(Pos(0, -3), Pos(24, -3), Pos(24, 3), Pos(0, 3)), fillWith(red), None)
    for angle <- Seq(90.0, 30.0) do
      val group =
        MaskGroup(Rectangle(60, 60, Pos.Origin, fillWith(blue), None), needle)
      assert(
        isInked(group.rotateAround(angle, Pos.Origin))(counterclockwiseFromThree(angle)),
        s"mask group at $angle"
      )
      assert(
        !isInked(group.rotateAround(angle, Pos.Origin))(counterclockwiseFromThree(-angle)),
        s"mask group at $angle"
      )

      val picture = Picture(needle).rotateAround(angle, Pos.Origin)
      assert(isInked(picture)(counterclockwiseFromThree(angle)), s"picture at $angle")
  }
