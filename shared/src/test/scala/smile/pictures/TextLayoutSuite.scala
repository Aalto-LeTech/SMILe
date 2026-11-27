package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos, TextLayout, TextShape}
import smile.testing.TextFixtures.boxedText

/** A text's layout box and bounds on both platforms: centered on the text's position, scaled onto a
  * box with its outline, carried along by scaling, and turned counterclockwise with the glyphs. Its
  * baseline comes from the same measurement as its boxes.
  */
class TextLayoutSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  /** How many pixels of `rotated` disagree, in being inked or not, with `upright` after `upright`
    * has been turned a quarter turn on screen, counterclockwise or clockwise. The glyphs are
    * rasterized anew at the rotated angle, so the two never match pixel for pixel. The right
    * direction leaves a thin fringe of differences and the wrong one a mirrored letter's worth.
    */
  private def quarterTurnMismatches(
      upright: Bitmap,
      rotated: Bitmap,
      counterclockwise: Boolean
  ): Int =
    val (width, height) = (upright.pixels().width, upright.pixels().height)
    assertEquals((rotated.pixels().width, rotated.pixels().height), (height, width))

    def isInked(bitmap: Bitmap, x: Int, y: Int): Boolean = bitmap.getColor(x, y).opacity > 127

    val pixels = for x <- 0 until height; y <- 0 until width yield (x, y)
    pixels.count: (x, y) =>
      // A counterclockwise quarter turn carries the upright pixel (u, v) to (v, width-1-u),
      // and a clockwise one carries it to (height-1-v, u).
      val source =
        if counterclockwise then isInked(upright, width - 1 - y, x)
        else isInked(upright, y, height - 1 - x)
      isInked(rotated, x, y) != source

  /** The layout box of a text where its affine puts it in the world. */
  private def layoutOf(text: Element): Bounds = text match
    case shape: Shape =>
      shape.shape match
        case glyphs: TextShape => Element.mapBox(shape.affine, glyphs.layout.layoutBox)
        case other             => fail(s"$other is not a text")
    case other => fail(s"$other is not a text")

  /** `obtained` is `expected` up to the rounding of the arithmetic that scales a text. */
  private def assertClose(obtained: Bounds, expected: Bounds): Unit =
    for (a, b) <- Seq(
        (obtained.upperLeftCorner.x, expected.upperLeftCorner.x),
        (obtained.upperLeftCorner.y, expected.upperLeftCorner.y),
        (obtained.lowerRightCorner.x, expected.lowerRightCorner.x),
        (obtained.lowerRightCorner.y, expected.lowerRightCorner.y)
      )
    do assertEqualsDouble(a, b, 1e-6, s"$obtained, expected $expected")

  test("a text's layout box is centered on the text's position") {
    // Every backend centers the glyphs on the text's local origin, so a box anchored at the
    // position would hold only their bottom-right quadrant and the renderers would clip the rest.
    val text = Text(Pos(20, 10), None, "Lo", "Dialog", 12, 400, fillWith(red), None)

    assertEquals(layoutOf(text).center, Pos(20, 10))
    // On a box, the text fills it.
    val boxed = Text(
      Pos(20, 10),
      Some(Bounds(Pos(-5, -5), Pos(45, 25))),
      "Lo",
      "Dialog",
      12,
      400,
      fillWith(red),
      None
    )
    assertClose(boxed.bounds.get, Bounds(Pos(-5, -5), Pos(45, 25)))
  }

  test("a stroked text on a box fits the box with its outline, whatever the join") {
    val layoutBox = Bounds(Pos(0, 0), Pos(40, 20))

    def text(line: Option[StrokeStyle]): Element = Text(
      pos = Pos(20, 10),
      customBounds = Some(layoutBox),
      content = "wide",
      font = "Dialog",
      size = 12,
      weight = 400,
      fill = fillWith(red),
      line = line
    )

    // Unstroked, the bounds are the box itself.
    assertClose(text(None).bounds.get, layoutBox)

    // Stroked, the text is scaled onto the box outline and all, since the outline scales with
    // the glyphs, so the box still holds everything it draws. The backends stroke glyphs with
    // round joins, which reach one stroke radius whatever join the style asks for.
    for join <- StrokeStyle.Join.values do
      val stroked = text(Some(StrokeStyle(blue, width = 4, join = join)))
      val box     = stroked.bounds.get
      assertEqualsDouble(box.width, 40, 1e-6, join.toString)
      assertEqualsDouble(box.height, 20, 1e-6, join.toString)
      assertEqualsDouble(box.center.x, 20, 1e-6, join.toString)
      assertEqualsDouble(box.center.y, 10, 1e-6, join.toString)
      // The glyphs' layout box lies inside, by the scaled stroke radius.
      assert(layoutOf(stroked).width < 40, s"$join: ${layoutOf(stroked)}")
  }

  test("scaling text around a point moves its position with the layout box") {
    val text = Text(
      pos = Pos(30, 10),
      customBounds = Some(Bounds(Pos(0, 0), Pos(60, 20))),
      content = "oblong text",
      font = "Dialog",
      size = 12,
      weight = 400,
      fill = fillWith(red),
      line = None
    )

    val scaled = text.scaleAround(2, 2, Pos.Origin)

    assertClose(scaled.bounds.get, Bounds(Pos(0, 0), Pos(120, 40)))
    assertEqualsDouble(scaled.anchor.x, 2 * text.anchor.x, 1e-6)
    assertEqualsDouble(scaled.anchor.y, 2 * text.anchor.y, 1e-6)
  }

  test("text rotated by +90 degrees reads from the bottom up, and by -90 from the top down") {
    // "L" has no symmetry that could hide a mirrored result.
    val upright     = boxedText("L", 0).toBitmap()
    val turnedLeft  = boxedText("L", 90).toBitmap()
    val turnedRight = boxedText("L", -90).toBitmap()
    val area        = upright.pixels().width * upright.pixels().height

    assert(upright.pixels().colors.exists(_.opacity > 127), "the text should paint something")

    def mismatches(rotated: Bitmap, counterclockwise: Boolean): Int =
      quarterTurnMismatches(upright, rotated, counterclockwise)

    // A positive angle is a counterclockwise turn on screen. The baseline runs upward.
    val leftAsCounterclockwise = mismatches(turnedLeft, counterclockwise = true)
    val leftAsClockwise        = mismatches(turnedLeft, counterclockwise = false)
    assert(leftAsCounterclockwise < area / 10, s"$leftAsCounterclockwise of $area pixels differ")
    assert(
      leftAsCounterclockwise * 3 < leftAsClockwise,
      s"+90 should match a counterclockwise turn ($leftAsCounterclockwise differences), " +
        s"not a clockwise one ($leftAsClockwise)"
    )

    // A negative angle turns the other way.
    val rightAsClockwise        = mismatches(turnedRight, counterclockwise = false)
    val rightAsCounterclockwise = mismatches(turnedRight, counterclockwise = true)
    assert(rightAsClockwise < area / 10, s"$rightAsClockwise of $area pixels differ")
    assert(
      rightAsClockwise * 3 < rightAsCounterclockwise,
      s"-90 should match a clockwise turn ($rightAsClockwise differences), " +
        s"not a counterclockwise one ($rightAsCounterclockwise)"
    )
  }

  test("a Text built with an angle matches one rotated to it, bounds and pixels alike") {
    // The constructor's angle and the angle of rotateAround have to mean the same turn, or a text
    // rotated in place would be painted outside the boundary computed for it.
    val built   = boxedText("L", 90)
    val rotated = boxedText("L", 0).rotateBy(90)

    assertEquals(rotated, built)
    val box = built.bounds.get
    assertEqualsDouble(box.width, 20, 1e-9)
    assertEqualsDouble(box.height, 40, 1e-9)
    assertEquals(
      rotated.toBitmap().pixels().colors,
      built.toBitmap().pixels().colors
    )
  }

  test("the baseline drop lies within half the layout box") {
    val layout = TextLayout("Ag", "Serif", 20, 400)
    val drop   = layout.baselineDrop.getOrElse(fail("the baseline drop is not measured"))
    assert(drop.abs <= layout.layoutBox.height / 2, s"$drop in ${layout.layoutBox}")
  }
