package smile.pictures

import smile.modeling.{Bounds, Pos}
import smile.testing.{Cases, InkAssertions}

/** The algebra of the element transforms, checked on every kind of element. The laws hold whatever
  * the element is, so that a new element type, or a rewritten transformation, is held to them
  * without a test of its own.
  *
  * Every element turns and scales around its anchor, and every transform composes into one affine,
  * so the laws are the same for shapes, text, groups and bitmaps. Geometries are compared to a
  * rounding error, except where a bitmap's own operation resamples it onto whole pixels.
  */
class TransformLawsSuite extends munit.FunSuite with InkAssertions:

  // Turns away from right angles round translations to the 2^-20 pixel grid, a step of which is
  // about a millionth of a pixel, so chains of them agree to a few steps.
  private val Exact  = 4e-6
  private val Raster = 1.0

  private def toleranceFor(element: Element): Double =
    if Cases.isRaster(element) then Raster else Exact

  private def assertSameGeometry(
      obtained: Element,
      expected: Element,
      clue: => String,
      tolerance: Double = Exact
  )(using munit.Location): Unit =
    assertClose(boundsOf(obtained), boundsOf(expected), tolerance, clue)
    assertClose(obtained.anchor, expected.anchor, tolerance, clue)

  /** The box `b` scaled by `h` and `v` around `pivot`. */
  private def scaledBox(b: Bounds, h: Double, v: Double, pivot: Pos): Bounds =
    val (from, to) =
      (b.upperLeftCorner.scaleBy(h, v, pivot), b.lowerRightCorner.scaleBy(h, v, pivot))
    Bounds(from.x, from.y, to.x, to.y)

  test("rotateBy(a) is rotateAround(a, anchor)") {
    for element <- Cases.elements; angle <- Cases.angles ++ Cases.seededAngles do
      assertSameGeometry(
        element.value.rotateBy(angle),
        element.value.rotateAround(angle, element.value.anchor),
        s"${element.label} by $angle"
      )
  }

  test("a rotation keeps its pivot fixed: turning an element around its anchor leaves it there") {
    for element <- Cases.elements; angle <- Cases.angles do
      val turned = element.value.rotateBy(angle)
      assertClose(turned.anchor, element.value.anchor, Exact, s"${element.label} by $angle")
  }

  test("an element's anchor travels around a pivot like a point does") {
    for element <- Cases.elements; angle <- Cases.angles; pivot <- Cases.pivots do
      assertClose(
        element.value.rotateAround(angle, pivot).anchor,
        element.value.anchor.rotateAround(angle, pivot),
        Exact,
        s"${element.label} by $angle around $pivot"
      )
  }

  test("two rotations around one pivot add up") {
    val pairs = Seq((30.0, 60.0), (90.0, 90.0), (45.0, -45.0), (137.5, 211.5), (-37.0, 400.0))
    for element <- Cases.elements; (first, second) <- pairs; pivot <- Cases.pivots do
      assertSameGeometry(
        element.value.rotateAround(first, pivot).rotateAround(second, pivot),
        element.value.rotateAround(first + second, pivot),
        s"${element.label} by $first and then $second around $pivot",
        toleranceFor(element.value)
      )
  }

  test("a whole turn changes nothing, and neither does a turn there and back") {
    for element <- Cases.elements; pivot <- Cases.pivots do
      for wholeTurn <- Seq(360.0, -360.0, 720.0) do
        assertEquals(
          element.value.rotateAround(wholeTurn, pivot),
          element.value,
          s"${element.label} by $wholeTurn around $pivot"
        )
      for angle <- Cases.rightAngles do
        assertEquals(
          element.value.rotateAround(angle, pivot).rotateAround(-angle, pivot),
          element.value,
          s"${element.label} by $angle and back around $pivot"
        )
      for angle <- Cases.obliqueAngles do
        assertSameGeometry(
          element.value.rotateAround(angle, pivot).rotateAround(-angle, pivot),
          element.value,
          s"${element.label} by $angle and back around $pivot",
          toleranceFor(element.value)
        )
  }

  test("scaleBy(h, v) scales around the element's own anchor") {
    for
      element <- Cases.elements
      (h, v)  <- Cases.positiveFactorPairs ++ Cases.mirroringFactorPairs
    do
      assertSameGeometry(
        element.value.scaleBy(h, v),
        element.value.scaleAround(h, v, element.value.anchor),
        s"${element.label} by ($h, $v)",
        toleranceFor(element.value)
      )
  }

  test("scaling moves an element's anchor like a point, away from the fixed point") {
    for
      element <- Cases.elements
      (h, v)  <- Cases.positiveFactorPairs ++ Cases.mirroringFactorPairs
      pivot   <- Cases.pivots
    do
      assertClose(
        element.value.scaleAround(h, v, pivot).anchor,
        element.value.anchor.scaleBy(h, v, pivot),
        toleranceFor(element.value),
        s"${element.label} by ($h, $v) around $pivot"
      )
  }

  test("scaling by a factor and then by its inverse restores the element") {
    for element <- Cases.elements; factor <- Cases.uniformFactors; pivot <- Cases.pivots do
      assertSameGeometry(
        element.value.scaleAround(factor, pivot).scaleAround(1 / factor, pivot),
        element.value,
        s"${element.label} by $factor and back around $pivot",
        toleranceFor(element.value)
      )
  }

  test("every element's bounds scale by its factors along the world axes, outlines included") {
    // Strokes scale with their shape, and turned arcs and texts scale along the world axes like
    // everything else, so the box scales exactly and a negative factor mirrors it.
    for
      element <- Cases.elements
      (h, v)  <- Cases.positiveFactorPairs ++ Cases.mirroringFactorPairs
      pivot   <- Cases.pivots
    do
      assertClose(
        boundsOf(element.value.scaleAround(h, v, pivot)),
        scaledBox(boundsOf(element.value), h, v, pivot),
        toleranceFor(element.value),
        s"${element.label} by ($h, $v) around $pivot"
      )
  }

  test("mirroring twice restores the element exactly") {
    val elements = Cases.elements.filterNot(element => Cases.isRaster(element.value))
    for element <- elements do
      for (h, v) <- Seq((-1.0, 1.0), (1.0, -1.0), (-1.0, -1.0)); pivot <- Cases.pivots do
        assertEquals(
          element.value.scaleAround(h, v, pivot).scaleAround(h, v, pivot),
          element.value,
          s"${element.label} by ($h, $v) twice around $pivot"
        )
      assertEquals(element.value.flipHorizontally.flipHorizontally, element.value, element.label)
      assertEquals(element.value.flipVertically.flipVertically, element.value, element.label)
  }

  test("a bitmap mirrored twice draws where it did") {
    val bitmap = Cases.twoColorBitmap
    for (h, v) <- Seq((-1.0, 1.0), (1.0, -1.0), (-1.0, -1.0)); pivot <- Cases.pivots do
      assertSameGeometry(
        bitmap.scaleAround(h, v, pivot).scaleAround(h, v, pivot),
        bitmap,
        s"by ($h, $v) twice around $pivot"
      )
  }

  test("moves add up, the bounds travel rigidly, and moveTo puts the anchor where it says") {
    for element <- Cases.elements do
      val original = element.value
      assertSameGeometry(
        original.moveBy(3, -4).moveBy(-10.5, 6),
        original.moveBy(-7.5, 2),
        element.label
      )
      assertClose(
        boundsOf(original.moveBy(3, -4)),
        boundsOf(original).moveBy(3, -4),
        Exact,
        s"${element.label}: the bounds travel rigidly"
      )
      assertClose(original.moveTo(Pos(40, -30)).anchor, Pos(40, -30), Exact, element.label)
  }

  test("scaleTo gives every element the bounds it was asked for, around its anchor") {
    for
      element         <- Cases.elements
      (width, height) <- Seq((60.0, 60.0), (90.0, 25.0), (17.0, 40.0))
    do
      val scaled = element.value.scaleTo(width, height)
      val box    = boundsOf(scaled)
      val clue   = element.label
      assertEqualsDouble(box.width, width, 1e-6, s"$clue: width")
      assertEqualsDouble(box.height, height, 1e-6, s"$clue: height")
      assertClose(scaled.anchor, element.value.anchor, 1e-6, s"$clue: fixed point")
  }

  test("scaleTo leaves alone an extent that is zero and cannot be scaled") {
    val vertical = Polygon(Pos(3, 4), Seq(Pos(0, -5), Pos(0, 5)), None, None)
    val scaled   = vertical.scaleTo(10, 40)

    assertEquals(scaled.bounds, Some(Bounds(Pos(3, -16), Pos(3, 24))))
  }

  test("a picture transforms like its elements, one by one") {
    val picture = Cases.picture
    val parts   = Seq(
      Cases.polygons.head.value,
      Cases.arcs(1).value,
      Cases.texts.head.value,
      Point(Pos(-25, -12), Cases.green)
    )
    val pivot = Pos(7, -3)

    def union(elements: Seq[Element]): Bounds =
      elements
        .map(boundsOf)
        .reduce: (a, b) =>
          Bounds(
            a.upperLeftCorner.x.min(b.upperLeftCorner.x),
            a.upperLeftCorner.y.min(b.upperLeftCorner.y),
            a.lowerRightCorner.x.max(b.lowerRightCorner.x),
            a.lowerRightCorner.y.max(b.lowerRightCorner.y)
          )

    for (h, v) <- Cases.positiveFactorPairs ++ Cases.mirroringFactorPairs do
      assertClose(
        boundsOf(picture.scaleAround(h, v, pivot)),
        union(parts.map(_.scaleAround(h, v, pivot))),
        Exact,
        s"scaled by ($h, $v) around $pivot"
      )
    for angle <- Cases.angles do
      assertClose(
        boundsOf(picture.rotateAround(angle, pivot)),
        union(parts.map(_.rotateAround(angle, pivot))),
        Exact,
        s"turned by $angle"
      )
  }

  test("a mask group and an animation scale as one thing, around one point") {
    val group = Cases.maskGroup
    assertClose(
      boundsOf(group.scaleBy(2, 2)),
      scaledBox(boundsOf(group), 2, 2, group.anchor),
      Exact,
      "the mask scales around the group's anchor"
    )

    val frames = Seq(
      Rectangle(10, 10, Pos(0, 0), fillWith(Cases.red), None),
      Rectangle(4, 4, Pos(20, 6), fillWith(Cases.blue), None)
    )
    val animation = AnimatedPicture(frames, Seq(100, 100), startTime = 0)
    val enlarged  = animation.scaleBy(2, 2) match
      case scaled: AnimatedPicture => scaled
      case other                   => fail(s"an animation scales into an animation, not $other")
    assertClose(
      boundsOf(enlarged.frameAt(100)).center - boundsOf(enlarged.frameAt(0)).center,
      Pos(40, 12),
      Exact,
      "the frames keep their places relative to each other"
    )
    assertClose(enlarged.anchor, animation.anchor, Exact, "around the animation's anchor")
  }
end TransformLawsSuite
