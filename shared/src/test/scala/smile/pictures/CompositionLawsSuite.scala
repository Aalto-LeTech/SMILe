package smile.pictures

import smile.modeling.*
import smile.testing.{Cases, InkAssertions}

/** The laws of the composition DSL, for every side, every alignment, and a spread of gaps, on boxes
  * of different sizes that sit away from the origin and from each other, since a layout can be
  * right for equal squares at the origin and wrong everywhere else.
  */
class CompositionLawsSuite extends munit.FunSuite with InkAssertions:

  // Moves put translations on the 2^-20 pixel grid, half a step of which is 4.8e-7.
  private val Exact = 1e-6

  private val base  = Rectangle(30, 12, Pos(-4, 6), fillWith(Cases.red), None)
  private val added = Triangle(22, 14, 17, Pos(40, -25), fillWith(Cases.blue), Cases.stroke(3))
  private val gaps  = Seq(0.0, 3.5, -2.0)

  private val sides: Seq[Side] = Side.values.toSeq
  private val aligns           = Align.Generic.values.toSeq

  /** What `addTo` promises, spelled out from the two boundaries alone. */
  private def expectedBoundary(
      existing: Bounds,
      content: Bounds,
      side: Side,
      gap: Double,
      align: Align.Generic
  ): Bounds =
    val (width, height)                                       = (content.width, content.height)
    def along(from: Double, to: Double, size: Double): Double = align match
      case Align.Generic.TopOrLeft     => from
      case Align.Generic.EitherMiddle  => (from + to) / 2 - size / 2
      case Align.Generic.BottomOrRight => to - size
    val corner = side match
      case Side.Right =>
        Pos(
          existing.lowerRightCorner.x + gap,
          along(existing.upperLeftCorner.y, existing.lowerRightCorner.y, height)
        )
      case Side.Left =>
        Pos(
          existing.upperLeftCorner.x - gap - width,
          along(existing.upperLeftCorner.y, existing.lowerRightCorner.y, height)
        )
      case Side.Bottom =>
        Pos(
          along(existing.upperLeftCorner.x, existing.lowerRightCorner.x, width),
          existing.lowerRightCorner.y + gap
        )
      case Side.Top =>
        Pos(
          along(existing.upperLeftCorner.x, existing.lowerRightCorner.x, width),
          existing.upperLeftCorner.y - gap - height
        )
    Bounds(corner, corner + (width, height))

  /** The parts of a composition, front first. A picture gives its children, and any other element
    * gives itself.
    */
  private def partsOf(e: Element): Seq[Element] = e match
    case picture: Picture => picture.children
    case other            => Vector(other)

  /** The box around a composition's parts, worked out again from them. */
  private def recomputedBoundary(e: Element): Bounds =
    Element.union(partsOf(e).iterator.map(_.bounds)).get

  /** `obtained` is `expected` moved, nothing else changed. */
  private def assertMoved(obtained: Element, expected: Element, clue: => String)(using
      munit.Location
  ): Unit =
    val offset = boundsOf(obtained).upperLeftCorner - boundsOf(expected).upperLeftCorner
    assertClose(boundsOf(obtained), boundsOf(expected).moveBy(offset.x, offset.y), Exact, clue)
    assertClose(obtained.anchor, expected.anchor + (offset.x, offset.y), Exact, clue)
    assertEquals(
      (obtained.affine.a, obtained.affine.b, obtained.affine.c, obtained.affine.d),
      (expected.affine.a, expected.affine.b, expected.affine.c, expected.affine.d),
      clue
    )

  test(
    "addTo puts content a gap away on the side it names, aligned as asked, and moves nothing else"
  ) {
    for side <- sides; align <- aligns; gap <- gaps do
      val picture = base.addTo(side, added, gap, align)
      val clue    = s"$side, $align, gap $gap"

      assertEquals(partsOf(picture).size, 2, clue)
      // The new content comes to the front, and the existing element stays what it was.
      assertEquals(partsOf(picture).last, base, clue)
      assertClose(
        partsOf(picture).head.bounds.get,
        expectedBoundary(base.bounds.get, added.bounds.get, side, gap, align),
        Exact,
        clue
      )
      // The same triangle, stroke and all, moved.
      assertMoved(partsOf(picture).head, added, clue)
      assertClose(
        picture.bounds.get,
        recomputedBoundary(picture),
        Exact,
        s"$clue: the cached boundary"
      )
  }

  test("the four named additions are addTo with their side") {
    for gap <- gaps do
      for align <- Align.Vertical.values do
        assertEquals(
          partsOf(base.addToRight(added, gap, align)),
          partsOf(base.addTo(Side.Right, added, gap, align.toGeneric))
        )
        assertEquals(
          partsOf(base.addToLeft(added, gap, align)),
          partsOf(base.addTo(Side.Left, added, gap, align.toGeneric))
        )
      for align <- Align.Horizontal.values do
        assertEquals(
          partsOf(base.addToTop(added, gap, align)),
          partsOf(base.addTo(Side.Top, added, gap, align.toGeneric))
        )
        assertEquals(
          partsOf(base.addToBottom(added, gap, align)),
          partsOf(base.addTo(Side.Bottom, added, gap, align.toGeneric))
        )
  }

  test("content added with no gap touches the picture without overlapping it") {
    for side <- sides; align <- aligns do
      val picture         = base.addTo(side, added, gap = 0, align)
      val content         = partsOf(picture).head.bounds.get
      val (width, height) = (
        base.bounds.get.width + content.width,
        base.bounds.get.height + content.height
      )
      side match
        case Side.Left | Side.Right =>
          assertEqualsDouble(picture.bounds.get.width, width, Exact, s"$side, $align")
        case Side.Top | Side.Bottom =>
          assertEqualsDouble(picture.bounds.get.height, height, Exact, s"$side, $align")
  }

  test("additions chain: each one lands next to everything added before it (#2)") {
    val third = Circle(Pos(-70, 80), 9, fillWith(Cases.green), None)
    for side <- sides; align <- aligns do
      val first  = base.addTo(side, added, 2, align)
      val second = first.addTo(side, third, 2, align)
      val nextTo = partsOf(second).head.bounds.get
      val clue   = s"$side, $align"

      assertEquals(partsOf(second).tail, partsOf(first), s"$clue: earlier elements stay put")
      assertClose(
        nextTo,
        expectedBoundary(first.bounds.get, third.bounds.get, side, 2, align),
        Exact,
        clue
      )
      assertClose(second.bounds.get, recomputedBoundary(second), Exact, clue)

      // Adding across the direction of the row leaves the row alone as well.
      val across = if side == Side.Left || side == Side.Right then Side.Top else Side.Left
      val capped = second.addTo(across, third, 0, align)
      assertEquals(partsOf(capped).tail, partsOf(second), s"$clue, then $across")
  }

  test("replicateTo lays out the replicas the way chained additions do, on every side") {
    for side <- sides; align <- aligns; gap <- gaps do
      val replicated = added.replicateTo(side, 3, gap, align)
      val chained    =
        (1 to 3).foldLeft(added: Element)((picture, _) => picture.addTo(side, added, gap, align))
      val clue = s"$side, $align, gap $gap"

      assertEquals(partsOf(replicated).size, 4, clue)
      assertEquals(partsOf(replicated), partsOf(chained), clue)
      assertClose(replicated.bounds.get, chained.bounds.get, Exact, clue)
      assertClose(replicated.bounds.get, recomputedBoundary(replicated), Exact, clue)

      // Equal replicas sit a constant step apart.
      val positions = partsOf(replicated).map(_.anchor)
      val steps     = positions.zip(positions.tail).map((one, next) => one - next)
      for step <- steps do assertClose(step, steps.head, Exact, clue)
  }

  test("replicateDownwards and replicateRightwards are replicateTo with their side") {
    val shrink: Element => Element = _.scaleBy(0.5)
    assertEquals(
      partsOf(added.replicateDownwards(3, 2, Align.Horizontal.Right, shrink)),
      partsOf(added.replicateTo(Side.Bottom, 3, 2, Align.Generic.BottomOrRight, shrink))
    )
    assertEquals(
      partsOf(added.replicateRightwards(3, 2, Align.Vertical.Top, shrink)),
      partsOf(added.replicateTo(Side.Right, 3, 2, Align.Generic.TopOrLeft, shrink))
    )
    // Each replica is the transformation of the one before it.
    val widths =
      partsOf(added.replicateRightwards(3, 0, transformer = shrink))
        .map(_.bounds.get.width)
    for (narrower, wider) <- widths.zip(widths.tail) do assert(narrower < wider, s"$widths")
  }

  test("alternateWith cycles through the picture and its alternatives, on every side") {
    val alternatives =
      Seq[Element](added, Circle(Pos(9, 9), 7, fillWith(Cases.green), None))
    for side <- sides; align <- aligns do
      val alternated = base.alternateWith(alternatives, side, 5, 1.5, align)
      val order      = Seq(alternatives(0), alternatives(1), base, alternatives(0), alternatives(1))
      val chained    =
        order.foldLeft(base: Element)((picture, next) => picture.addTo(side, next, 1.5, align))

      assertEquals(partsOf(alternated), partsOf(chained), s"$side, $align")
      assertClose(alternated.bounds.get, recomputedBoundary(alternated), Exact, s"$side, $align")

    assertEquals(
      partsOf(base.alternateRightwardsWith(alternatives, 4, 1, Align.Vertical.Bottom)),
      partsOf(base.alternateWith(alternatives, Side.Right, 4, 1, Align.Generic.BottomOrRight))
    )
    assertEquals(
      partsOf(base.alternateDownwardsWith(alternatives, 4, 1, Align.Horizontal.Middle)),
      partsOf(base.alternateWith(alternatives, Side.Bottom, 4, 1, Align.Generic.EitherMiddle))
    )
  }

  test("negative counts are refused with the count in the message") {
    val replicas = intercept[IllegalArgumentException](base.replicateRightwards(-1))
    assert(replicas.getMessage.contains("-1"), replicas)
    val alternations =
      intercept[IllegalArgumentException](base.alternateRightwardsWith(Seq(added), -2))
    assert(alternations.getMessage.contains("-2"), alternations)
  }

  test("addAt places content by its center or its corner, one piece or many") {
    val byCenter = base.addAt(added, 50, -40, PositionType.Center)
    assertClose(partsOf(byCenter).head.bounds.get.center, Pos(50, -40), Exact, "by center")
    val byCorner = base.addAt(added, 50, -40, PositionType.UpperLeftCorner)
    assertClose(partsOf(byCorner).head.bounds.get.upperLeftCorner, Pos(50, -40), Exact, "by corner")

    assertEquals(
      partsOf(base.addAt(Seq((added, 50.0, -40.0)), PositionType.Center)),
      partsOf(byCenter)
    )
    val several = base.addAt(
      Seq((added, 50.0, -40.0), (base, -60.0, 70.0)),
      PositionType.UpperLeftCorner
    )
    assertEquals(partsOf(several).size, 3)
    assertClose(
      partsOf(several)(0).bounds.get.upperLeftCorner,
      Pos(50, -40),
      Exact,
      "first of several"
    )
    assertClose(
      partsOf(several)(1).bounds.get.upperLeftCorner,
      Pos(-60, 70),
      Exact,
      "second of several"
    )
    assertEquals(partsOf(several)(2), base)
    assertClose(several.bounds.get, recomputedBoundary(several), Exact, "several")
  }

  test("addToFront and addToBack stack the very elements they were given") {
    val front = base.addToFront(added)
    val back  = base.addToBack(added)

    assertEquals(partsOf(front), Seq(added, base))
    assertEquals(partsOf(back), Seq(base, added))
    assertClose(front.bounds.get, recomputedBoundary(front), Exact, "front")
    assertClose(back.bounds.get, recomputedBoundary(back), Exact, "back")

    // A whole picture keeps its own order inside the stack.
    val pair = Picture(added, base)
    assertEquals(partsOf(base.addToBack(pair)), Seq(base, added, base))
  }

  test("the composition DSL's default gap, alignments, and position type are plain arguments") {
    assertEquals(
      partsOf(base.addToRight(added)),
      partsOf(base.addToRight(added, 0, Align.Vertical.Middle))
    )
    assertEquals(
      partsOf(base.addToTop(added)),
      partsOf(base.addToTop(added, 0, Align.Horizontal.Left))
    )
    assertEquals(
      partsOf(base.replicateDownwards(2)),
      partsOf(base.replicateDownwards(2, 0, Align.Horizontal.Left))
    )
    assertClose(
      partsOf(base.addAt(added, 10, 20)).head.bounds.get.center,
      Pos(10, 20),
      Exact,
      "addAt"
    )
  }

  test("a place onto a background turned by 30 degrees is the box around the turned box") {
    val background = Rectangle(100, 100, Pos(0, 0), fillWith(Cases.red), None).rotateBy(30)
    val dot        = Circle(Pos(0, 0), 5, fillWith(Cases.blue), None)
    val side       = 100 * (math.cos(math.toRadians(30)) + math.sin(math.toRadians(30)))

    val placed = background.place(dot, 0, 0)
    assertEqualsDouble(placed.width, side, Exact)
    assertEqualsDouble(placed.height, side, Exact)

    // Another turn takes the box around that box, so the side grows once more.
    assertEqualsDouble(placed.rotateBy(30).place(dot, 0, 0).width, side * 1.3660254, 1e-3)
  }

  test("a gap that is not finite is refused, and named as a gap") {
    for gap <- Seq(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      val calls = Seq[() => Any](
        () => base.addTo(Side.Right, added, gap, Align.Generic.TopOrLeft),
        () => base.addToRight(added, gap),
        () => base.alternateWith(Seq(added), Side.Bottom, 3, gap, Align.Generic.TopOrLeft),
        () => base.replicateTo(Side.Right, 2, gap, Align.Generic.TopOrLeft)
      )
      for call <- calls do
        val message = intercept[IllegalArgumentException](call()).getMessage
        assert(message.contains("A gap must be finite"), message)
  }

  test("the alignment helpers of Bounds place a box at an edge or in the middle") {
    val outer = Bounds(Pos(10, 20), Pos(50, 100))
    val inner = Bounds(Pos(-3, -3), Pos(7, 17))

    assertEquals(outer.horizontalPositionFor(Align.Horizontal.Left, inner), 10.0)
    assertEquals(outer.horizontalPositionFor(Align.Horizontal.Middle, inner), 25.0)
    assertEquals(outer.horizontalPositionFor(Align.Horizontal.Right, inner), 40.0)
    assertEquals(outer.verticalPositionFor(Align.Vertical.Top, inner), 20.0)
    assertEquals(outer.verticalPositionFor(Align.Vertical.Middle, inner), 50.0)
    assertEquals(outer.verticalPositionFor(Align.Vertical.Bottom, inner), 80.0)

    for align <- Align.Generic.values do
      assertEquals(align.toHorizontal.toGeneric, align)
      assertEquals(align.toVertical.toGeneric, align)
  }
