package smile.pictures

import smile.colors.Color
import smile.modeling.*

/** Covers the picture-composition DSL: addAt/addTo*, stacking order, and replication. Guards
  * against regressions of the closed issue #2, where adding on top of a row broke the row's
  * positions.
  */
class PictureCompositionSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  private def square(side: Double, color: Color): Element =
    Rectangle(side, side, Pos.Origin, fillWith(color), line = None)

  private def addedIn(picture: Element): Element =
    picture.elements.find(_.anchor != Pos.Origin).get

  test("addToRight places content edge to edge with a gap, centered vertically") {
    val picture = square(10, red).addToRight(square(4, blue), gap = 3)

    assertEquals(picture.elements.size, 2)
    assertEquals(addedIn(picture).bounds.get, Bounds(Pos(8, -2), Pos(12, 2)))
  }

  test("addToLeft, addToTop, and addToBottom place content on the requested side") {
    val base  = square(10, red)
    val added = square(4, blue)

    assertEquals(
      addedIn(base.addToLeft(added, gap = 3)).bounds.get,
      Bounds(Pos(-12, -2), Pos(-8, 2))
    )
    assertEquals(
      addedIn(base.addToTop(added, gap = 3)).bounds.get,
      Bounds(Pos(-5, -12), Pos(-1, -8))
    )
    assertEquals(
      addedIn(base.addToBottom(added, gap = 3)).bounds.get,
      Bounds(Pos(-5, 8), Pos(-1, 12))
    )

    // Squares at the origin with the default alignment say nothing about the other
    // alignments, or about content that starts somewhere else. An oblong base away from the
    // origin, x from 20 to 50 and y from -6 to 6, and content that starts far off:
    val oblong  = Rectangle(30, 12, Pos(35, 0), fillWith(red), line = None)
    val faraway = Rectangle(8, 4, Pos(-70, 90), fillWith(blue), line = None)
    def placed(picture: Element): Bounds = picture.elements.head.bounds.get

    assertEquals(
      placed(oblong.addToLeft(faraway, gap = 3, Align.Vertical.Top)),
      Bounds(Pos(9, -6), Pos(17, -2))
    )
    assertEquals(
      placed(oblong.addToRight(faraway, gap = 3, Align.Vertical.Bottom)),
      Bounds(Pos(53, 2), Pos(61, 6))
    )
    assertEquals(
      placed(oblong.addToTop(faraway, gap = 3, Align.Horizontal.Middle)),
      Bounds(Pos(31, -13), Pos(39, -9))
    )
    assertEquals(
      placed(oblong.addToTop(faraway, gap = 3, Align.Horizontal.Right)),
      Bounds(Pos(42, -13), Pos(50, -9))
    )
    assertEquals(
      placed(oblong.addToBottom(faraway, gap = 3, Align.Horizontal.Right)),
      Bounds(Pos(42, 9), Pos(50, 13))
    )
    val both = oblong.addToBottom(faraway, gap = 3, Align.Horizontal.Right)
    assertEquals(both.elements.last, oblong)
    assertEquals(both.bounds.get, Bounds(Pos(20, -6), Pos(50, 13)))
  }

  test("addAt positions content by the requested corner or center") {
    val base  = square(10, red)
    val added = square(4, blue)

    val byCorner = base.addAt(added, 20, 30, PositionType.UpperLeftCorner)
    assertEquals(addedIn(byCorner).bounds.get, Bounds(Pos(20, 30), Pos(24, 34)))

    val byCenter = base.addAt(added, 20, 30, PositionType.Center)
    assertEquals(addedIn(byCenter).anchor, Pos(20, 30))
  }

  test("addToFront and addToBack control element stacking order") {
    val base  = square(10, red)
    val small = square(4, blue)

    assertEquals(base.addToFront(small).elements.head.bounds.get.width, 4.0)
    assertEquals(base.addToBack(small).elements.last.bounds.get.width, 4.0)
    // The elements are the same instances as given, in order, front first.
    assertEquals(base.addToFront(small).elements, Seq(small, base))
    assertEquals(base.addToBack(small).elements, Seq(base, small))
  }

  test("adding on top of a row keeps the row's element positions (#2)") {
    val row          = square(10, red).addToRight(square(10, blue)).addToRight(square(10, red))
    val rowPositions = row.elements.map(_.anchor)

    val stacked = row.addToTop(square(4, blue))

    assertEquals(stacked.elements.size, 4)
    val stackedPositions = stacked.elements.map(_.anchor)
    assert(
      rowPositions.forall(stackedPositions.contains),
      s"row positions $rowPositions changed after addToTop: $stackedPositions"
    )
  }

  test("replicateRightwards lays out evenly spaced copies") {
    val picture = square(10, red).replicateRightwards(2, gap = 5)

    assertEquals(picture.elements.size, 3)
    assertEquals(picture.elements.map(_.anchor.x).sorted, Seq(0.0, 15.0, 30.0))
    assert(picture.elements.forall(_.anchor.y == 0.0))
  }

  /** The composition loops accumulate elements and the combined boundary in a single pass. These
    * tests pin the result of a whole run against the same run written out as single additions,
    * element by element.
    */
  private def assertSameComposition(actual: Element, expected: Element)(using
      munit.Location
  ): Unit =
    assertEquals(actual.elements.map(_.anchor), expected.elements.map(_.anchor))
    assertEquals(actual.elements.map(_.bounds.get), expected.elements.map(_.bounds.get))
    assertEquals(actual.bounds.get, expected.bounds.get)

  test("alternateRightwardsWith matches a hand-built sequence of addToRight calls") {
    val base         = square(10, red)
    val firstAlt     = square(4, blue)
    val secondAlt    = square(6, red)
    val alternatives = Seq(firstAlt, secondAlt)

    // Five alternations cycle through first, second, base, first, second.
    val alternated = base.alternateRightwardsWith(alternatives, 5, gap = 3)

    val byHand = base
      .addToRight(firstAlt, gap = 3)
      .addToRight(secondAlt, gap = 3)
      .addToRight(base, gap = 3)
      .addToRight(firstAlt, gap = 3)
      .addToRight(secondAlt, gap = 3)

    assertEquals(alternated.elements.size, 6)
    assertSameComposition(alternated, byHand)
  }

  test("alternateDownwardsWith matches a hand-built sequence of addToBottom calls") {
    val base      = square(10, red)
    val alternate = Rectangle(8, 4, Pos.Origin, fillWith(blue), line = None)

    val alternated =
      base.alternateDownwardsWith(Seq(alternate), 3, gap = 2, Align.Horizontal.Right)

    val byHand = base
      .addToBottom(alternate, gap = 2, Align.Horizontal.Right)
      .addToBottom(base, gap = 2, Align.Horizontal.Right)
      .addToBottom(alternate, gap = 2, Align.Horizontal.Right)

    assertEquals(alternated.elements.size, 4)
    assertSameComposition(alternated, byHand)
  }

  test("replicateTo matches a hand-built sequence of addTo calls, transformer included") {
    val base                       = square(10, red)
    val shrink: Element => Element = _.scaleBy(0.5)

    val replicated =
      base.replicateTo(Side.Left, 3, 4, Align.Vertical.Top.toGeneric, shrink)

    val firstReplica  = shrink(base)
    val secondReplica = shrink(firstReplica)
    val thirdReplica  = shrink(secondReplica)

    val byHand = base
      .addToLeft(firstReplica, gap = 4, Align.Vertical.Top)
      .addToLeft(secondReplica, gap = 4, Align.Vertical.Top)
      .addToLeft(thirdReplica, gap = 4, Align.Vertical.Top)

    assertEquals(replicated.elements.size, 4)
    assertSameComposition(replicated, byHand)
  }

  test("zero alternations and zero replicas leave the content untouched") {
    val base = square(10, red)

    assertEquals(base.alternateRightwardsWith(Seq(square(4, blue)), 0).elements.size, 1)
    assertEquals(base.replicateRightwards(0).elements.size, 1)
    assertEquals(base.replicateRightwards(0).bounds.get, base.bounds.get)

    val cropped = base.crop(base.bounds.get)
    assertEquals(cropped.replicateRightwards(0), cropped)
  }

  test("the empty picture is the identity for layout, with no gap next to it") {
    val replicated = Empty.replicateRightwards(2, gap = 5)
    assertEquals(replicated, Empty)
    assertEquals(replicated.bounds, None)

    val alternated = Empty.alternateRightwardsWith(Seq(square(4, blue)), 1, gap = 5)
    assertEquals(alternated, square(4, blue))
    assertEquals(alternated.bounds, Some(Bounds(Pos(-2, -2), Pos(2, 2))))
  }
