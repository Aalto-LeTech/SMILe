package smile.pictures

import smile.colors.Color
import smile.modeling.{Affine, Align, Bounds, Pos, PositionType}
import smile.pictures.{FillStyle, StrokeStyle}

/** The laws of groups, clips, wrap shifts, the empty picture, blank space and composition. */
class GroupLawsSuite extends munit.FunSuite:

  private val red   = Some(FillStyle(Color(255, 0, 0)))
  private val blue  = Some(FillStyle(Color(0, 0, 255)))
  private val black = StrokeStyle(Color(0, 0, 0), 2)

  private val square   = Rectangle(20, Pos(10, 10), red, None)
  private val wide     = Rectangle(60, 30, Pos(100, 50), blue, Some(black))
  private val star     = Star5(40, 15, Pos(-30, 70), red, Some(black))
  private val circle   = Circle(Pos(5, -40), 12, blue, None)
  private val elements = Seq[Element](square, wide, star, circle)

  // Every move rounds its translation to the grid of 2^-20 pixel, so a layout or an outline placed
  // at an arbitrary point lands within half a grid step of it.
  private val Grid = 1.0 / 1048576

  private def leaves(e: Element): Seq[Element] = e match
    case s: Shape           => Seq(s)
    case b: Bitmap          => Seq(b)
    case s: SvgPicture      => Seq(s)
    case p: Picture         => p.children.flatMap(leaves)
    case c: Clip            => leaves(c.child)
    case m: MaskGroup       => m.masks.flatMap((mask, _) => leaves(mask)) ++ leaves(m.masked)
    case a: AnimatedPicture => a.frames.flatMap(leaves)
    case w: Wrap            => leaves(w.child)
    case Empty              => Seq()

  private def boundsOf(e: Element): Bounds = e.bounds.getOrElse(fail(s"$e has no bounds"))

  private def sides(b: Bounds) =
    (b.upperLeftCorner.x, b.upperLeftCorner.y, b.lowerRightCorner.x, b.lowerRightCorner.y)

  private def assertNearBounds(actual: Bounds, expected: Bounds, tolerance: Double = 1e-9): Unit =
    val (l, t, r, b)     = sides(actual)
    val (l2, t2, r2, b2) = sides(expected)
    for (x, y) <- Seq(l -> l2, t -> t2, r -> r2, b -> b2) do
      assert((x - y).abs <= tolerance, s"$actual is not within $tolerance of $expected")

  private def assertNear(actual: Pos, expected: Pos, tolerance: Double = 1e-9): Unit =
    assert(
      (actual.x - expected.x).abs <= tolerance && (actual.y - expected.y).abs <= tolerance,
      s"$actual is not within $tolerance of $expected"
    )

  test("every transform and new anchor of the empty picture is the empty picture") {
    val transformed = Seq[Element](
      Empty.moveBy(3, 4),
      Empty.moveTo(Pos(7, 8)),
      Empty.rotateBy(30),
      Empty.rotateAround(45, Pos(10, 10)),
      Empty.scaleBy(2),
      Empty.scaleBy(2, -3),
      Empty.scaleAround(4, Pos(1, 1)),
      Empty.scaleTo(30, 20),
      Empty.flipHorizontally,
      Empty.flipVertically,
      Empty.flipDiagonally,
      Empty.withAnchor(Pos(5, 5))
    )
    for t <- transformed do assert(t eq Empty, t)
  }

  test("the empty picture has no bounds, is 0x0 and is anchored at the origin") {
    assertEquals(Empty.bounds, None)
    assertEquals((Empty.width, Empty.height), (0.0, 0.0))
    assertEquals(Empty.anchor, Pos.Origin)
    assert(Picture.empty eq Empty)
  }

  test("a picture whose children are all empty is the empty picture") {
    assert(Picture() eq Empty)
    assert(Picture(Empty, Empty) eq Empty)
    assert(Picture(Empty, Picture(Empty)) eq Empty)
  }

  test("the empty picture is the identity for adding to a picture") {
    for e <- elements do
      assert(e.addAt(Empty, 30, 40) eq e)
      assert(e.addAt(Seq((Empty, 1.0, 2.0)), PositionType.UpperLeftCorner) eq e)
      assert(e.addToFront(Empty) eq e)
      assert(e.addToBack(Empty) eq e)
      assert(Empty.addToFront(e) eq e)
      assert(Empty.addToBack(e) eq e)
      assertEquals[Element, Element](
        Empty.addAt(e, 30, 40),
        e.moveBy(30 - e.bounds.get.center.x, 40 - e.bounds.get.center.y)
      )
  }

  test("the empty picture is the identity for side-by-side layout, with no gap next to it") {
    for e <- elements do
      assert(e.addToRight(Empty, 10) eq e)
      assert(e.addToTop(Empty, 10) eq e)
      assert(Empty.addToLeft(e, 10) eq e)
      assert(Empty.addToBottom(e, 10) eq e)
      assertEquals(e.addToRight(Empty, 10).addToRight(square, 10), e.addToRight(square, 10))
      assertEquals(
        e.alternateRightwardsWith(Seq(Empty), 2, gap = 10),
        e.replicateRightwards(1, gap = 10)
      )
  }

  test("placing the empty picture under a clip gives the background unchanged") {
    val spacer = blank(0, 10).moveBy(5, 5)
    for background <- elements :+ spacer do
      assert(background.place(Empty, 3, 4) eq background, background)
      assert(background.place(Empty, 3, 4, PositionType.UpperLeftCorner) eq background)
      assert(background.place(Seq((Empty, 3.0, 4.0)), PositionType.Center) eq background)
  }

  test("placing something onto the empty picture gives the empty picture") {
    for e <- elements do
      assert(Empty.place(e, 3, 4) eq Empty)
      assert(Empty.place(Seq((e, 3.0, 4.0)), PositionType.Center) eq Empty)
  }

  test("placing something onto a background clips it to the background's bounds") {
    val placed = wide.place(circle, 125, 50)
    assertEquals(placed.bounds, wide.bounds)
    assertNear(placed.anchor, boundsOf(wide).center, Grid)
    assertEquals[Element, Element](placed, wide.addAt(circle, 125, 50).crop(boundsOf(wide)))
    assertEquals(
      wide.place(Seq((circle, 125.0, 50.0)), PositionType.Center),
      placed
    )
  }

  test("places in a row clip once, so a long fold of them still renders") {
    // A course exercise folds thousands of stars onto a sky one place at a time.
    val sky   = blank(400, 300).place(Rectangle(400, 300, Pos.Origin, blue, None), 0, 0)
    val dot   = Circle(Pos.Origin, 2, red, None)
    val stars = (0 until 4000).map(i => ((i * 37) % 400 - 200.0, (i * 53) % 300 - 150.0))
    val night = stars.foldLeft(sky)((scene, star) => scene.place(dot, star._1, star._2))
    night match
      case clip: Clip =>
        assert(!leavesOrClips(clip.child).exists(_.isInstanceOf[Clip]), "a clip nests inside")
      case other => fail(s"$other is not a clip")
    assertEquals(night.bounds, sky.bounds)
    assertEquals(night.anchor, boundsOf(sky).center)
    val (x, y) = (stars(1)._1 + 200, stars(1)._2 + 150)
    assertEquals(night.toBitmap().getColor(x.toInt, y.toInt), Color(255, 0, 0))
  }

  test("a place onto a placed picture draws as a place onto its clip") {
    val first  = wide.place(circle, 125, 50)
    val nested = first.addAt(square, 70, 40).crop(boundsOf(first))
    val placed = first.place(square, 70, 40)
    assertEquals(placed.bounds, nested.bounds)
    assertEquals(placed.anchor, nested.anchor)
    assertEquals(
      placed.toBitmap().pixels().argbData.toSeq,
      nested.toBitmap().pixels().argbData.toSeq
    )
    assertEquals(first.place(Seq((square, 70.0, 40.0)), PositionType.Center).bounds, nested.bounds)
  }

  private def leavesOrClips(e: Element): Seq[Element] = e match
    case p: Picture => p.children.flatMap(leavesOrClips)
    case other      => Seq(other)

  test("blank space has its size and draws nothing") {
    val space = blank(30, 20)
    assertEquals((space.width, space.height), (30.0, 20.0))
    assertEquals(leaves(space), Seq())
    assertEquals(space.anchor, Pos.Origin)
  }

  test("blank space takes room in a layout") {
    val laidOut = square.addToRight(blank(30, 20)).addToRight(circle)
    assertEquals(laidOut.width, 20.0 + 30 + 24)
  }

  test("a crop of a turned picture is exactly the requested rectangle") {
    val turned  = Picture(elements*).rotateBy(30)
    val r       = Bounds(-10, -20, 70, 50)
    val cropped = turned.crop(r)
    assertEquals(cropped.bounds, Some(r))
    assertEquals(cropped.anchor, r.center)
  }

  test("a crop with no width is blank space of the requested height") {
    val (x, y)  = (12.0, 7.0)
    val cropped = Picture(elements*).crop(Bounds(x, y, x, y + 10))
    assertEquals((cropped.width, cropped.height), (0.0, 10.0))
    assertEquals(cropped.bounds, Some(Bounds(x, y, x, y + 10)))
    assertEquals(leaves(cropped), Seq())
  }

  test("a crop is anchored in the middle of its rectangle, even of exactly the bounds") {
    val turnedStar = star.rotateBy(30)
    val anchored   = Picture(square, wide).withAnchor(Pos(0, 0))
    for p <- Seq[Element](turnedStar, anchored) do
      val r       = boundsOf(p)
      val cropped = p.crop(r)
      assert(cropped.isInstanceOf[Clip], cropped)
      assertEquals(cropped.bounds, Some(r))
      assertNear(cropped.anchor, r.center, Grid)
  }

  test("a clip's bounds are its rectangle under the clip's transforms") {
    val cropped = wide.crop(Bounds(80, 40, 100, 50))
    assertEquals(cropped.rotateBy(90).bounds, Some(Bounds(85, 35, 95, 55)))
    assertEquals(cropped.moveBy(5, 1).bounds, Some(Bounds(85, 41, 105, 51)))
  }

  test("composing keeps the parts' anchors and is anchored at the middle of its bounds") {
    val anchored = star.withAnchor(Pos(-30, 50))
    val composed = anchored.addToRight(circle, 10)
    composed match
      case p: Picture =>
        assertEquals(p.children.size, 2)
        val placed = p.children.head
        assert(p.children.last eq anchored)
        assertEquals(p.children.last.anchor, Pos(-30, 50))
        val shift = boundsOf(placed).upperLeftCorner - boundsOf(circle).upperLeftCorner
        assertNear(placed.anchor, circle.anchor + shift)
        val gapped = boundsOf(placed).upperLeftCorner.x - boundsOf(anchored).lowerRightCorner.x
        assert((gapped - 10).abs <= Grid, gapped)
      case other => fail(s"Expected a picture, not $other")
    // An anchor is rounded to the grid, too.
    assertNear(composed.anchor, boundsOf(composed).center, Grid)
    val built = Picture(elements*)
    assertNear(built.anchor, boundsOf(built).center, Grid)
  }

  test("composing gives the children of a picture part whose anchor was moved") {
    // A part's anchor is not seen inside a composition, so it is no reason to nest a picture.
    val group    = Picture(square, wide)
    val anchored = group.withAnchor(Pos(1, 2))
    val composed = anchored.addToRight(circle)
    assertEquals[Element, Element](composed, group.addToRight(circle))
    composed match
      case p: Picture => assertEquals(p.children.size, 3)
      case other      => fail(s"Expected a picture, not $other")
    assertEquals(composed.anchor, boundsOf(composed).center)
  }

  test("composing onto a moved picture keeps one level and draws as nesting it") {
    val group  = Picture(square, wide, star).moveBy(10.5, -3.25)
    val onto   = group.addToFront(circle)
    val nested = Picture.of(Vector(circle, group), None)
    onto match
      case p: Picture =>
        assertEquals(p.children.size, 4)
        assert(p.children.forall(!_.isInstanceOf[Picture]))
      case other => fail(s"Expected a picture, not $other")
    assertEquals(onto.bounds, nested.bounds)
    assertEquals(onto.anchor, nested.anchor)
    assertEquals(
      onto.toBitmap().pixels().argbData.toSeq,
      nested.toBitmap().pixels().argbData.toSeq
    )
    // Moving the whole and adding the moved part gives the same picture.
    assertEquals(
      Picture(square, wide, star).addToFront(circle.moveBy(-10.5, 3.25)).moveBy(10.5, -3.25),
      onto
    )
  }

  test("composing with a turned or scaled picture once keeps the exact bounds of its parts") {
    // Only a fold that would nest a second level takes parts into a turned frame, whose inverse is
    // rounded. One composition keeps the turned picture as a part, so its bounds stay exact.
    val group = Picture(square, wide)
    for transformed <- Seq(group.rotateBy(30), group.scaleBy(1.5), group.scaleBy(1.7, 0.3)) do
      for composed <- Seq(
          transformed.addToFront(circle),
          transformed.addToLeft(circle),
          transformed.addToRight(circle),
          transformed.addToTop(circle),
          transformed.addToBottom(circle)
        )
      do
        val parts = composed match
          case p: Picture => p.children
          case other      => fail(s"Expected a picture, not $other")
        assertEquals(composed.bounds, Element.union(parts.iterator.map(_.bounds)), s"$transformed")
        assert(parts.contains(transformed), s"$transformed is not kept as a part")
  }

  test("side-by-side layout puts the parts next to each other, aligned") {
    val laidOut = square.addToRight(wide, gap = 5, align = Align.Vertical.Top)
    val b       = boundsOf(laidOut)
    assertEquals(b.upperLeftCorner, square.bounds.get.upperLeftCorner)
    assertEquals(b.width, 20.0 + 5 + 62)
    val below = square.addToBottom(wide, align = Align.Horizontal.Right)
    assertEquals(boundsOf(below).lowerRightCorner.x, square.bounds.get.lowerRightCorner.x)
    assertEquals(boundsOf(below).height, 20.0 + 32)
  }

  test("replicating and alternating lay out every copy") {
    assertEquals(square.replicateRightwards(3, gap = 2).width, 4 * 20.0 + 3 * 2)
    assertEquals(square.alternateDownwardsWith(Seq(circle), 3).height, 20.0 + 24 + 20 + 24)
    intercept[IllegalArgumentException](square.replicateRightwards(-1))
  }

  test("a picture's transform keeps its children and composes into its affine") {
    val p = Picture(elements*) match
      case p: Picture => p
      case other      => fail(s"Expected a picture, not $other")
    val q = p.rotateBy(30).moveBy(5, 7)
    assertEquals(q.children.size, p.children.size)
    for (a, b) <- q.children.zip(p.children) do assert(a eq b)
    assertEquals(
      q.affine,
      p.affine.andThen(Affine.rotation(30, p.anchor)).andThen(Affine.translation(5, 7))
    )
  }

  test("a turned picture's bounds are the union of its turned leaves' bounds") {
    val p        = Picture(square, Picture(wide, star).rotateBy(15), circle)
    val pivot    = Pos(13, 17)
    val turned   = p.rotateAround(33, pivot)
    val expected = Picture(
      square.rotateAround(33, pivot),
      Picture(wide, star).rotateBy(15).rotateAround(33, pivot),
      circle.rotateAround(33, pivot)
    )
    assertEquals(leaves(turned).size, 4)
    assertNearBounds(boundsOf(turned), boundsOf(expected))
    assertNearBounds(boundsOf(p.rotateBy(90)), boundsOf(p).rotateQuarter(p.anchor))
  }

  extension (b: Bounds)
    private def rotateQuarter(pivot: Pos): Bounds =
      Affine
        .rotation(90, pivot)
        .boxBounds(
          b.upperLeftCorner.x,
          b.upperLeftCorner.y,
          b.lowerRightCorner.x,
          b.lowerRightCorner.y
        )

  test("pictures of every kind are equal after a quarter turn and back, with equal hashes") {
    val mixed = Picture(
      square,
      Picture(wide, circle).rotateBy(20),
      star.crop(Bounds(-40, 60, -20, 80)),
      wide.wrapShift(7, 3),
      MaskGroup(circle, wide, inverted = true),
      AnimatedPicture(Seq(square, circle), Seq(100, 200)),
      blank(10, 5)
    )
    val back = mixed.rotateBy(90).rotateBy(-90)
    assertEquals(back, mixed)
    assertEquals(back.hashCode, mixed.hashCode)
    assertNotEquals[Element, Element](mixed.rotateBy(90), mixed)
    assertNotEquals[Element, Element](mixed.withAnchor(Pos(1, 2)), mixed)
  }

  // A plain rectangle over 70..130 x 35..65, and a circle over 118..142 x 38..62 half past its
  // right edge.
  private val plain     = Rectangle(60, 30, Pos(100, 50), blue, None)
  private val pastPlain = Circle(Pos(130, 50), 12, blue, None)

  test("a mask is anchored at its content and bounded by the part of the mask over it") {
    val masked = MaskGroup(pastPlain, plain)
    assertEquals(masked.anchor, plain.anchor)
    assertEquals(masked.bounds, Some(Bounds(118, 38, 130, 62)))
    assertEquals(MaskGroup(pastPlain, plain, inverted = true).bounds, plain.bounds)
  }

  test("a mask larger than its content is bounded by the content") {
    assertEquals(MaskGroup(Circle(Pos(100, 50), 80, blue, None), plain).bounds, plain.bounds)
  }

  test("a moved and scaled mask group is bounded by the part of the mask over it, moved") {
    val moved = MaskGroup(pastPlain, plain).scaleBy(2).moveBy(5, -3)
    // Scaled by 2 about the anchor (100, 50), then moved.
    assertEquals(moved.bounds, Some(Bounds(141, 23, 165, 71)))
  }

  test("a turned mask group is bounded by its mask turned and its unturned bounds turned") {
    val turn     = Affine.rotation(30, plain.anchor)
    val turned   = boundsOf(MaskGroup(pastPlain, plain).rotateBy(30))
    val content  = Element.mapBox(turn, boundsOf(plain))
    val mask     = boundsOf(pastPlain.rotateAround(30, plain.anchor))
    val expected = Clipping.intersection(mask, Element.mapBox(turn, Bounds(118, 38, 130, 62)))
    assertNearBounds(turned, expected)
    for outer <- Seq(content, mask) do
      assertEquals(Clipping.intersection(turned, outer), turned, s"$turned is not within $outer")
    assert(turned.width < mask.width && turned.height < mask.height, s"$turned")
  }

  test("a mask with nothing to show is the empty picture") {
    assert(MaskGroup(Empty, wide) eq Empty)
    assert(MaskGroup(circle, Empty) eq Empty)
    assert(MaskGroup(circle, Empty, inverted = true) eq Empty)
    assert(MaskGroup(Empty, wide, inverted = true) eq wide)
  }

  test("a mask that misses its content is the empty picture") {
    assert(MaskGroup(circle, wide) eq Empty)
    assert(MaskGroup(circle, MaskGroup(pastPlain, plain)) eq Empty)
    assertEquals(MaskGroup(circle, wide, inverted = true).bounds, wide.bounds)
  }

  test("an animation of nothing but empty pictures is the empty picture") {
    assert(AnimatedPicture(Seq(Empty, Empty), Seq(10, 20)) eq Empty)
    assert(AnimatedPicture(Seq(Empty, square), Seq(10, 20)).isInstanceOf[AnimatedPicture])
  }

  test("an animation is anchored at its first frame and bounded by every frame") {
    val animation = AnimatedPicture(Seq(square, circle), Seq(100, 200), startTime = 0) match
      case a: AnimatedPicture => a
      case other              => fail(s"Expected an animation, not $other")
    assertEquals(animation.anchor, square.anchor)
    assertEquals(animation.bounds, Picture(square, circle).bounds)
    assertEquals[Element, Element](animation.frameAt(150), circle)
    assertEquals[Element, Element](animation.moveBy(3, 0).frameAt(50), square.moveBy(3, 0))
  }

  test("a wrap shift of a wrap shift is one shift by the sum") {
    val once = wide.wrapShift(7, 3).wrapShift(20, -11)
    assertEquals(once, wide.wrapShift(27, -8))
    assertEquals(once.hashCode, wide.wrapShift(27, -8).hashCode)
    assertEquals(once.moveBy(4, 4).wrapShift(1, 1), wide.wrapShift(28, -7).moveBy(4, 4))
  }

  test("fractional wrap shifts add up as exactly as moves do") {
    // 0.1 + 0.2 is not 0.3 in floating point, but on the grid of 2^-20 pixel it is.
    assertEquals(wide.wrapShift(0.1, 0).wrapShift(0.2, 0), wide.wrapShift(0.3, 0))
    assertEquals(wide.wrapShift(0, 0.7).wrapShift(0, 0.1), wide.wrapShift(0, 0.8))
  }

  test("a wrap shift keeps the bounds and the anchor where they are") {
    val shifted = star.rotateBy(30).wrapShift(5, 6)
    assertEquals(shifted.bounds, star.rotateBy(30).bounds)
    assertEquals(shifted.anchor, star.rotateBy(30).anchor)
    val turned = shifted.rotateBy(17)
    assertEquals(turned.wrapShift(1, 1).bounds, turned.bounds)
  }

  test("shifting by the full period gives the original back") {
    val (w, h) = (wide.width, wide.height)
    assertEquals[Element, Element](wide.wrapShift(w, h), wide)
    assertEquals[Element, Element](wide.wrapShift(-2 * w, 3 * h), wide)
    val shifted = wide.wrapShift(5, 6)
    assertEquals(shifted.wrapShift(w, -h), shifted)
    assertEquals[Element, Element](shifted.wrapShift(-5, -6), wide)
  }

  test("ten thousand shifts of a background are one wrap with the summed offset") {
    val background       = Rectangle(300, 200, Pos(150, 100), blue, None)
    var scenery: Element = background
    for _ <- 1 to 10000 do scenery = scenery.wrapShift(-2, 0)
    scenery match
      case wrap: Wrap =>
        assert(wrap.child eq background)
        assertEquals(wrap.dx, Math.floorMod(-20000, 300).toDouble)
        assertEquals(wrap.dy, 0.0)
      case other => fail(s"Expected one wrap, not $other")
  }

  test("guides outline the bounds and mark the anchor with a cross") {
    for e <- elements :+ Picture(elements*).rotateBy(30) do
      val guided = e.withGuides
      val (b, g) = (boundsOf(e), boundsOf(guided))
      assert(
        g.upperLeftCorner.x <= b.upperLeftCorner.x && g.upperLeftCorner.y <= b.upperLeftCorner.y &&
          g.lowerRightCorner.x >= b.lowerRightCorner.x &&
          g.lowerRightCorner.y >= b.lowerRightCorner.y,
        s"$g does not contain $b"
      )
      val Seq(cross, outline, content) = guided.children
      assert(content eq e)
      assertNearBounds(boundsOf(outline).shrunk, b, Grid)
      assertNear(boundsOf(cross).center, e.anchor, Grid)
      assertNear(cross.anchor, e.anchor, Grid)
      assertEquals(guided.anchor, e.anchor)
  }

  extension (b: Bounds)
    /** The box of a 1-pixel outline less the half stroke and the miters' reach at its corners. */
    private def shrunk: Bounds = Bounds(
      b.upperLeftCorner.x + 0.5,
      b.upperLeftCorner.y + 0.5,
      b.lowerRightCorner.x - 0.5,
      b.lowerRightCorner.y - 0.5
    )
end GroupLawsSuite
