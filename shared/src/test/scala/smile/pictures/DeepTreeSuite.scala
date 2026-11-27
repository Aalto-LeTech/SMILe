package smile.pictures

import smile.RenderSettings
import smile.infrastructure.{DeviceGrid, DrawPlan}
import smile.modeling.{Affine, Bounds, Pos}

/** Hashing, comparing, printing and measuring a tree work however deep it is, even under Node,
  * whose stack is about 1 MB, and give the values they gave before they stopped recursing.
  */
class DeepTreeSuite extends munit.FunSuite:
  import DeepFoldSuite.green

  // Deep enough that a walk that recurses once per level overflows a stack of 1 MB.
  private val Steps = 10000

  for d <- DeepTrees.nested do
    test(s"${d.name}: equal to itself built again, with equal hashes") {
      val (once, again) = (d.build(Steps), d.build(Steps))
      assertEquals(once, again)
      assertEquals(once.hashCode, again.hashCode)
    }

    test(s"${d.name}: unequal to the fold one step shorter") {
      assertNotEquals(d.build(Steps), d.build(Steps - 1))
    }

    test(s"${d.name}: prints") {
      assert(d.build(Steps).toString.length > Steps)
    }

    test(s"${d.name}: bounds and turned bounds") {
      val built = d.build(Steps)
      assert(built.bounds.isDefined)
      assert(built.rotateBy(30).bounds.isDefined)
    }

  private def assertSameBox(obtained: Option[Bounds], expected: Option[Bounds]): Unit =
    assert(obtained.isDefined && expected.isDefined, s"$obtained or $expected is missing")
    for (o, e) <- obtained.zip(expected) do
      assertEqualsDouble(o.upperLeftCorner.x, e.upperLeftCorner.x, 1e-9)
      assertEqualsDouble(o.upperLeftCorner.y, e.upperLeftCorner.y, 1e-9)
      assertEqualsDouble(o.lowerRightCorner.x, e.lowerRightCorner.x, 1e-9)
      assertEqualsDouble(o.lowerRightCorner.y, e.lowerRightCorner.y, 1e-9)

  test(s"a mask of masks $Steps deep measures turned") {
    val chain = DeepTrees.masksOfMasks.build(Steps)
    // A mask's bounds are the part of its mask over its content, so the chain's are the box of the
    // circle at its top, which lies within the square at its bottom, turned.
    val circleBox = Rectangle(30, 30, green, None)
    assertSameBox(chain.rotateBy(30).bounds, circleBox.rotateAround(30, chain.anchor).bounds)
  }

  for d <- DeepTrees.counterTurned do
    test(s"${d.name}, rebuilt as a recolor rebuilds it, measures turned") {
      val built = d.build(Steps)
      assertEquals(DeepTrees.rebuilt(built).rotateBy(30).bounds, built.rotateBy(30).bounds)
      assertEquals(DeepTrees.rebuilt(built).bounds, built.bounds)
    }

  test("a picture built without a known box measures deep") {
    val chain  = DeepTrees.unmeasured(Steps)
    val leaves = Vector.newBuilder[Element]
    val stack  = scala.collection.mutable.Stack(chain)
    while stack.nonEmpty do
      val e     = stack.pop()
      val parts = Trees.parts(e).toVector
      if parts.isEmpty then leaves += e else stack.pushAll(parts)
    assertEquals(chain.bounds, Element.union(leaves.result().iterator.map(_.worldBounds)))
  }

  private val square = Rectangle(4, 4, green, None)
  private val spot   = Circle(Pos.Origin, 2, green, None)
  private val clip   = square.crop(Bounds(0, 0, 2, 2)).asInstanceOf[Clip]
  private val mask   = MaskGroup(spot, square).asInstanceOf[MaskGroup]
  private val wrap   = square.wrapShift(1, 1).asInstanceOf[Wrap]

  /** What `e.boundsIn(parent)` gave when it recursed, every box measured again. */
  private def boundsAsBefore(e: Element, parent: Affine): Option[Bounds] =
    val m = if parent == Affine.Identity then e.affine else e.affine.andThen(parent)
    def group(parts: Seq[Element]): Option[Bounds] =
      if m.keepsAxes then
        Element
          .union(parts.iterator.map(boundsAsBefore(_, Affine.Identity)))
          .map(Element.mapBox(m, _))
      else Element.union(parts.iterator.map(boundsAsBefore(_, m)))
    e match
      case p: Picture         => group(p.children)
      case g: MaskGroup       => g.within(m, group(Seq(g.bounding)))
      case a: AnimatedPicture => group(a.frames)
      case other              => other.boundsThrough(m)

  // A group turned one way inside a picture turned back, so that its composed affine keeps the
  // axes, and its box is mapped where the rest is measured through the turn.
  private val tilted              = Rectangle(30, 10, green, None).rotateBy(20)
  private val counterTurnedGroups = Seq(
    "a picture"              -> Picture(tilted, spot.moveBy(9, 3)),
    "a mask group"           -> MaskGroup(tilted, square),
    "an inverted mask group" -> MaskGroup(spot, tilted, inverted = true),
    "an animation"           -> AnimatedPicture(Seq(tilted, spot), Seq(10, 10), startTime = 0)
  )

  for (name, group) <- counterTurnedGroups do
    test(s"$name turned back inside a turned picture keeps its exact bounds") {
      val inner = group.rotateBy(30)
      val outer = new Picture(Vector(inner, tilted.moveBy(5, 5)), Affine.Identity, Pos.Origin)
      for e <- Seq(
          outer.rotateBy(-30),
          outer.rotateBy(-30).rotateBy(45),
          outer.rotateBy(-30).scaleBy(1.5)
        )
      do assertEquals(e.bounds, boundsAsBefore(e, Affine.Identity))
    }

  test(s"a mask-side chain $Steps deep draws turned in linear time") {
    // Drawing measures each level through its own turned affine, which walked the whole chain below
    // it again, so ten times the levels took a hundred times as long.
    def millis(steps: Int): Double =
      (1 to 3)
        .map: _ =>
          val turned = DeepTrees.masksOfMasks.build(steps).rotateBy(30)
          val start  = System.nanoTime()
          val _      = DrawPlan.of(turned, DeviceGrid(1), RenderSettings.Default, 0L)
          (System.nanoTime() - start) / 1e6
        .min
    val (short, long) = (millis(Steps / 10), millis(Steps))
    assert(long < 30 * short, s"$long ms for $Steps levels, $short ms for ${Steps / 10}")
  }

  test("a mask-side chain measured turned level by level keeps its exact bounds") {
    // Masks of masks turned and moved on every level, measured as drawing the chain turned measures
    // it. Each level is measured through its own affine under the levels above it, the deepest
    // first. The moves go back and forth, so that each mask keeps overlapping the spot it masks.
    val chain = DeepFoldSuite.steps(300, square): (a, i) =>
      val move = DeepFoldSuite.shuttle(i)
      MaskGroup(a.rotateBy(7).moveBy(0.6 * move, 0.2 * move), spot)
    val top    = chain.rotateBy(30)
    val levels = Vector.unfold[(Element, Affine), (Element, Affine)]((top, Affine.Identity)):
      case (group: MaskGroup, parent) =>
        Some(((group, parent), (group.bounding, group.affine.andThen(parent))))
      case _ => None
    assertEquals(levels.size, 300)
    for (group, parent) <- levels.reverse do
      assertEquals(group.boundsIn(parent), boundsAsBefore(group, parent))
    // Measured from the top under another turn, every level was last measured through another.
    val again = top.rotateBy(15)
    assertEquals(again.bounds, boundsAsBefore(again, Affine.Identity))
  }

  test("a clip's hash is the hash of its fields") {
    assertEquals(clip.hashCode, (clip.rect, clip.child, clip.affine, clip.localAnchor).hashCode)
  }

  test("a picture's hash is the hash of its fields") {
    val p = Picture(clip, mask, wrap).asInstanceOf[Picture]
    assertEquals(p.hashCode, (p.parts.hash, p.affine, p.localAnchor).hashCode)
  }

  test("a mask group's hash is the hash of its fields") {
    val m = mask
    assertEquals(m.masks.size, 1)
    val (one, inverted) = m.masks.head
    assertEquals(m.hashCode, (one, m.masked, inverted, m.affine, m.localAnchor).hashCode)
  }

  test("a mask group of several masks hashes them in order") {
    val m = MaskGroup(spot, MaskGroup(spot, tilted, inverted = true)).asInstanceOf[MaskGroup]
    assertEquals(m.masks.map(_._2), Vector(true, false))
    assertEquals(m.hashCode, (m.masks, m.masked, m.affine, m.localAnchor).hashCode)
  }

  test("a mask group of several masks prints them in order, and their flags") {
    val m = MaskGroup(spot, MaskGroup(spot, square, inverted = true))
    assertEquals(
      m.toString,
      s"MaskGroup($spot, $spot, $square, inverted = (true, false), "
        + s"${Affine.Identity}, anchor = ${square.anchor})"
    )
  }

  test("a wrap's hash is the hash of its fields") {
    val w = wrap
    assertEquals(w.hashCode, (w.period, w.child, w.dx, w.dy, w.affine, w.localAnchor).hashCode)
  }

  // Scala.js prints a whole Double without its ".0".
  private val onTheJVM = 1.0.toString == "1.0"

  // Copied from what toString printed before it stopped recursing.
  private val PictureOnTheJVM =
    "Picture(Clip(Bounds((0.0,0.0),(2.0,2.0)), Shape(PathShape([-2.0, -2.0, 2.0, "
      + "-2.0, 2.0, 2.0, -2.0, 2.0], closed = true), Some(FillStyle(#008000)), None, "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0)), "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (1.0,1.0)), "
      + "MaskGroup(Shape(EllipseArcShape(2.0, 2.0, 0.0, 360.0), "
      + "Some(FillStyle(#008000)), None, Affine(1.0,0.0,0.0,1.0,0.0,0.0), "
      + "anchor = (0.0,0.0)), Shape(PathShape([-2.0, -2.0, 2.0, -2.0, 2.0, 2.0, -2.0, "
      + "2.0], closed = true), Some(FillStyle(#008000)), None, "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0)), inverted = false, "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0)), "
      + "Wrap(Bounds((-2.0,-2.0),(2.0,2.0)), Shape(PathShape([-2.0, -2.0, 2.0, -2.0, "
      + "2.0, 2.0, -2.0, 2.0], closed = true), Some(FillStyle(#008000)), None, "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0)), dx = 1.0, dy = 1.0, "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0)), "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0))"

  private val AnimationOnTheJVM =
    "AnimatedPicture(Shape(EllipseArcShape(2.0, 2.0, 0.0, 360.0), "
      + "Some(FillStyle(#008000)), None, Affine(1.0,0.0,0.0,1.0,0.0,0.0), "
      + "anchor = (0.0,0.0)), Empty, Vector(10, 20), startTime = 0, "
      + "Affine(1.0,0.0,0.0,1.0,0.0,0.0), anchor = (0.0,0.0))"

  private val PictureInJS =
    "Picture(Clip(Bounds((0.0,0.0),(2.0,2.0)), Shape(PathShape([-2, -2, 2, -2, 2, "
      + "2, -2, 2], closed = true), Some(FillStyle(#008000)), None, "
      + "Affine(1,0,0,1,0,0), anchor = (0.0,0.0)), Affine(1,0,0,1,0,0), "
      + "anchor = (1.0,1.0)), MaskGroup(Shape(EllipseArcShape(2, 2, 0, 360), "
      + "Some(FillStyle(#008000)), None, Affine(1,0,0,1,0,0), anchor = (0.0,0.0)), "
      + "Shape(PathShape([-2, -2, 2, -2, 2, 2, -2, 2], closed = true), "
      + "Some(FillStyle(#008000)), None, Affine(1,0,0,1,0,0), anchor = (0.0,0.0)), "
      + "inverted = false, Affine(1,0,0,1,0,0), anchor = (0.0,0.0)), "
      + "Wrap(Bounds((-2.0,-2.0),(2.0,2.0)), Shape(PathShape([-2, -2, 2, -2, 2, 2, -2, "
      + "2], closed = true), Some(FillStyle(#008000)), None, Affine(1,0,0,1,0,0), "
      + "anchor = (0.0,0.0)), dx = 1, dy = 1, Affine(1,0,0,1,0,0), "
      + "anchor = (0.0,0.0)), Affine(1,0,0,1,0,0), anchor = (0.0,0.0))"

  private val AnimationInJS =
    "AnimatedPicture(Shape(EllipseArcShape(2, 2, 0, 360), "
      + "Some(FillStyle(#008000)), None, Affine(1,0,0,1,0,0), anchor = (0.0,0.0)), "
      + "Empty, Vector(10, 20), startTime = 0, Affine(1,0,0,1,0,0), anchor = (0.0,0.0))"

  test("toString is as before") {
    val animation           = AnimatedPicture(Seq(spot, Empty), Seq(10, 20), startTime = 0)
    val (picture, animated) =
      if onTheJVM then (PictureOnTheJVM, AnimationOnTheJVM) else (PictureInJS, AnimationInJS)
    assertEquals(Picture(clip, mask, wrap).toString, picture)
    assertEquals(animation.toString, animated)
  }
end DeepTreeSuite
