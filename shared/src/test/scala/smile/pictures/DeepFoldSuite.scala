package smile.pictures

import smile.colors.Color
import smile.modeling.{Affine, Bounds, Pos, PositionType}
import smile.testing.Rasterizer

/** Long folds of compositions and crops, as an animation's tick or a course exercise builds them
  * one step at a time, keep the element tree shallow. Its depth does not grow with the number of
  * steps, so everything that walks the tree, drawing included, works however long the fold runs.
  * The shallow tree draws what the same steps nested one level each draw.
  */
class DeepFoldSuite extends munit.FunSuite:
  import DeepFoldSuite.*
  import DeepTrees.depth

  // Deep enough that a tree nesting one level per step would be thousands of levels deep.
  private val Steps = 2000

  // A tree that nests one level per step is about Steps deep. A flat one stays a few levels deep.
  private val Shallow = 4

  // The grid of 2^-20 pixel that every move lies on.
  private val GridStep = 1.0 / 1048576

  for fold <- Folds do
    test(s"${fold.name} keeps the tree shallow") {
      val levels = depth(fold.flat(Steps))
      assert(levels <= Shallow, s"${fold.name} is $levels levels deep after $Steps steps")
    }

  for fold <- Folds do
    test(s"${fold.name} draws as the steps nested one by one") {
      val (flat, nested) = (fold.flat(FewSteps), fold.nested(FewSteps))
      assertNearBounds(boundsOf(flat), boundsOf(nested))
      assertNear(flat.anchor, nested.anchor)
      for scale <- Seq(1.0, 1.5) do
        assertSamePixels(flat.toBitmap(scale = scale), nested.toBitmap(scale = scale), s"at $scale")
    }

  for fold <- Folds do
    test(s"${fold.name} is equal to itself built again") {
      val (once, again) = (fold.flat(FewSteps), fold.flat(FewSteps))
      assertEquals(once, again)
      assertEquals(once.hashCode, again.hashCode)
    }

  test("a mask of masks has its bounds and its hash at once, however deep the chain") {
    val chain = (0 until Steps).foldLeft(Rectangle(400, 400, green, None): Element)((acc, _) =>
      MaskGroup(acc, Circle(Pos.Origin, 150, green, None))
    )
    // The part of the mask, the square at the bottom of the chain, over the circle.
    assertEquals(chain.bounds, Some(Bounds(-150, -150, 150, 150)))
    assertEquals(chain.hashCode, chain.hashCode)
  }

  // Inverted masks over turned or scaled content. The flat group's bounds are the content's moved
  // with it, which the nested groups gave by moving the content's box, and they are equal. Turned
  // as a whole, the walk composes each level's affine onto the turn, and every composition snaps
  // its move to the grid of 2^-20 pixel, so fewer levels land up to a grid step apart.
  for (name, content) <- Seq(
      "turned"        -> Rectangle(40.3, 17.9, green, None).rotateBy(37),
      "scaled"        -> Rectangle(40.3, 17.9, green, None).scaleBy(1.37, 0.71),
      "a turned mask" -> Rectangle(40.3, 17.9, green, None)
    )
  do
    test(s"inverted masks of moved masked groups keep their bounds, over $name content") {
      val hole = Circle(Pos(0.3, 0.1), 3.3, green, None).rotateBy(23).scaleBy(1.1)
      def fold(mask: (Element, Element) => Element) =
        steps(FewSteps, content)((a, i) => mask(hole.moveBy(i * 0.7, 0.3), a).moveBy(0.37, 0.13))
      val flat   = fold((m, a) => MaskGroup(m, a, inverted = true))
      val nested = fold((m, a) => new MaskGroup(Vector((m, true)), a, Affine.Identity, a.anchor))
      assert(DeepTrees.depth(flat) <= Shallow)
      assertEquals(flat.bounds, nested.bounds)
      assertNearBounds(boundsOf(flat.rotateBy(30)), boundsOf(nested.rotateBy(30)), GridStep)
    }

  // A part added to a turned picture is placed into its frame by the inverse turn, whose move is
  // rounded to the grid of 2^-20 pixel like every move, so it lands within a few grid steps.
  private val Near = 4.0 / 1048576

  private def boundsOf(e: Element): Bounds = e.bounds.getOrElse(fail(s"$e has no bounds"))

  /** Asserts that `flat` and `nested` have the same pixels, up to what the platform's rasterizer
    * makes of the grid steps between where the two trees place the same shapes (see
    * [[smile.testing.Rasterizer.gridStepLevels]]).
    */
  private def assertSamePixels(flat: Bitmap, nested: Bitmap, clue: String): Unit =
    val (f, n) = (flat.pixels(), nested.pixels())
    val slack  = Rasterizer.gridStepLevels
    if slack == 0 then assertEquals(f.argbData.toSeq, n.argbData.toSeq, clue)
    else
      assertEquals((f.width, f.height), (n.width, n.height), clue)
      // Channels multiplied by alpha, so that a sample flipped at a faint edge moves each by as
      // little as it moves the alpha.
      def premultiplied(argb: Int): Seq[Int] =
        val alpha = argb >>> 24
        alpha +: Seq(16, 8, 0).map(shift => ((argb >>> shift) & 255) * alpha / 255)
      for (p, q) <- f.argbData.zip(n.argbData) do
        val moved = premultiplied(p).zip(premultiplied(q)).map((a, b) => (a - b).abs).max
        assert(moved <= slack, f"$clue: $p%08x and $q%08x are $moved levels apart")

  private def assertNearBounds(actual: Bounds, expected: Bounds, near: Double = Near): Unit =
    val corners = Seq(
      actual.upperLeftCorner  -> expected.upperLeftCorner,
      actual.lowerRightCorner -> expected.lowerRightCorner
    )
    for (a, b) <- corners do
      assert((a.x - b.x).abs <= near && (a.y - b.y).abs <= near, s"$actual is not $expected")

  private def assertNear(actual: Pos, expected: Pos): Unit =
    assert(
      (actual.x - expected.x).abs <= Near && (actual.y - expected.y).abs <= Near,
      s"$actual is not near $expected"
    )
end DeepFoldSuite

object DeepFoldSuite:
  val green: Option[FillStyle] = Some(FillStyle(Color(0, 128, 0)))

  /** Few enough steps that nesting one level per step is no trouble. */
  val FewSteps = 6

  /** A small dot in one of 50 columns of a grid, depending on `i`. */
  def dot(i: Int): Element = Circle(Pos(i % 50 * 4.0, i / 50 % 50 * 4.0), 2, green, None)

  /** A move of half a pixel on step `i`, to the right on even steps and back on odd ones, so that
    * masks moved on every step keep overlapping, as a mask group shows only where they all do.
    */
  def shuttle(i: Int): Double = if i % 2 == 0 then 0.5 else -0.5

  /** A fold of `step` over `n` steps from `start`, built `flat` by the library and `nested` one
    * level per step, as composition built it before it kept long folds flat. A fold whose flat tree
    * still draws in SVG as elements nested one level per step, as a mask group's masks are,
    * `nestsInSvg`.
    */
  final case class Fold(
      name: String,
      flat: Int => Element,
      nested: Int => Element,
      nestsInSvg: Boolean = false
  )

  /** `step` folded over the steps 0 until `n`, from `start`. */
  def steps(n: Int, start: Element)(step: (Element, Int) => Element): Element =
    (0 until n).foldLeft(start)(step)

  private def topLeft(e: Element): Pos = e.bounds.fold(Pos.Origin)(_.upperLeftCorner)

  // The nested forms of the steps: a picture of the parts as they are, front first, anchored at
  // the middle of its bounds, and a clip of the element as it is.
  private def group(parts: Element*): Element =
    parts.filter(Element.isSomething) match
      case Seq()     => Empty
      case Seq(only) => only
      case kept      => Picture.of(kept.toVector, None)

  private def clipOf(e: Element, rect: Bounds): Element =
    new Clip(rect, e, Affine.Identity, rect.center)

  private def wrapOf(e: Element, dx: Double, dy: Double): Element =
    e.bounds.fold(e)(box => new Wrap(box, e, dx, dy, Affine.Identity, e.anchor))

  // The nested form of a masking step. It is a group of the one given mask over the masked
  // element, which is kept as it is.
  private def maskOf(mask: Element, a: Element): Element =
    new MaskGroup(Vector((mask, false)), a, Affine.Identity, a.anchor)

  private def placedAt(e: Element, x: Double, y: Double, positionType: PositionType) =
    Composition.placedAt(e, x, y, positionType)

  private val UpperLeft = PositionType.UpperLeftCorner

  private val square = Rectangle(4, 4, green, None)
  private val big    = Rectangle(400, 400, green, None)
  private val spot   = Circle(Pos.Origin, 20, green, None)
  private val window = Bounds(-150, -150, 150, 150)
  private val whole  = Bounds(-200, -200, 200, 200) // The bounds of `big`

  // A dot added with its corner near the corner of the picture so far, and the result anchored at
  // that old corner, as placing onto a background keeps its custom anchor.
  private def cornerStep(a: Element, i: Int, add: (Element, Pos) => Element): Element =
    val corner = topLeft(a)
    add(a, Pos(corner.x + i % 7, corner.y + i % 5)).withAnchor(corner)

  // The nested form of placing the dot of step i onto an element. Both are cropped to the bounds
  // of that element.
  private def placedOnto(a: Element, i: Int): Element =
    a.bounds.fold(Empty)(box =>
      clipOf(group(placedAt(dot(i), 10, 10, PositionType.Center), a), box)
    )

  private def shrinking(i: Int) = Bounds(-200 + i * 0.01, -200 + i * 0.01, 200, 200)

  private def sliding(i: Int) = Bounds(-200 + i * 0.01, -200, 200 + i * 0.01, 200)

  /** The folds whose trees nest one level per step unless composition keeps them flat. */
  val Folds: Seq[Fold] = Seq(
    Fold(
      "moving, then adding in front",
      n => steps(n, Empty)((a, i) => a.moveBy(0.5, 0).addToFront(dot(i))),
      n => steps(n, Empty)((a, i) => group(dot(i), a.moveBy(0.5, 0)))
    ),
    Fold(
      "adding in front, then moving",
      n => steps(n, Empty)((a, i) => a.addToFront(dot(i)).moveBy(0.5, 0)),
      n => steps(n, Empty)((a, i) => group(dot(i), a).moveBy(0.5, 0))
    ),
    Fold(
      "turning, then adding in front",
      n => steps(n, square)((a, i) => a.rotateBy(5).addToFront(dot(i))),
      n => steps(n, square)((a, i) => group(dot(i), a.rotateBy(5)))
    ),
    Fold(
      "adding in front, then anchoring",
      n => steps(n, square)((a, i) => a.addToFront(dot(i)).withAnchor(Pos(1, 2))),
      n => steps(n, square)((a, i) => group(dot(i), a).withAnchor(Pos(1, 2)))
    ),
    Fold(
      "adding at a corner, anchored at the old corner",
      n =>
        steps(n, square)((a, i) =>
          cornerStep(a, i, (a, at) => a.addAt(dot(i), at.x, at.y, PositionType.UpperLeftCorner))
        ),
      n =>
        steps(n, square)((a, i) =>
          cornerStep(a, i, (a, at) => group(placedAt(dot(i), at.x, at.y, UpperLeft), a))
        )
    ),
    Fold(
      "cropping a little more every step",
      n => steps(n, big)((a, i) => a.crop(shrinking(i))),
      n => steps(n, big)((a, i) => clipOf(a, shrinking(i)))
    ),
    Fold(
      "cropping a sliding window",
      n => steps(n, big)((a, i) => a.crop(sliding(i))),
      n => steps(n, big)((a, i) => clipOf(a, sliding(i)))
    ),
    Fold(
      "placing onto a moved background",
      n => steps(n, big)((a, i) => a.place(dot(i), 10, 10).moveBy(0.5, 0)),
      n => steps(n, big)((a, i) => placedOnto(a, i).moveBy(0.5, 0))
    ),
    Fold(
      "zooming in: cropping and scaling up",
      n => steps(n, big)((a, _) => a.crop(window).scaleBy(1.001)),
      n => steps(n, big)((a, _) => clipOf(a, window).scaleBy(1.001))
    ),
    Fold(
      "zooming out: cropping and scaling down",
      n => steps(n, big)((a, _) => a.crop(window).scaleBy(0.999)),
      n => steps(n, big)((a, _) => clipOf(a, window).scaleBy(0.999))
    ),
    Fold(
      "cropping and turning by quarter turns",
      n => steps(n, big)((a, i) => a.crop(sliding(i)).rotateBy(90)),
      n => steps(n, big)((a, i) => clipOf(a, sliding(i)).rotateBy(90))
    ),
    Fold(
      "placing onto a background turned by quarter turns",
      n => steps(n, big)((a, i) => a.place(dot(i), 10, 10).rotateBy(90)),
      n => steps(n, big)((a, i) => placedOnto(a, i).rotateBy(90))
    ),
    Fold(
      "shifting and turning by quarter turns",
      n => steps(n, big)((a, _) => a.wrapShift(3, 2).rotateBy(90)),
      n => steps(n, big)((a, _) => wrapOf(a, 3, 2).rotateBy(90))
    ),
    Fold(
      "shifting a crop of a shift",
      n => steps(n, big)((a, _) => a.wrapShift(3, 2).crop(whole)),
      n => steps(n, big)((a, _) => clipOf(wrapOf(a, 3, 2), whole))
    ),
    Fold(
      "masking a moved masked group every tick",
      n => steps(n, big)((a, i) => MaskGroup(spot, a).moveBy(shuttle(i), 0)),
      n => steps(n, big)((a, i) => maskOf(spot, a).moveBy(shuttle(i), 0)),
      nestsInSvg = true
    ),
    Fold(
      "a picture of the picture so far",
      n => steps(n, square)((a, i) => Picture(a, dot(i))),
      n => steps(n, square)((a, i) => Picture.of(Vector(a, dot(i)), None))
    )
  )
end DeepFoldSuite
