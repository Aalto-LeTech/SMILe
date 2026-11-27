package smile.pictures

import smile.infrastructure.Traversal
import smile.modeling.{Affine, Bounds, Pos}

import scala.collection.mutable

/** Folds whose trees legitimately keep one level per step, for the suites that check that every
  * walk of a tree works however deep it is.
  */
object DeepTrees:
  import DeepFoldSuite.{dot, green, shuttle, steps}

  /** A tree of `build(n)` levels, more or less, named for the step that nests it. */
  final case class Deep(name: String, build: Int => Element)

  private val square = Rectangle(40, 40, green, None)
  private val window = Bounds(-15, -15, 15, 15)
  private val spot   = Circle(Pos.Origin, 5, green, None)
  private val circle = Circle(Pos.Origin, 15, green, None)

  private def turnedCrops(n: Int): Element =
    steps(n, square)((a, i) => a.rotateBy(5).addToFront(dot(i)).crop(window))

  // A mask of a group that is turned as well as moved is a group of its own. It moves back and
  // forth, so that every spot overlaps the ones before it and the group keeps something to show.
  private def turnedMasks(n: Int): Element =
    steps(n, square)((a, i) => MaskGroup(spot, a).moveBy(shuttle(i), 0).rotateBy(5))

  /** Masks of masks, whose bounds are the circle's, the part of the square at the bottom of the
    * chain over it.
    */
  val masksOfMasks: Deep =
    Deep("masks of masks", n => steps(n, square)((a, _) => MaskGroup(a, circle)))

  /** The folds that keep one level per step, each built from small parts. */
  val nested: Seq[Deep] = Seq(
    Deep("turning, adding in front and cropping", turnedCrops),
    Deep(
      "placing, turning and cropping",
      n => steps(n, square)((a, i) => a.place(dot(i), 0, 0).rotateBy(5).crop(window))
    ),
    Deep(
      "shifting, turning and cropping",
      n => steps(n, square)((a, _) => a.wrapShift(3, 2).rotateBy(5).crop(window))
    ),
    Deep(
      "scaling, adding in front and cropping",
      n => steps(n, square)((a, i) => a.scaleBy(1.001).addToFront(dot(i)).crop(window))
    ),
    Deep("masking, moving and turning", turnedMasks),
    masksOfMasks,
    Deep(
      "masks of turned masked content",
      n => steps(n, square)((a, _) => MaskGroup(circle, a.rotateBy(5)))
    ),
    Deep("a shift of turned crops", n => turnedCrops(n).wrapShift(3, 2)),
    Deep(
      "an animation of deep frames",
      n => AnimatedPicture(Seq(turnedCrops(n), turnedMasks(n)), Seq(100, 100), startTime = 0)
    )
  )

  /** A spot in one of 25 places inside a square of 10 by 10 pixels at the origin, by `i`. */
  private def speck(i: Int): Element =
    Circle(Pos(i % 5 * 2.0 - 4, i / 5 % 5 * 2.0 - 4), 1, green, None)

  /** A step that clips a level 1000 pixels wide and 4 high to the 400 pixels around the origin and
    * lays it over a strip as wide as the level. The clip decides which of the level's pixels show.
    * They are columns 300 to 699, in the two tiles of 256 from column 256 on (see
    * `smile.pictures.PixelSource.ContentTile`). The frame of the level alone would show them all.
    */
  private def clippedWide(a: Element, i: Int): Element =
    a.addToFront(speck(i)).crop(Bounds(-200, -2, 200, 2)).addToBack(Rectangle(1000, 4, green, None))

  /** The steps of chains of bitmaps of pictures, each of the bitmap before it changed by the step,
    * as a program that makes a bitmap of what it drew before on every tick builds them, named for
    * the step, with the picture each chain starts from.
    */
  val lazySteps: Seq[(String, Element, (Element, Int) => Element)] = Seq(
    ("adding in front", Rectangle(10, 10, green, None), (a, i) => a.addToFront(speck(i))),
    ("clipping a wide picture", Rectangle(1000, 4, green, None), clippedWide),
    (
      "masking",
      Rectangle(10, 10, green, None),
      (a, i) => MaskGroup(Circle(Pos.Origin, 4, green, None), a.addToFront(speck(i)))
    ),
    // A period of 11 pixels from a whole pixel is no whole number of device pixels at 1.5 of them
    // per pixel, where the wrap draws a tile of its content.
    (
      "wrapping",
      Rectangle(11, 11, Pos(5.5, 5.5), green, None),
      (a, i) => a.addToFront(speck(i).moveBy(5.5, 5.5)).wrapShift(3, 2)
    )
  )

  /** A bitmap of `start` and the `n` bitmaps of pictures made after it, each of the bitmap before
    * it changed by `step`, in the order they are made.
    */
  def lazyLevels(n: Int, start: Element)(step: (Element, Int) => Element): Vector[Bitmap] =
    (0 until n).scanLeft(Bitmap.of(start))((a, i) => Bitmap.of(step(a, i))).toVector

  /** The chains of `lazySteps`, `build(n)` levels deep. */
  val lazyChains: Seq[Deep] = lazySteps.map: (name, start, step) =>
    Deep(
      s"a chain of bitmaps of pictures, $name",
      n => steps(n, Bitmap.of(start))((a, i) => Bitmap.of(step(a, i)))
    )

  /** A chain of `n` pictures, each of the one before it and a dot, built without the box that
    * composition would measure, so that none of them knows its bounds until they are asked for.
    */
  def unmeasured(n: Int): Element =
    steps(n, square)((a, i) => new Picture(Vector(a, dot(i)), Affine.Identity, Pos.Origin))

  /** Folds that turn each level one way and the group inside it back, so that measuring the whole
    * turned meets groups whose composed affine keeps the axes again, on every level.
    */
  val counterTurned: Seq[Deep] = Seq(
    Deep(
      "inverted masks turned back",
      n =>
        steps(n, square)((a, i) =>
          Picture(
            MaskGroup(circle, a, inverted = true).rotateBy(-5).addToFront(dot(i)).moveBy(1, 0),
            dot(i + 1),
            dot(i + 2)
          ).rotateBy(5)
        )
    ),
    Deep(
      "animations turned back",
      n =>
        steps(n, square)((a, i) =>
          Picture(
            AnimatedPicture(Seq(a, dot(i)), Seq(100, 100), startTime = 0)
              .rotateBy(-5)
              .addToFront(dot(i))
              .moveBy(1, 0),
            dot(i + 1),
            dot(i + 2)
          ).rotateBy(5)
        )
    )
  )

  /** `e` rebuilt as a recolor rebuilds it, with every color kept. Every group is new, so that none
    * has measured its bounds, and every picture is given the box of the one it replaces.
    */
  def rebuilt(e: Element): Element = Traversal.mapPaints(e, identity)

  /** The number of levels of groups, clips, masks and wraps above the deepest leaf of `e`. It is
    * walked with a stack of its own, so that a deep tree is measured and does not overflow the call
    * stack.
    */
  def depth(e: Element): Int =
    var deepest = 0
    val stack   = mutable.Stack((e, 0))
    while stack.nonEmpty do
      val (element, level) = stack.pop()
      deepest = deepest.max(level)
      Trees.parts(element).foreach(part => stack.push((part, level + 1)))
    deepest
end DeepTrees
