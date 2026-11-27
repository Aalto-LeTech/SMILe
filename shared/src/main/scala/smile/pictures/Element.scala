package smile.pictures

import smile.RenderSettings
import smile.colors.{Color, LinearGradient, Paint}
import smile.infrastructure.{NativeBuffer, PlatformSpecific, Renderer, ResourceFactory}
import smile.modeling.{Affine, Bounds, LocalShape, Pos, Support}

import scala.annotation.unused
import scala.concurrent.Future

/** Anything that can be drawn, such as a shape, a group of elements, a bitmap, an SVG picture or
  * nothing at all.
  *
  * Every element is immutable and carries one affine, which places its local geometry in the world,
  * and an anchor, a local point that the affine carries along. Every transform composes into the
  * affine and is defined once, here:
  *
  *   - `rotateBy`, `scaleBy` and `scaleTo` turn and scale around the anchor, and the flips mirror
  *     along the world axes through it, so an asymmetric shape spins in place and two rotations
  *     equal one combined rotation.
  *   - `moveTo` puts the anchor at a point, and `withAnchor` moves the anchor without moving
  *     anything drawn.
  *   - `rotateAround` and `scaleAround` turn and scale around a pivot given for that one call.
  *
  * By default the anchor is the middle of the element's local, untransformed geometry, so an
  * unrotated element starts with its anchor in the middle of its bounds.
  *
  * A zero scale factor or a zero target size leaves nothing to draw and gives the empty picture.
  * Amounts that are not finite numbers are refused with an `IllegalArgumentException`.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  *
  * val flag   = Rectangle(90, 60, fillWith(Tomato), None)
  * val turned = flag.rotateBy(30)
  * val both   = flag.addToRight(turned, gap = 20)
  * ```
  *
  * ![A rectangle, and the same rectangle turned 30 degrees counterclockwise](pictures/both.png)
  *
  * The same transforms on a text, whose letters show which way each one goes. An uneven scaling
  * after a turn slants it.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val word     = Text(Pos.Origin, None, "SMILe", "sans-serif", 40, 700, fillWith(Navy), None)
  * val tilted   = word.rotateBy(15)
  * val wide     = word.scaleBy(1.5, 0.6)
  * val mirrored = word.flipHorizontally
  * val slanted  = word.rotateBy(30).scaleBy(1.4, 0.5)
  * val versions = Seq(word, tilted, wide, mirrored, slanted)
  * val showcase = versions.reduce((column, next) => column.addToBottom(next, gap = 12))
  * ```
  *
  * ![The word SMILe plain, tilted, wide, mirrored and slanted](pictures/showcase.png)
  *
  * Composing elements with `addToRight`, `addToFront`, `replicateTo` and the rest gives one picture
  * anchored at the middle of its bounds. The empty picture is the identity. Adding it changes
  * nothing, and no gap is laid out next to it.
  *
  * Elements are equal when they draw the same geometry in the same style through the same affine
  * with the same anchor. Affines and anchors are normalized when they are made (see
  * [[smile.modeling.Affine]]), so a quarter turn and back gives exactly the element it started as.
  * Bitmaps are the exception. They are equal only with the same source, and their own transforms
  * record steps on a new source. Turns that add up to none give back the source from before them,
  * so a turn and back is exact there too, but a scaling and back, or a flip and back, is not.
  *
  * @groupname measuring Size and place
  * @groupprio measuring 10
  * @groupname moving Moving
  * @groupprio moving 20
  * @groupname turning Turning and flipping
  * @groupprio turning 30
  * @groupname scaling Scaling
  * @groupprio scaling 40
  */
sealed trait Element derives CanEqual:

  /** The type of element that a transform that keeps an element whole returns. */
  type Self <: Element

  /** The map from this element's local frame to the world. */
  private[smile] def affine: Affine

  /** The anchor in this element's local frame, which `affine` carries to the world. */
  private[smile] def localAnchor: Pos

  /** This element with `a` in place of its affine and the same local anchor. */
  private[smile] def withAffine(a: Affine): Self

  /** This element with `p` as its local anchor, drawn exactly as before. */
  private[smile] def withLocalAnchor(p: Pos): Self

  /** The tight box around what this element draws in the world, or `None` for nothing. */
  private[smile] def worldBounds: Option[Bounds]

  /** The tight box around what this element draws once `m` places its local frame in the world in
    * place of `affine`, or `None` for nothing.
    */
  private[smile] def boundsThrough(m: Affine): Option[Bounds]

  /** Whether this element's hash is at hand without walking the elements under it. It always is for
    * a leaf, and for a group once its hash is cached.
    */
  private[pictures] def hashKnown: Boolean = true

  /** Whether this element's world bounds are at hand without walking the elements under it. They
    * always are for a leaf, a clip or a wrap, whose bounds come from a rectangle of its own, and
    * for a group once its bounds are cached. A group caches them only once every group under it
    * that measuring it walks has cached its own.
    */
  private[pictures] def boundsKnown: Boolean = true

  /** The tight box around what this element draws once `parent` places the frame that `affine` maps
    * into, as a group's affine places its parts.
    */
  private[smile] final def boundsIn(parent: Affine): Option[Bounds] =
    if parent == Affine.Identity then worldBounds else boundsThrough(affine.andThen(parent))

  /** The tight box around what this element draws, outlines included, or `None` when it draws
    * nothing and takes no room.
    *
    * @group measuring
    */
  final def bounds: Option[Bounds] = worldBounds

  /** The width of [[smile.pictures.Element.bounds]] in pixels, or 0 when there are none.
    *
    * @group measuring
    */
  final def width: Double = bounds.fold(0.0)(_.width)

  /** The height of [[smile.pictures.Element.bounds]] in pixels, or 0 when there are none.
    *
    * @group measuring
    */
  final def height: Double = bounds.fold(0.0)(_.height)

  /** The point this element is placed, turned and scaled by.
    *
    * @group measuring
    */
  final def anchor: Pos = affine(localAnchor)

  /** This element with its anchor at `p`, drawn exactly as before. The anchor goes to the nearest
    * multiple of 2^-20 pixel, where every translation lies, so that quarter turns around it stay
    * exact. Every transform puts the anchor back on that grid, which moves it by at most half a
    * step.
    *
    * @throws IllegalArgumentException
    *   if a coordinate of `p` is not finite
    * @group moving
    */
  final def withAnchor(p: Pos): Self =
    withLocalAnchor(Element.localAnchorAt(affine, p))

  /** This element moved by `dx` to the right and `dy` down.
    *
    * @throws IllegalArgumentException
    *   if an offset is not finite
    * @group moving
    */
  final def moveBy(dx: Double, dy: Double): Self =
    require(dx.isFinite && dy.isFinite, s"An offset must be finite, not ($dx, $dy)")
    withAffine(affine.andThen(Affine.translation(dx, dy)))

  /** This element moved so that its anchor is at `p`.
    *
    * @throws IllegalArgumentException
    *   if a coordinate of `p` is not finite
    * @group moving
    */
  final def moveTo(p: Pos): Self =
    Element.requireFinite(p, "A position")
    val from = anchor
    moveBy(p.x - from.x, p.y - from.y)

  /** This element turned by `degrees` around its anchor, counterclockwise on screen for a positive
    * angle. Right angles are exact.
    *
    * @throws IllegalArgumentException
    *   if the angle is not finite
    * @group turning
    */
  final def rotateBy(degrees: Double): Self = rotateAround(degrees, anchor)

  /** This element turned by `degrees` around `pivot`, counterclockwise on screen for a positive
    * angle. Right angles are exact.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val hand = Rectangle(60, 8, fillWith(Navy), None)
    * val fan  = Picture((0 to 150 by 30).map(angle => hand.rotateAround(angle, Pos(-30, 0)))*)
    * ```
    *
    * ![Six bars fanning out counterclockwise from one end](pictures/fan.png)
    *
    * @throws IllegalArgumentException
    *   if the angle or a coordinate of `pivot` is not finite
    * @group turning
    */
  final def rotateAround(degrees: Double, pivot: Pos): Self =
    Element.requireFinite(pivot, "A pivot")
    turnedAround(degrees, pivot)

  /** This element turned by `degrees` around the world point `pivot`, which is finite. A bitmap
    * also records the turn as a step of its own.
    *
    * @throws IllegalArgumentException
    *   if the angle is not finite
    */
  private[smile] def turnedAround(degrees: Double, pivot: Pos): Self =
    withAffine(affine.andThen(Affine.rotation(degrees, pivot)))

  /** This element scaled by `factor` around its anchor. A negative factor also mirrors it, and a
    * zero factor gives the empty picture.
    *
    * @throws IllegalArgumentException
    *   if the factor is not finite
    * @group scaling
    */
  final def scaleBy(factor: Double): Element = scaleAround(factor, factor, anchor)

  /** This element scaled by `horizontal` along the world's x axis and `vertical` along its y axis,
    * around its anchor. A negative factor also mirrors it on that axis, and a zero factor gives the
    * empty picture.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val coin      = Circle(Pos.Origin, 30, fillWith(Gold), lineWith(Black, 4, round = true))
    * val stretched = coin.scaleBy(2, 0.5)
    * ```
    *
    * ![A golden circle with a black outline](pictures/coin.png) ![The circle stretched wide and
    * flat, its outline thicker at the sides](pictures/stretched.png)
    *
    * @throws IllegalArgumentException
    *   if a factor is not finite
    * @group scaling
    */
  final def scaleBy(horizontal: Double, vertical: Double): Element =
    scaleAround(horizontal, vertical, anchor)

  /** This element scaled by `factor` around `pivot`. A negative factor also mirrors it, and a zero
    * factor gives the empty picture.
    *
    * @throws IllegalArgumentException
    *   if the factor or a coordinate of `pivot` is not finite, or the scaled element would be too
    *   large for its transform to hold in a `Double`
    * @group scaling
    */
  final def scaleAround(factor: Double, pivot: Pos): Element =
    scaleAround(factor, factor, pivot)

  /** This element scaled by `horizontal` along the world's x axis and `vertical` along its y axis,
    * around `pivot`. A negative factor also mirrors it on that axis, and a zero factor gives the
    * empty picture.
    *
    * @throws IllegalArgumentException
    *   if a factor or a coordinate of `pivot` is not finite, or the scaled element would be too
    *   large for its transform to hold in a `Double`
    * @group scaling
    */
  final def scaleAround(horizontal: Double, vertical: Double, pivot: Pos): Element =
    require(
      horizontal.isFinite && vertical.isFinite,
      s"A scale factor must be finite, not ($horizontal, $vertical)"
    )
    Element.requireFinite(pivot, "A pivot")
    val determinant = affine.determinant * horizontal * vertical
    // Factors small enough to flatten the plane in floating point leave nothing to draw either.
    if determinant == 0 then Empty
    else
      val (dx, dy) = (pivot.x - horizontal * pivot.x, pivot.y - vertical * pivot.y)
      val scaled   = Seq(
        affine.a * horizontal,
        affine.b * vertical,
        affine.c * horizontal,
        affine.d * vertical,
        affine.e * horizontal + dx,
        affine.f * vertical + dy
      )
      require(
        determinant.isFinite && dx.isFinite && dy.isFinite && scaled.forall(_.isFinite),
        s"Scaling by ($horizontal, $vertical) around $pivot overflows the element's transform"
      )
      scaledAround(horizontal, vertical, pivot, Affine(horizontal, 0, 0, vertical, dx, dy))

  /** This element scaled by `horizontal` and `vertical` around the world point `pivot`, which
    * `scaling` does, with the arguments checked and the result known not to overflow. A bitmap also
    * records the scaling as a step of its own where a step can express it.
    */
  private[smile] def scaledAround(
      @unused horizontal: Double,
      @unused vertical: Double,
      @unused pivot: Pos,
      scaling: Affine
  ): Element =
    withAffine(affine.andThen(scaling))

  /** This element scaled around its anchor along the world axes so that its bounds are `width` wide
    * and `height` high, outlines included, since they scale with the shape.
    *
    * A negative target flips the element on that axis first. A zero target gives the empty picture.
    * An axis on which the element has no extent, such as the width of an empty text, is left as it
    * is.
    *
    * Scaling along the world axes scales the bounds by exactly the same factors, so one step is
    * enough, up to the rounding of the factors and of the arithmetic that places the bounds. That
    * rounding has an absolute floor of about 2^-19 pixel, since the anchor and every move lie on
    * the grid of 2^-20 pixel, so a target smaller than that is not met to its own precision.
    *
    * @throws IllegalArgumentException
    *   if a target is not finite, or the target is so much larger than the current extent that the
    *   factor between them, or the scaled transform, overflows a `Double`
    * @group scaling
    */
  final def scaleTo(width: Double, height: Double): Element =
    require(
      width.isFinite && height.isFinite,
      s"A target size must be finite, not ($width, $height)"
    )
    bounds match
      case None                                 => this
      case Some(_) if width == 0 || height == 0 => Empty
      case Some(_) if width < 0 || height < 0   =>
        val flipped: Element = (width < 0, height < 0) match
          case (true, false) => flipHorizontally
          case (false, true) => flipVertically
          case _             => flipDiagonally
        flipped.scaleTo(width.abs, height.abs)
      case Some(current) =>
        def factor(target: Double, extent: Double) = if extent == 0 then 1.0 else target / extent
        val (h, v)                                 =
          (factor(width, current.width), factor(height, current.height))
        require(
          h.isFinite && v.isFinite,
          s"Scaling $current to $width by $height overflows: the factors would be ($h, $v)"
        )
        scaleBy(h, v)

  /** This element mirrored left to right across the vertical line through its anchor.
    *
    * @group turning
    */
  final def flipHorizontally: Self = mirrored(horizontally = true, vertically = false)

  /** This element mirrored top to bottom across the horizontal line through its anchor.
    *
    * @group turning
    */
  final def flipVertically: Self = mirrored(horizontally = false, vertically = true)

  /** This element mirrored both ways through its anchor, which is a half turn around it.
    *
    * @group turning
    */
  final def flipDiagonally: Self = mirrored(horizontally = true, vertically = true)

  /** This element mirrored along the world axes through its anchor. A bitmap also records the flip
    * as a step of its own.
    */
  private[smile] def mirrored(horizontally: Boolean, vertically: Boolean): Self =
    withAffine(affine.andThen(Affine.mirroring(horizontally, vertically, anchor)))
end Element

private[smile] object Element:

  private val Grid = 1048576.0 // 2^20, as for an affine's translation

  /** The local anchor an element placed by `affine` keeps for the local point `local`. It is the
    * local point whose world position is that of `local` rounded to the 2^-20 grid (see
    * `smile.pictures.Element.localAnchorAt`), with `-0.0` made `0.0`. Every element class stores
    * its anchor through this whenever it is made with a new affine, so that every stored anchor is
    * normalized and quarter turns compose exactly after any transform.
    *
    * @throws IllegalArgumentException
    *   if a coordinate of `local` is not finite
    */
  def normalizedAnchor(affine: Affine, local: Pos): Pos =
    requireFinite(local, "An anchor")
    localAnchorAt(affine, affine(local))

  /** `fill` with its zeros made `0.0`, so that styles that are equal hash equally. */
  def normalizedFill(fill: Option[FillStyle]): Option[FillStyle] =
    fill.map(style => FillStyle(normalizedPaint(style.paint)))

  /** `stroke` with its zeros made `0.0`, so that styles that are equal hash equally. */
  def normalizedStroke(stroke: Option[StrokeStyle]): Option[StrokeStyle] =
    stroke.map(style => style.copy(paint = normalizedPaint(style.paint), width = style.width + 0.0))

  private def normalizedPaint(paint: Paint): Paint = paint match
    case gradient: LinearGradient =>
      gradient.copy(
        start = Pos(gradient.start.x + 0.0, gradient.start.y + 0.0),
        end = Pos(gradient.end.x + 0.0, gradient.end.y + 0.0),
        stops = gradient.stops.map((fraction, color) => (fraction + 0.0, color))
      )
    case other => other

  /** The local anchor that puts an element placed by `affine` with its anchor at the world point
    * `p`, rounded to the grid of 2^-20 pixel that an affine's translation is rounded to.
    *
    * Every translation lies on that grid, so an anchor on it stays on it under moves, quarter turns
    * and mirrors, and turning around it and back gives exactly the element it started as. An anchor
    * off the grid would not. The turn around it would round its translation, and the anchor would
    * creep by up to half a grid step on every turn.
    *
    * The local point is solved for directly, since the translation of
    * [[smile.modeling.Affine.inverse]] is rounded to the grid.
    *
    * @throws IllegalArgumentException
    *   if a coordinate of `p` is not finite
    */
  def localAnchorAt(affine: Affine, p: Pos): Pos =
    requireFinite(p, "An anchor")
    val det = affine.determinant
    val x   = Math.rint(p.x * Grid) / Grid - affine.e
    val y   = Math.rint(p.y * Grid) / Grid - affine.f
    Pos((affine.d * x - affine.c * y) / det + 0.0, (affine.a * y - affine.b * x) / det + 0.0)

  def requireFinite(p: Pos, what: String): Unit =
    require(p.x.isFinite && p.y.isFinite, s"$what must be finite, not $p")

  /** Whether `e` draws something or takes room, that is, is not the empty picture. */
  def isSomething(e: Element): Boolean = e match
    case Empty => false
    case _     => true

  /** `e` with `m` composed into its affine, or `e` itself when `m` is the identity. */
  def placedBy(e: Element, m: Affine): Element =
    if m == Affine.Identity then e else e.withAffine(e.affine.andThen(m))

  /** The box around where `m` takes the box `b`, which is exact when `m` keeps the axes. */
  def mapBox(m: Affine, b: Bounds): Bounds =
    m.boxBounds(
      b.upperLeftCorner.x,
      b.upperLeftCorner.y,
      b.lowerRightCorner.x,
      b.lowerRightCorner.y
    )

  /** `b` with its zeros made `0.0`, so that boxes that are equal hash equally.
    *
    * @throws IllegalArgumentException
    *   if a coordinate of `b` is not finite
    */
  def normalizedBox(b: Bounds): Bounds =
    requireFinite(b.upperLeftCorner, "A corner")
    requireFinite(b.lowerRightCorner, "A corner")
    Bounds(
      b.upperLeftCorner.x + 0.0,
      b.upperLeftCorner.y + 0.0,
      b.lowerRightCorner.x + 0.0,
      b.lowerRightCorner.y + 0.0
    )

  /** The smallest box around every box in `boxes`, or `None` when there are none. */
  def union(boxes: Iterator[Option[Bounds]]): Option[Bounds] =
    var (left, top, right, bottom) = (0.0, 0.0, 0.0, 0.0)
    var found                      = false
    for box <- boxes; b <- box do
      val (l, t, r, w) =
        (b.upperLeftCorner.x, b.upperLeftCorner.y, b.lowerRightCorner.x, b.lowerRightCorner.y)
      if found then
        left = left.min(l)
        top = top.min(t)
        right = right.max(r)
        bottom = bottom.max(w)
      else
        left = l
        top = t
        right = r
        bottom = w
        found = true
    if found then Some(Bounds(left, top, right, bottom)) else None

  /** The tight box around `parts` once `m` places the frame they share.
    *
    * When `m` keeps the axes, it maps `box`, the parts' cached box in that frame, exactly. Under
    * other angles and shears, each part is measured again through the composed affine, down to the
    * leaves where the parts' own affines do not keep the axes either.
    */
  def groupBounds(parts: Iterable[Element], box: => Option[Bounds], m: Affine): Option[Bounds] =
    if m.keepsAxes then box.map(mapBox(m, _)) else Trees.turnedBounds(parts, m)
end Element

/** A leaf element, which is a local `shape` filled with `fill` and outlined with `stroke`, placed
  * in the world by `affine`. Paths, ellipse arcs, points and text are all shapes. The stroke lives
  * in the local frame and scales with the shape.
  *
  * The factories in [[smile.pictures]] build shapes, such as [[smile.pictures.Rectangle]] and
  * [[smile.pictures.Circle]].
  */
final class Shape private[smile] (
    private[smile] val shape: LocalShape,
    private[smile] val fill: Option[FillStyle],
    private[smile] val stroke: Option[StrokeStyle],
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends Element:
  type Self = Shape

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): Shape = new Shape(shape, fill, stroke, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): Shape = new Shape(shape, fill, stroke, affine, p)

  private[smile] lazy val worldBounds: Option[Bounds] = boundsThrough(affine)

  private[smile] def boundsThrough(m: Affine): Option[Bounds] =
    Some(Support.worldBounds(shape, stroke, m))

  override def equals(other: Any): Boolean = other match
    case that: Shape =>
      (this eq that) || (affine == that.affine && localAnchor == that.localAnchor &&
        fill == that.fill && stroke == that.stroke && shape == that.shape)
    case _ => false

  override lazy val hashCode: Int = (shape, fill, stroke, affine, localAnchor).hashCode

  override def toString: String = s"Shape($shape, $fill, $stroke, $affine, anchor = $localAnchor)"
end Shape

/** A group, which is an element with elements under it. Its hash is cached and computed with a
  * stack of its own, so that a tree of any depth hashes without overflowing the call stack.
  */
private[pictures] sealed trait CachedHash extends Element:

  /** The hash of this group from the hashes of its fields, the elements under it included. */
  private[pictures] def hashFormula: Int

  // A true hash of 0 is computed again on every call. One Int is safe to publish without a lock.
  private var hashCache: Int = 0

  override private[pictures] final def hashKnown: Boolean = hashCache != 0

  override final def hashCode: Int =
    val cached = hashCache
    if cached != 0 then cached
    else
      Trees.hashParts(this)
      val hash = hashFormula
      hashCache = hash
      hash
end CachedHash

/** A group whose world bounds are cached and measured with a stack of their own, so that a tree of
  * any depth is measured without overflowing the call stack.
  */
private[pictures] sealed trait CachedBounds extends Element:

  // Neither volatile nor locked, as the racy single-check idiom needs neither. It is one field, and
  // an Option of Bounds is immutable with final fields, so another thread sees null or the whole
  // value, and one that sees null measures again and caches the same bounds.
  private var boundsCache: Option[Bounds] | Null = null

  override private[pictures] final def boundsKnown: Boolean = boundsCache != null

  /** Whether the bounds follow from a box that is cached already, without measuring any part. */
  protected def boundsFromCachedBox: Boolean = false

  private[smile] final def worldBounds: Option[Bounds] =
    val cached = boundsCache
    if cached != null then cached
    // Not cached, since the parts under the box may not have measured themselves.
    else if boundsFromCachedBox then boundsThrough(affine)
    else
      Trees.measureParts(this)
      measured()

  /** The bounds measured from parts that have cached theirs, and cached in turn. */
  private[pictures] final def measured(): Option[Bounds] =
    val bounds = boundsThrough(affine)
    boundsCache = bounds
    bounds
end CachedBounds

/** A group of elements, drawn front to back. The first child is in front of all the others.
  *
  * A picture is its own affine plus its children, so transforming it composes into that affine in
  * constant time and keeps every child as it is. Its bounds are the union of its children's bounds
  * through its affine. Its anchor is the middle of its content's bounds when it is built, carried
  * by its later transforms.
  *
  * [[smile.pictures.Picture.apply]] builds one, and the composition methods, such as `addToRight`,
  * combine elements into one. A picture never holds the empty picture, and a picture of nothing but
  * empty pictures is the empty picture itself.
  */
final class Picture private[smile] (
    private[smile] val parts: Picture.Parts,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends CachedHash,
      CachedBounds:
  type Self = Picture

  /** A picture of `children` placed by `affine` with its anchor at the local point `localAnchor`.
    */
  private[smile] def this(children: Vector[Element], affine: Affine, localAnchor: Pos) =
    this(new Picture.Parts(children, None), affine, localAnchor)

  /** The children, front first. */
  private[smile] def children: Vector[Element] = parts.elements

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): Picture = new Picture(parts, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): Picture = new Picture(parts, affine, p)

  // Through an affine that keeps the axes, the bounds are the children's box mapped.
  override protected def boundsFromCachedBox: Boolean = affine.keepsAxes && parts.boxKnown

  private[smile] def boundsThrough(m: Affine): Option[Bounds] =
    Element.groupBounds(children, parts.box, m)

  override def equals(other: Any): Boolean = other match
    case that: Picture => (this eq that) || Trees.sameTree(this, that)
    case _             => false

  private[pictures] def hashFormula: Int = (parts.hash, affine, localAnchor).hashCode

  override def toString: String = Trees.text(this)
end Picture

/** Builds pictures. */
object Picture:

  /** The empty picture. */
  val empty: Empty.type = Empty

  /** A picture of `elements`, the first in front, anchored at the middle of their bounds. The empty
    * pictures among them are left out, and nothing but empty pictures gives the empty picture. A
    * picture among them gives its children, as composition does, so a picture of a picture is no
    * deeper than the picture.
    */
  def apply(elements: Element*): Element =
    val kept = elements.iterator.filter(Element.isSomething).toVector
    if kept.isEmpty then Empty else Composition.grouped(kept)

  /** A picture of `children` anchored at the middle of their bounds, which are `box` when it is
    * given. A given `box` must be the union of the children's bounds.
    */
  private[smile] def of(children: Vector[Element], box: Option[Option[Bounds]]): Picture =
    val parts = new Parts(children, box)
    new Picture(parts, Affine.Identity, parts.box.fold(Pos.Origin)(_.center))

  /** The elements under a group with what they cache, their box in the group's own frame and their
    * hash. They are a picture's children, which every transformed copy of the picture shares, or an
    * animation's frames, whose box alone is used.
    */
  private[smile] final class Parts(val elements: Vector[Element], known: Option[Option[Bounds]]):
    // Cached as a group caches its bounds, and not volatile for the same reason (see
    // smile.pictures.CachedBounds).
    private var boxCache: Option[Bounds] | Null = known.orNull

    def boxKnown: Boolean = boxCache != null

    def box: Option[Bounds] =
      val cached = boxCache
      if cached != null then cached
      else
        val computed = Element.union(elements.iterator.map(_.worldBounds))
        boxCache = computed
        computed

    // Cached as a group caches its hash, and not volatile for the same reason (see
    // smile.pictures.CachedHash).
    private var hashCache: Int = 0

    def hash: Int =
      val cached = hashCache
      if cached != 0 then cached
      else
        val computed = elements.hashCode
        hashCache = computed
        computed
  end Parts
end Picture

/** `child` seen only inside `rect`, a rectangle in the clip's own frame, so it turns with the clip.
  * The child keeps its own affine. The clip's bounds are its rectangle under its affine, even where
  * the child does not fill it, and its anchor starts in the middle of the rectangle.
  *
  * `crop` and `blank` in [[smile.pictures]] make clips. A blank clips the empty picture.
  */
final class Clip private[smile] (
    givenRect: Bounds,
    private[smile] val child: Element,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends CachedHash:
  type Self = Clip

  /** The visible rectangle in the clip's own frame. */
  private[smile] val rect: Bounds = Element.normalizedBox(givenRect)

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): Clip = new Clip(rect, child, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): Clip = new Clip(rect, child, affine, p)

  private[smile] lazy val worldBounds: Option[Bounds] = boundsThrough(affine)

  private[smile] def boundsThrough(m: Affine): Option[Bounds] = Some(Element.mapBox(m, rect))

  override def equals(other: Any): Boolean = other match
    case that: Clip => (this eq that) || Trees.sameTree(this, that)
    case _          => false

  private[pictures] def hashFormula: Int = (rect, child, affine, localAnchor).hashCode

  override def toString: String = Trees.text(this)
end Clip

/** `masked` seen through `masks`, one after another in order. Each keeps what is left where it is
  * opaque, or for an inverted mask, where it is not. Its bounds are the part of its last mask that
  * is not inverted that lies within the masked content's bounds and those of its other masks that
  * are not inverted, or with only inverted masks, the masked content's, since the content shows
  * outside them. Its anchor starts at the masked content's.
  */
final class MaskGroup private[smile] (
    private[smile] val masks: Vector[(Element, Boolean)],
    private[smile] val masked: Element,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends CachedHash,
      CachedBounds:
  type Self = MaskGroup

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): MaskGroup =
    new MaskGroup(masks, masked, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): MaskGroup =
    new MaskGroup(masks, masked, affine, p)

  /** The masks that are not inverted, in order. The content shows only within each of them. */
  private[pictures] def keepingMasks: Iterator[Element] =
    masks.iterator.collect { case (mask, false) => mask }

  /** The last mask that is not inverted, if there is one. */
  private[pictures] val boundingMask: Option[Element] =
    masks.reverseIterator.collectFirst { case (mask, false) => mask }

  /** The part whose bounds the group's are measured from. It is the last mask that is not inverted,
    * or with only inverted masks, the masked content.
    */
  private[pictures] val bounding: Element = boundingMask.getOrElse(masked)

  // The turned affine this group was last measured through, and its bounds there. Not volatile, as
  // a group caches its bounds (see smile.pictures.CachedBounds). It is one field of an immutable
  // pair whose fields are final, so another thread sees null or a whole pair, and one that sees
  // another pair measures again.
  private var lastTurned: (Affine, Option[Bounds]) | Null = null

  /** The bounds of this group through `m`, if `m` is the turned affine it was last measured
    * through.
    */
  private[pictures] def measuredThrough(m: Affine): Option[Option[Bounds]] = lastTurned match
    case (turned, bounds) if turned == m => Some(bounds)
    case _                               => None

  /** Through an affine that keeps the axes, the bounding part's bounds mapped, cut to the kept box
    * mapped (see `within`). Through any other, the bounding part's bounds measured through it and
    * cut the same way, kept until the group is measured through another turned affine, so that a
    * chain of mask groups drawn turned, each measured through its own affine below the one before,
    * walks each level once (see `smile.pictures.Trees.turnedBounds`).
    */
  private[smile] def boundsThrough(m: Affine): Option[Bounds] =
    measuredThrough(m).getOrElse:
      val bounds = within(m, Element.groupBounds(Seq(bounding), bounding.worldBounds, m))
      if !m.keepsAxes then lastTurned = (m, bounds)
      bounds

  /** The box in the group's frame that the content shows within when a mask that is not inverted
    * bounds the group. It is the masked content's box cut to that of each mask that is not
    * inverted, from the bounds they have cached. With a single mask, it is the part of the mask's
    * box over the content's.
    */
  private lazy val keptBox: Option[Bounds] =
    keepingMasks.foldLeft(masked.worldBounds): (kept, mask) =>
      for
        k <- kept
        b <- mask.worldBounds
      yield Clipping.intersection(k, b)

  /** `box`, the bounding part's bounds through `m`, cut to the kept box mapped by `m` when a mask
    * that is not inverted bounds the group. The kept box is mapped without walking the parts. The
    * result is exact through an affine that keeps the axes, and a box around what the group shows
    * through any other.
    */
  private[pictures] def within(m: Affine, box: Option[Bounds]): Option[Bounds] =
    if boundingMask.isEmpty then box
    else
      for
        b    <- box
        kept <- keptBox
      yield Clipping.intersection(b, Element.mapBox(m, kept))

  override def equals(other: Any): Boolean = other match
    case that: MaskGroup => (this eq that) || Trees.sameTree(this, that)
    case _               => false

  // A group of one mask hashes as it did before a group could hold several.
  private[pictures] def hashFormula: Int = masks match
    case Vector((mask, inverted)) => (mask, masked, inverted, affine, localAnchor).hashCode
    case _                        => (masks, masked, affine, localAnchor).hashCode

  override def toString: String = Trees.text(this)
end MaskGroup

/** Builds masks.
  *
  * ```scala
  * import smile.colors.LinearGradient
  * import smile.colors.PresetColor.*
  * import smile.modeling.*
  *
  * val gradient = LinearGradient(Pos(0, 0), Pos(160, 0), Red, Blue)
  * val stripe   = Rectangle(160, 80, fillWith(gradient), None)
  * val circle   = Circle(Pos.Origin, 40, fillWith(Black), None)
  * val masked   = MaskGroup(circle, stripe)
  * val pierced  = MaskGroup(circle, stripe, inverted = true)
  * ```
  *
  * ![A circle cut out of a red-to-blue gradient](pictures/masked.png) ![The gradient with a
  * circular hole in it](pictures/pierced.png)
  */
object MaskGroup:

  /** `masked` seen through `mask`, or with `mask` cut out of it as a hole when `inverted`.
    *
    * Nothing to see gives the empty picture. That is the case with empty content, an empty mask
    * that is not inverted, or one that misses the content, so that the group's bounds would have no
    * area. An inverted empty mask cuts nothing out and gives the content itself.
    *
    * Masking a translated mask group appends a mask to the same group. Repeated masking keeps the
    * group one level deep and applies its masks in order. Rotating the group as a whole can give
    * different bounds from rotating nested groups because the bounding boxes differ. With inverted
    * masks only, the difference is at most 2^-20 pixel.
    */
  def apply(mask: Element, masked: Element, inverted: Boolean = false): Element =
    if !Element.isSomething(masked) then Empty
    else if !Element.isSomething(mask) then if inverted then masked else Empty
    else
      masked match
        case inner: MaskGroup if Clipping.isTranslation(inner.affine) =>
          // The inner group's masks and content placed by its move, and then the new mask.
          // Drawing the content through them in order draws the inner group under the new mask.
          val placed = inner.masks.map((each, flag) => (Element.placedBy(each, inner.affine), flag))
          // Its bounds are measured now, as below. Its masks lie side by side, each with its hash
          // or the parts' hashes cached, so its hash, taken when first asked for, walks no chain.
          unlessHidden(
            new MaskGroup(
              placed :+ (mask, inverted),
              Element.placedBy(inner.masked, inner.affine),
              Affine.Identity,
              inner.anchor
            )
          )
        case _ =>
          // The bounds are measured now, from what the parts have measured already, so that every
          // step of a chain of masks of masks costs the same. Left for later, the first measure
          // would walk the whole chain.
          val group = unlessHidden(
            new MaskGroup(Vector((mask, inverted)), masked, Affine.Identity, masked.anchor)
          )
          val _ = group.hashCode
          group

  /** `group` with its bounds measured, or the empty picture when a mask that is not inverted keeps
    * nothing of the content, that is, when those bounds have no area.
    */
  private def unlessHidden(group: MaskGroup): Element =
    val bounds = group.worldBounds
    val shows  = group.boundingMask.isEmpty || bounds.exists(b => b.width > 0 && b.height > 0)
    if shows then group else Empty
end MaskGroup

/** An animation, which shows `frames` one after another, each for its delay in milliseconds,
  * looping forever from `startTime`.
  *
  * The frame on display is a pure function of the clock. [[smile.pictures.AnimatedPicture.frameAt]]
  * gives it for an instant, which makes it deterministic to test. The bounds are the union of every
  * frame's, so a layout stays still while the animation runs. The anchor starts at the first
  * frame's.
  *
  * Equality covers the frames, their delays, the start time, the affine and the anchor, so equal
  * animations show the same frame at every instant. A renderer draws the frame at the instant it is
  * given (see [[smile.infrastructure.Renderer.createBitmapFrom]]).
  */
final class AnimatedPicture private[smile] (
    private[smile] val frames: Vector[Element],
    private[smile] val frameDelays: Vector[Int],
    val startTime: Long,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends CachedHash,
      CachedBounds:
  type Self = AnimatedPicture

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): AnimatedPicture =
    new AnimatedPicture(frames, frameDelays, startTime, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): AnimatedPicture =
    new AnimatedPicture(frames, frameDelays, startTime, affine, p)

  // The frames with their box cached, as a picture caches its children's.
  private val frameParts = new Picture.Parts(frames, None)

  private def framesBox: Option[Bounds] = frameParts.box

  private[smile] def boundsThrough(m: Affine): Option[Bounds] =
    Element.groupBounds(frames, framesBox, m)

  /** The length of one cycle of the animation in milliseconds. */
  def cycleLength: Long = frameDelays.foldLeft(0L)(_ + _)

  /** The frame on display at the instant `now`, in milliseconds on the scale of
    * `System.currentTimeMillis()`, placed as this animation places it. When every delay is zero,
    * the first frame is on display.
    */
  def frameAt(now: Long): Element = Element.placedBy(frames(frameIndexAt(now)), affine)

  /** The frame on display `seconds` into the animation, independent of the clock, placed as this
    * animation places it. Negative times wrap around the cycle.
    *
    * @throws IllegalArgumentException
    *   if the time is not finite
    */
  def frameAtElapsed(seconds: Double): Element =
    require(seconds.isFinite, s"Elapsed animation time must be finite, not $seconds")
    if cycleLength <= 0 then Element.placedBy(frames.head, affine)
    else
      val milliseconds = (seconds % (cycleLength.toDouble / 1000)) * 1000
      frameAt(startTime + math.floor(milliseconds).toLong)

  /** The frame on display now, by the system clock. */
  def currentFrame: Element = frameAt(System.currentTimeMillis())

  /** The index of the frame on display at the instant `now`. */
  private[smile] def frameIndexAt(now: Long): Int =
    val cycle = cycleLength
    if cycle <= 0 then 0
    else
      // Scala's % keeps the dividend's sign, so a negative remainder moves up into the cycle.
      val elapsed   = (now - startTime) % cycle
      var remaining = if elapsed < 0 then elapsed + cycle else elapsed
      var index     = 0
      while index < frameDelays.size - 1 && remaining >= frameDelays(index) do
        remaining -= frameDelays(index)
        index += 1
      index

  override def equals(other: Any): Boolean = other match
    case that: AnimatedPicture => (this eq that) || Trees.sameTree(this, that)
    case _                     => false

  private[pictures] def hashFormula: Int =
    (frames, frameDelays, startTime, affine, localAnchor).hashCode

  override def toString: String = Trees.text(this)
end AnimatedPicture

/** Builds and loads animations. */
object AnimatedPicture:

  /** An animation of `frames`, each shown for its delay in `frameDelays` milliseconds, starting at
    * `startTime` on the scale of `System.currentTimeMillis()`. An animation of nothing but empty
    * pictures is the empty picture.
    *
    * @throws IllegalArgumentException
    *   if there are no frames, the delays are not one per frame, or a delay is negative
    */
  def apply(
      frames: Seq[Element],
      frameDelays: Seq[Int],
      startTime: Long = System.currentTimeMillis()
  ): Element =
    val animation = ofFrames(frames, frameDelays, startTime)
    if !frames.exists(Element.isSomething) then Empty else animation

  /** The animated GIF at `path`, a resource path, a file path or a URL, loaded at once. In a
    * browser, it must have been loaded with [[smile.pictures.AnimatedPicture.load]] first.
    *
    * @throws java.io.IOException
    *   if the image cannot be found or loaded
    */
  def fromGifPath(path: String): AnimatedPicture = ResourceFactory().animatedPictureFromPath(path)

  /** Loads the animated GIF at `path` asynchronously, on every platform. In a browser it is the
    * only way to load one the first time, and afterwards
    * [[smile.pictures.AnimatedPicture.fromGifPath]] serves the same path.
    */
  def load(path: String): Future[AnimatedPicture] = ResourceFactory().loadAnimatedPicture(path)

  /** An animation of `frames` as [[smile.pictures.AnimatedPicture.apply]] makes it, even when every
    * frame is empty.
    */
  private[smile] def ofFrames(
      frames: Seq[Element],
      frameDelays: Seq[Int],
      startTime: Long = System.currentTimeMillis()
  ): AnimatedPicture =
    require(frames.nonEmpty, "An animated picture requires at least one frame")
    require(
      frameDelays.size == frames.size,
      s"Expected one frame delay per frame (${frames.size} frames, ${frameDelays.size} delays)"
    )
    require(
      frameDelays.forall(_ >= 0),
      s"Frame delays cannot be negative (were: ${frameDelays.mkString(", ")})"
    )
    new AnimatedPicture(
      frames.toVector,
      frameDelays.toVector,
      startTime,
      Affine.Identity,
      frames.head.anchor
    )
end AnimatedPicture

/** The internal form of a wrap shift, which is `child` moved by `dx` and `dy` around `period`, a
  * rectangle in the wrap's own frame, so that what leaves one edge comes back at the opposite one.
  *
  * The period is the child's bounds when the wrap is made, and the offsets are kept within the
  * period's size, at least 0 and less than it. The wrap's bounds are its period under its affine,
  * and its anchor starts at the child's. It is drawn as copies of the child under a clip of the
  * period. Where the period lies on whole device pixels, those are up to four moved copies of the
  * child itself. Elsewhere, and always in SVG output, they are copies of one tile that the child is
  * drawn into (see `smile.infrastructure.DrawPlan`).
  *
  * `wrapShift` (see [[smile.pictures]]) makes wraps. A shift of a wrap that has only been moved
  * adds to its offsets, which keeps one wrap.
  */
final class Wrap private[smile] (
    givenPeriod: Bounds,
    private[smile] val child: Element,
    givenDx: Double,
    givenDy: Double,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends CachedHash:
  type Self = Wrap

  /** The rectangle the child wraps around, in the wrap's own frame. */
  private[smile] val period: Bounds = Element.normalizedBox(givenPeriod)

  /** The offset to the right, at least 0 and less than the period's width. */
  private[smile] val dx: Double = givenDx + 0.0

  /** The offset down, at least 0 and less than the period's height. */
  private[smile] val dy: Double = givenDy + 0.0

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): Wrap = new Wrap(period, child, dx, dy, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): Wrap = new Wrap(period, child, dx, dy, affine, p)

  private[smile] lazy val worldBounds: Option[Bounds] = boundsThrough(affine)

  private[smile] def boundsThrough(m: Affine): Option[Bounds] = Some(Element.mapBox(m, period))

  override def equals(other: Any): Boolean = other match
    case that: Wrap => (this eq that) || Trees.sameTree(this, that)
    case _          => false

  private[pictures] def hashFormula: Int = (period, child, dx, dy, affine, localAnchor).hashCode

  override def toString: String = Trees.text(this)
end Wrap

/** A raster image, which is pixels, or a picture rendered when its pixels are asked for, placed in
  * the world by an affine.
  *
  * Its local frame is where its source lays its pixels, and its bounds are the box around those
  * pixels under its affine. A bitmap turned by 45 degrees is as wide as its turned corners. The
  * bitmap's own turns, flips and scalings, and its own `crop` and `wrapShift`, are also recorded as
  * steps of its source, so that its pixels at one buffer pixel per logical pixel are the ones the
  * buffer algorithms give, and a `Stepwise` output and a recoloring can replay them. Consecutive
  * turns fold into one step by their total angle and crops into one crop, so a bitmap turned on
  * every tick keeps one step. A transform no step can express, such as a scaling to less than a
  * pixel, only composes into the affine, and so does a transform of a picture the bitmap is in.
  *
  * Its anchor starts in the middle of its pixels. Its own crop is anchored in the middle of the
  * crop, as a crop of any element is, and its flips and shifts keep its anchor. A crop that is not
  * on whole pixels of the bitmap is the generic crop, a clip of the rectangle over the bitmap, and
  * only a crop with no width or no height is blank space, as for any element.
  *
  * Bitmaps are equal when they have the same source, the same affine and the same anchor, so two
  * bitmaps of the same pixels made separately are not equal, and neither is a bitmap and the same
  * bitmap scaled or flipped and back. Turns that add up to none give back the source from before
  * them, so a bitmap turned and turned back is the bitmap it was.
  *
  * [[smile.pictures.Bitmap$]] builds bitmaps.
  */
final class Bitmap private[smile] (
    private[smile] val source: PixelSource,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends Element:
  type Self = Bitmap

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): Bitmap =
    new Bitmap(source, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): Bitmap = new Bitmap(source, affine, p)

  private[smile] lazy val worldBounds: Option[Bounds] = boundsThrough(affine)

  private[smile] def boundsThrough(m: Affine): Option[Bounds] = Rasters.boundsThrough(source, m)

  override private[smile] def turnedAround(degrees: Double, pivot: Pos): Bitmap =
    Rasters.turned(this, degrees, pivot)

  override private[smile] def scaledAround(
      horizontal: Double,
      vertical: Double,
      pivot: Pos,
      scaling: Affine
  ): Element =
    Rasters.scaled(this, horizontal, vertical, pivot, scaling)

  override private[smile] def mirrored(horizontally: Boolean, vertically: Boolean): Bitmap =
    Rasters.mirrored(this, horizontally, vertically)

  /** This bitmap cropped to the world rectangle `world`, with the bounds and the anchor of any
    * element's crop, which are the rectangle, anchored in its middle. Where the rectangle lies on
    * whole pixels of this bitmap, which only its own operations have placed, the result is a
    * [[smile.pictures.Bitmap]] that records the crop, so that its pixels are exactly the ones
    * cropped and a crop of a crop is one crop. Otherwise it is the generic crop (see
    * [[smile.pictures.crop]]), a clip of the rectangle over this bitmap.
    *
    * @throws IllegalArgumentException
    *   if a corner of `world` is not finite
    */
  def crop(world: Bounds): Element = Rasters.crop(this, world)

  /** This bitmap's content moved right by `dx` and down by `dy` with wraparound, as the generic
    * `wrapShift` of [[smile.pictures]] moves it. The bounds and the anchor stay where they are.
    * Where this bitmap has one pixel per logical pixel on whole logical pixels, and only its own
    * operations have placed it, the shift is recorded on its pixels, shifts in a row fold into one,
    * and the result is a [[smile.pictures.Bitmap]]. Otherwise it is the generic wrap shift.
    */
  def wrapShift(dx: Int, dy: Int): Element = Rasters.wrapShifted(this, dx, dy)

  /** This bitmap's pixels at one buffer pixel per logical pixel, computed with `settings` at this
    * call, as a read-only view.
    *
    * While this bitmap's transform only moves it, which covers every transform of its own that a
    * step records, they are the pixels `smile.pictures.PixelSource.buffer` gives for its source. A
    * transform no step expresses, such as a scaling to less than a pixel or a transform of a
    * picture it was in, is drawn through the shared draw plan instead, and the pixels are the whole
    * pixels covering this bitmap's bounds. Those are rendered once for the last settings asked for.
    */
  def pixels(settings: RenderSettings = RenderSettings.Default): Pixels =
    new Pixels(nativeBuffer(settings))

  /** The buffer behind [[smile.pictures.Bitmap.pixels]], which may be shared with other bitmaps and
    * must not be changed.
    */
  private[smile] def nativeBuffer(settings: RenderSettings = RenderSettings.Default): NativeBuffer =
    val now = System.currentTimeMillis()
    if Clipping.isTranslation(affine) then source.buffer(settings, now)
    else rendered(settings).buffer(settings, now)

  /** The color of the pixel at column `x` and row `y` of [[smile.pictures.Bitmap.pixels]] for
    * `settings`.
    */
  def getColor(x: Int, y: Int, settings: RenderSettings = RenderSettings.Default): Color =
    if Clipping.isTranslation(affine) then source.getColor(x, y, settings)
    else rendered(settings).getColor(x, y, settings)

  /** The color of the pixel at column `x` and row `y` of [[smile.pictures.Bitmap.pixels]] for
    * `settings`, or `None` outside them.
    */
  def colorAt(x: Int, y: Int, settings: RenderSettings = RenderSettings.Default): Option[Color] =
    pixels(settings).colorAt(x, y)

  /** How this bitmap would like to be resampled where an output in `Direct` mode draws it at
    * another size than its pixels'. A `Stepwise` output ignores it. See
    * [[smile.pictures.ResamplingHint]].
    */
  def resampling: ResamplingHint = source.hint

  /** This bitmap with another resampling hint. `Pixelated` draws nearest neighbor and `Smooth`
    * bilinear in `Direct` mode, whatever the settings say, and `Auto` follows the settings'
    * `bufferScalingMethod`. The pixels of a `Stepwise` output are the same for every hint, and the
    * bitmap's own operations keep the hint.
    */
  def withResampling(hint: ResamplingHint): Bitmap =
    new Bitmap(source.withResampling(hint), affine, localAnchor)

  /** This bitmap with each pixel's color replaced by what `transformer` gives for it, from the
    * pixels for `settings`, as [[smile.pictures.Bitmap.transformARGB]] transforms them.
    */
  def transformColorToColor(transformer: Color => Color, settings: RenderSettings): Bitmap =
    transformARGB(argb => transformer(new Color(argb)).toARGBInt, settings)

  /** This bitmap with each pixel replaced by what `transformer` gives for it as a packed ARGB
    * integer, which boxes no [[smile.colors.Color]] per pixel. The pixels are this bitmap's
    * [[smile.pictures.Bitmap.pixels]] for `settings`, and the function runs at once, once per
    * pixel, row by row.
    */
  def transformARGB(transformer: Int => Int, settings: RenderSettings): Bitmap =
    withPixelsOf(settings)(_.transformARGB(transformer, settings))

  /** This bitmap with the color of each pixel given by `generator` from its column and row. */
  def setColorsByLocation(generator: (Int, Int) => Color): Bitmap =
    // Only the size and place of the pixels are used, which are the same for every settings.
    withPixelsOf(RenderSettings.Default)(_.setColorsByLocation(generator))

  /** This bitmap's pixels merged with those of `another`, pixel by pixel with `pixelMerger`, from
    * the upper left corners of both, over as many pixels as both have, as
    * [[smile.pictures.Bitmap.mergeARGBWith]] merges them.
    */
  def mergeWith(
      another: Bitmap,
      pixelMerger: (Color, Color) => Color,
      settings: RenderSettings
  ): Bitmap =
    mergeARGBWith(
      another,
      (these, those) => pixelMerger(new Color(these), new Color(those)).toARGBInt,
      settings
    )

  /** This bitmap's pixels merged with those of `another`, as [[smile.pictures.Bitmap.mergeWith]]
    * merges them, with a function on packed ARGB integers, which boxes no [[smile.colors.Color]]
    * per pixel. The pixels of both are their [[smile.pictures.Bitmap.pixels]] for `settings`, and
    * the merge runs at once.
    */
  def mergeARGBWith(
      another: Bitmap,
      pixelMerger: (Int, Int) => Int,
      settings: RenderSettings
  ): Bitmap =
    val those = another.frozen(settings).source
    withPixelsOf(settings)(_.mergeARGBWith(those, pixelMerger, settings))

  /** Saves this bitmap's pixels for `settings` as a PNG image at `path`, and tells whether that
    * worked. Only the JVM can save files. In a browser, this always gives `false`.
    */
  def saveAsPngTo(path: String, settings: RenderSettings = RenderSettings.Default): Boolean =
    nativeBuffer(settings).saveToPath(path)

  /** This bitmap with its pixels as a source of their own. That is its source while its affine only
    * moves it, and otherwise its rendering with `settings` at one buffer pixel per logical pixel,
    * laid where it is drawn and anchored where it is.
    */
  private def frozen(settings: RenderSettings): Bitmap =
    if Clipping.isTranslation(affine) then this
    else Rasters.anchoredAs(Rasters.of(rendered(settings)), this)

  /** This bitmap with the pixels that `change` makes from its own, frozen with `settings`, laid
    * where they are and anchored where it is.
    */
  private def withPixelsOf(settings: RenderSettings)(change: PixelSource => PixelSource): Bitmap =
    val pixels = frozen(settings)
    new Bitmap(change(pixels.source), pixels.affine, pixels.localAnchor)

  // The last rendering through the draw plan, with the settings it was made with. It is immutable,
  // so a thread that reads a stale entry only renders again.
  @volatile private var lastRendering: (RenderSettings, PixelSource) | Null = null

  /** This bitmap rendered through the platform's renderer of the draw plan at one device pixel per
    * logical pixel.
    */
  private def rendered(settings: RenderSettings): PixelSource =
    val cached = lastRendering
    if cached != null && cached._1 == settings then cached._2
    else
      val bitmap = PlatformSpecific.renderPlan(this, 1.0, settings, System.currentTimeMillis())
      lastRendering = (settings, bitmap)
      bitmap

  override def equals(other: Any): Boolean = other match
    case that: Bitmap =>
      (this eq that) || ((source eq that.source) && affine == that.affine &&
        localAnchor == that.localAnchor)
    case _ => false

  override lazy val hashCode: Int = (System.identityHashCode(source), affine, localAnchor).hashCode

  override def toString: String =
    s"Bitmap(${source.pixelSize._1}x${source.pixelSize._2}, $affine, anchor = $localAnchor)"
end Bitmap

/** Builds bitmaps. A bitmap of pixels is centered at the origin with its anchor there, in the
  * middle of its pixels, and a bitmap of a picture covers the whole pixels the picture's bounds
  * touch, anchored where the picture is.
  */
object Bitmap:
  // The factories are defined here because an export from Rasters is unsafe. Code compiled together
  // with the library's sources could resolve Bitmap(...) to the private constructor of the class
  // before the forwarders of the export exist.

  /** A bitmap of `width` by `height` pixels whose colors `generator` gives by column and row.
    *
    * ```scala
    * import smile.colors.Color
    *
    * val plaid = Bitmap(120, 80, (x, y) => Color(x * 2, y * 3, 160))
    * ```
    *
    * ![A bitmap shading from blue to pink and green across its pixels](pictures/plaid.png)
    */
  def apply(width: Int, height: Int, generator: (Int, Int) => Color): Bitmap =
    Rasters.of(PixelSource(width, height, generator))

  /** A bitmap of `width` by `height` pixels of `colors`, row by row from the top. */
  def apply(width: Int, height: Int, colors: Seq[Color]): Bitmap =
    Rasters.of(PixelSource(width, height, colors))

  /** A bitmap of the image at the resource path `path`, which may also be a file path or a URL. In
    * a browser, the image must have been loaded with [[smile.pictures.Bitmap.load]] first.
    */
  def apply(path: String): Bitmap = Rasters.of(PixelSource(path))

  /** A bitmap of `picture` rendered at once with `settings`, as `toBitmap` renders it. */
  def apply(picture: Element, settings: RenderSettings): Bitmap =
    Renderer().createBitmapFrom(picture, 1.0, settings)

  export Rasters.{fromARGB, generateARGB, load, of}
end Bitmap

/** A static SVG document, kept as vectors and drawn under an affine at the output's own density.
  *
  * Its local frame is the document's own, from the origin to its width and height, and its bounds
  * are that box under its affine, whatever the document paints inside it. Its anchor starts in the
  * middle of the document.
  *
  * Moves, scalings, turns and flips keep it a vector picture, since each only changes its affine.
  * Only what needs its pixels turns it into pixels, such as `toBitmap`, which renders it at the
  * settings' scale. The document is opaque to recoloring. Its paints are not rewritten, so a
  * [[smile.pictures.Recolor]] whose function changes a color the document shows gives a bitmap of
  * the recolored pixels alone.
  *
  * SVG pictures are equal when they have the same document, the same affine and the same anchor, so
  * two pictures parsed from the same text separately are not equal.
  *
  * In the browser, a document is decoded after it is parsed. The browser starts when the picture is
  * made, and until it is done, canvas output draws nothing in the picture's place, and no rendering
  * that left it out is kept. So a bitmap of a picture that holds it, made with
  * `Bitmap.of(picture)`, is transparent there and rendered again at every read and draw until then,
  * and shows the picture afterwards. Pixels taken at once keep what they showed. Those are the
  * pixels of `toBitmap`, which renders when it is called, and of a color operation such as
  * `transformARGB`. Await the picture's `ready` first, or load it with
  * [[smile.pictures.SvgPicture.load]], which gives a picture that is ready. SVG output needs no
  * decoding.
  *
  * [[smile.pictures.SvgPicture$]] parses and loads them.
  */
final class SvgPicture private[smile] (
    private[smile] val document: SvgDocument,
    private[smile] val affine: Affine,
    givenLocalAnchor: Pos
) extends Element:
  type Self = SvgPicture

  private[smile] val localAnchor: Pos = Element.normalizedAnchor(affine, givenLocalAnchor)

  private[smile] def withAffine(a: Affine): SvgPicture = new SvgPicture(document, a, localAnchor)

  private[smile] def withLocalAnchor(p: Pos): SvgPicture = new SvgPicture(document, affine, p)

  private[smile] lazy val worldBounds: Option[Bounds] = boundsThrough(affine)

  private[smile] def boundsThrough(m: Affine): Option[Bounds] =
    Some(m.boxBounds(0, 0, document.width, document.height))

  override def equals(other: Any): Boolean = other match
    case that: SvgPicture =>
      (this eq that) || ((document eq that.document) && affine == that.affine &&
        localAnchor == that.localAnchor)
    case _ => false

  override lazy val hashCode: Int =
    (System.identityHashCode(document), affine, localAnchor).hashCode

  override def toString: String =
    s"SvgPicture(${document.width}x${document.height}, $affine, anchor = $localAnchor)"
end SvgPicture

/** Parses and loads SVG pictures, each centered at the origin. */
object SvgPicture:
  // The factory apply is defined here because an export from SvgPictures is unsafe, for the reason
  // given at the factories of Bitmap.

  /** A picture of the SVG document at the resource path `path`, which may also be a file path or a
    * URL. It is looked up as `Bitmap(path)` is and parsed as by
    * [[smile.pictures.SvgPicture.fromString]]. In a browser, the document must have been loaded
    * with [[smile.pictures.SvgPicture.load]] first, and every picture of a path then shares its
    * document, so they are equal. On the JVM each call reads the document anew, as `Bitmap(path)`
    * does, so two pictures of a path are not equal.
    *
    * The JVM reads a file in the encoding its XML declaration names, and a browser reads it as
    * UTF-8 whatever the declaration says, so a file that is to be the same picture on both is saved
    * as UTF-8 or ASCII.
    *
    * @throws java.io.IOException
    *   naming `path`, on the JVM, if nothing is there or it cannot be read as an SVG document
    * @throws UnsupportedOperationException
    *   in a browser, if `path` has not been loaded
    */
  def apply(path: String): SvgPicture =
    SvgPictures.of(ResourceFactory().svgDocumentFromPath(path))

  export SvgPictures.{fromString, load}
end SvgPicture

/** The empty picture, with nothing drawn and no room taken. It has no bounds, it is 0x0, its anchor
  * is the origin, and every transform of it and every new anchor for it is it. It is the identity
  * for composition. Adding it to a picture, or a picture to it, gives that picture, and no gap is
  * laid out next to it. A zero scale factor gives it too.
  *
  * Its bitmap (see [[smile.pictures.toBitmap]]) has no bounds either, but its pixels are one
  * transparent pixel, since a buffer of pixels cannot have none.
  *
  * Blank space that takes room is [[smile.pictures.blank]].
  */
case object Empty extends Element:
  type Self = Empty.type

  private[smile] val affine: Affine = Affine.Identity

  private[smile] val localAnchor: Pos = Pos.Origin

  private[smile] def withAffine(a: Affine): Empty.type = this

  private[smile] def withLocalAnchor(p: Pos): Empty.type = this

  private[smile] val worldBounds: Option[Bounds] = None

  private[smile] def boundsThrough(m: Affine): Option[Bounds] = None
