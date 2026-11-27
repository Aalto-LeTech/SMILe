package smile.pictures

import smile.colors.Color
import smile.modeling.{Affine, Bounds, Pos}
import smile.pictures.StrokeStyle

/** Blank space `width` wide and `height` high, centered at the origin. It is a clip of the empty
  * picture, so its bounds are its rectangle and it draws nothing. Its anchor is the origin.
  *
  * @throws IllegalArgumentException
  *   if a size is negative or not finite
  */
def blank(width: Double, height: Double): Element =
  require(
    width >= 0 && height >= 0 && width.isFinite && height.isFinite,
    s"A blank's size must be finite and not negative, not $width by $height"
  )
  val (halfWidth, halfHeight) = (width / 2, height / 2)
  new Clip(
    Bounds(-halfWidth, -halfHeight, halfWidth, halfHeight),
    Empty,
    Affine.Identity,
    Pos.Origin
  )

extension (e: Element)

  /** What this element draws within the world rectangle `world`, with `world` as its bounds even
    * where nothing is drawn, and its anchor in the middle of `world`.
    *
    * The rectangle stays a rectangle however this element is turned, and later transforms turn it
    * with the content. A rectangle with no width or no height gives blank space of that size. A
    * crop is always a clip, even of a rectangle that is exactly this element's bounds, so its
    * anchor is always the middle of the rectangle.
    *
    * When an earlier crop has only moved, scaled, turned by quarter turns or been mirrored, both
    * crops share one clip. A crop on every tick, as a zoom makes, then stays one clip deep.
    *
    * A crop of a turned element, or a turn of a crop, is cropped to the box around the turned
    * rectangle, which is larger than the rectangle for an angle that is not a quarter turn. A
    * square's side grows by `abs(cos a) + abs(sin a)` per turn by `a`. Crop back to a fixed window
    * after each step of a fold to keep its bounds from growing.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val star = Star5(40, 16, Pos.Origin, fillWith(Gold), None)
    * val half = star.crop(Bounds(Pos(-40, -40), Pos(0, 40)))
    * ```
    *
    * ![The left half of a golden star](pictures/half.png)
    *
    * @throws IllegalArgumentException
    *   if a corner of `world` is not finite
    */
  def crop(world: Bounds): Element =
    val rect = Element.normalizedBox(world)
    if rect.width == 0 || rect.height == 0 then new Clip(rect, Empty, Affine.Identity, rect.center)
    else
      Clipping.uncropped(e) match
        case None                     => new Clip(rect, e, Affine.Identity, rect.center)
        case Some((Empty, _))         => new Clip(rect, Empty, Affine.Identity, rect.center)
        case Some((content, visible)) =>
          val inside = Clipping.intersection(rect, visible)
          if inside == rect then new Clip(rect, content, Affine.Identity, rect.center)
          else if inside.width == 0 || inside.height == 0 then
            new Clip(rect, Empty, Affine.Identity, rect.center)
          else
            val shown = new Clip(inside, content, Affine.Identity, inside.center)
            new Clip(rect, shown, Affine.Identity, rect.center)

  /** This element moved by `dx` to the right and `dy` down within its bounds, so that what leaves
    * one edge comes back at the opposite one. The bounds and the anchor stay where they are, and a
    * shift by the whole width or height gives this element back.
    *
    * Shifting a shifted element that has only been moved, turned by quarter turns or mirrored
    * since, adds to its shift. So does shifting a crop of a shift, where both have only been moved
    * since and the crop is the shift's own rectangle. Either way, any number of shifts costs the
    * same as one. After other transforms, a shift wraps the transformed element within its new
    * bounds. Those bounds are the box around a turned element, so shifting and then turning by an
    * angle that is not a quarter turn grows a square's side by `abs(cos a) + abs(sin a)` per step,
    * as `place` does.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val shape   = Star5(40, 16, Pos.Origin, fillWith(Gold), None)
    * val wrapped = shape.wrapShift(40, 0)
    * ```
    *
    * ![A star moved right by half its width, its right half wrapped around to the
    * left](pictures/wrapped.png)
    *
    * @throws IllegalArgumentException
    *   if an offset is not finite
    */
  def wrapShift(dx: Double, dy: Double): Element =
    require(dx.isFinite && dy.isFinite, s"A shift must be finite, not ($dx, $dy)")
    e match
      case wrap: Wrap if wrap.affine.keepsPixelGrid =>
        // The shift in the wrap's frame. The inverse of a signed permutation is its transpose.
        val m = wrap.affine
        Clipping.wrapped(
          wrap.period,
          wrap.child,
          wrap.dx + (m.a * dx + m.b * dy),
          wrap.dy + (m.c * dx + m.d * dy),
          m,
          wrap.localAnchor
        )
      case clip: Clip if Clipping.cropsWholeShift(clip) =>
        new Clip(clip.rect, clip.child.wrapShift(dx, dy), clip.affine, clip.localAnchor)
      case _ =>
        e.bounds match
          case None         => e
          case Some(period) => Clipping.wrapped(period, e, dx, dy, Affine.Identity, e.anchor)

  /** This element with guides for debugging. Its bounds are outlined and its anchor is marked by a
    * small cross, both in magenta and in front of it. The result is anchored where this element is.
    *
    * ```scala
    * import smile.colors.PresetColor.*
    * import smile.modeling.*
    *
    * val guided = Star5(40, 16, Pos.Origin, fillWith(Gold), None).rotateBy(20).withGuides
    * ```
    *
    * ![A turned star with its bounds and its anchor marked](pictures/guided.png)
    */
  def withGuides: Picture =
    val anchor = e.anchor
    val arm    = Clipping.GuideArm
    val cross  = Picture.of(
      Vector(
        Line(anchor - (arm, 0.0), anchor + (arm, 0.0), Clipping.Guide),
        Line(anchor - (0.0, arm), anchor + (0.0, arm), Clipping.Guide)
      ),
      None
    )
    val outline =
      e.bounds.map(b => Rectangle(b.width, b.height, b.center, None, Some(Clipping.Guide)))
    val content = if Element.isSomething(e) then Vector(e) else Vector()
    Picture.of((cross +: outline.toVector) ++ content, None).withAnchor(anchor)
end extension

/** How clips, wrap shifts and guides are made. */
private[pictures] object Clipping:

  /** How guides are drawn. */
  val Guide: StrokeStyle = StrokeStyle(Color(255, 0, 255))

  /** How far each arm of the anchor's cross reaches from the anchor, in pixels. */
  val GuideArm: Double = 4

  /** Whether `m` only moves. */
  def isTranslation(m: Affine): Boolean = m.a == 1 && m.b == 0 && m.c == 0 && m.d == 1

  /** Whether `clip` only moves a shift that only moves, and crops it to exactly its period in the
    * world, so that shifting the clip adds to that shift.
    */
  def cropsWholeShift(clip: Clip): Boolean = clip.child match
    case wrap: Wrap =>
      isTranslation(clip.affine) && isTranslation(wrap.affine) &&
      Element.mapBox(wrap.affine.andThen(clip.affine), wrap.period) ==
        Element.mapBox(clip.affine, clip.rect)
    case _ => false

  /** What the clip `e` shows and the world rectangle it shows it within, where `e` is a clip that
    * keeps the axes, that is, one only moved, scaled, turned by quarter turns or mirrored. They are
    * its child, placed as the clip places it, and the clip's rectangle there, which is the clip's
    * bounds. Where the child is a clip that keeps the axes too, they are that clip's child and both
    * rectangles' intersection. Clipping that content to a rectangle draws what clipping `e` to it
    * draws. `None` for any other element.
    */
  def uncropped(e: Element): Option[(Element, Bounds)] =
    def shown(clip: Clip): Option[(Element, Bounds)] =
      if !clip.affine.keepsAxes then None
      else Some((Element.placedBy(clip.child, clip.affine), Element.mapBox(clip.affine, clip.rect)))
    e match
      case clip: Clip =>
        shown(clip).map: (child, rect) =>
          child match
            case inner: Clip =>
              shown(inner).fold((child, rect))((content, within) =>
                (content, intersection(rect, within))
              )
            case _ => (child, rect)
      case _ => None

  /** The rectangle that both `a` and `b` cover, which is empty, with no width or no height, where
    * they do not overlap.
    */
  def intersection(a: Bounds, b: Bounds): Bounds =
    val left   = a.upperLeftCorner.x.max(b.upperLeftCorner.x)
    val top    = a.upperLeftCorner.y.max(b.upperLeftCorner.y)
    val right  = a.lowerRightCorner.x.min(b.lowerRightCorner.x).max(left)
    val bottom = a.lowerRightCorner.y.min(b.lowerRightCorner.y).max(top)
    Element.normalizedBox(Bounds(left, top, right, bottom))

  /** `child` wrapped around `period` with the offsets `dx` and `dy` brought within the period's
    * size. With no offset left, it is `child` placed by `affine` with its anchor where the wrap's
    * would be, since the period is the child's bounds and the wrap would draw the child as it is.
    */
  def wrapped(
      period: Bounds,
      child: Element,
      dx: Double,
      dy: Double,
      affine: Affine,
      localAnchor: Pos
  ): Element =
    val (x, y) = (within(dx, period.width), within(dy, period.height))
    if x == 0 && y == 0 then
      val placed = Element.placedBy(child, affine)
      val anchor = affine(localAnchor)
      if placed.anchor == anchor then placed else placed.withAnchor(anchor)
    else new Wrap(period, child, x, y, affine, localAnchor)

  /** `offset` brought to at least 0 and less than `size`, or 0 when `size` is 0, on the grid of
    * 2^-20 pixel that every move lies on, so that shifts add up as moves do.
    */
  private def within(offset: Double, size: Double): Double =
    if size == 0 then 0.0
    else
      val remainder = offset % size
      val wrapped   = if remainder < 0 then remainder + size else remainder
      val snapped   = Math.rint(wrapped * 1048576) / 1048576
      // A tiny negative remainder plus the size, or the snap, can reach the size itself.
      if snapped >= size then 0.0 else snapped + 0.0
end Clipping
