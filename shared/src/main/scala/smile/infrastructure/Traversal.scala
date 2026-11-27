package smile.infrastructure

import scala.annotation.{tailrec, unused}
import scala.collection.mutable

import smile.modeling.Affine
import smile.pictures.{
  AnimatedPicture,
  Bitmap,
  BitmapSource,
  Clip,
  Element,
  Empty,
  MaskGroup,
  Picture,
  Shape,
  SvgPicture,
  Wrap
}

/** The one internal fold over elements. The draw plan, the paints an element paints with and its
  * paints mapped through a table all walk the tree through it, so that a new kind of element is
  * handled in one place.
  */
private[smile] object Traversal:

  /** What a fold makes of each kind of element. Each case gets the element and `m`, the map from
    * the element's local frame to the frame the fold started in, so the frame its parts are placed
    * in. Group cases get what the fold made of the parts they read, each placed under `m` unless
    * the case says otherwise.
    */
  trait Fold[A]:
    def empty: A

    def shape(shape: Shape, m: Affine): A

    def bitmap(bitmap: Bitmap, m: Affine): A

    def svg(svg: SvgPicture, m: Affine): A

    /** A picture of `parts`, one for each child, front first. */
    def picture(picture: Picture, m: Affine, parts: Vector[A]): A

    def clip(clip: Clip, m: Affine, content: A): A

    /** Whether `mask` reads what the fold makes of a group's masks. */
    def foldsMasks: Boolean

    /** A mask group, with what the fold made of its masked content and, exactly when `foldsMasks`,
      * of each of its masks, in order, and otherwise of none.
      */
    def mask(group: MaskGroup, m: Affine, masks: Vector[A], masked: A): A

    /** The indices of the frames of `animation` that `animation` reads, all of them unless a fold
      * says otherwise.
      */
    def framesOf(animation: AnimatedPicture): Vector[Int] = animation.frames.indices.toVector

    /** An animation, with what the fold made of the frames that `framesOf` gives, in its order. */
    def animation(animation: AnimatedPicture, m: Affine, frames: Vector[A]): A

    /** The map that `wrap`'s content is folded under, `m` unless a fold says otherwise, or `None`
      * when `wrap` reads nothing of its content.
      */
    def wrapContent(@unused wrap: Wrap, m: Affine): Option[Affine] = Some(m)

    /** A wrap, with what the fold made of its content under the map `wrapContent` gives, if any. */
    def wrap(wrap: Wrap, m: Affine, content: Option[A]): A
  end Fold

  /** An element placed in the frame the fold starts in by `m`, waiting for what the fold makes of
    * `parts`, each placed by `under`. The fold has started on the first `started` of them, and
    * `results` holds what it made of those it has finished.
    */
  private final class Frame[A](
      val element: Element,
      val m: Affine,
      val parts: Vector[Element],
      val under: Affine
  ):
    val results: mutable.Builder[A, Vector[A]] = Vector.newBuilder[A]
    var started: Int                           = 0

  /** What `fold` makes of `element` placed by `parent` in the frame the fold starts in. It walks
    * the tree with a stack of its own, so a tree of any depth is folded without overflowing.
    */
  def fold[A](element: Element, fold: Fold[A], parent: Affine = Affine.Identity): A =
    def frame(e: Element, parent: Affine): Frame[A] =
      val m = if parent == Affine.Identity then e.affine else e.affine.andThen(parent)
      e match
        case picture: Picture => new Frame(e, m, picture.children, m)
        case clip: Clip       => new Frame(e, m, Vector(clip.child), m)
        case group: MaskGroup =>
          val parts =
            if fold.foldsMasks then group.masks.map(_._1) :+ group.masked
            else Vector(group.masked)
          new Frame(e, m, parts, m)
        case anim: AnimatedPicture => new Frame(e, m, fold.framesOf(anim).map(anim.frames), m)
        case wrap: Wrap            =>
          fold.wrapContent(wrap, m) match
            case Some(under) => new Frame(e, m, Vector(wrap.child), under)
            case None        => new Frame(e, m, Vector(), m)
        case _ => new Frame(e, m, Vector(), m)

    def finish(frame: Frame[A]): A =
      val m       = frame.m
      val results = frame.results.result()
      frame.element match
        case Empty                 => fold.empty
        case shape: Shape          => fold.shape(shape, m)
        case bitmap: Bitmap        => fold.bitmap(bitmap, m)
        case svg: SvgPicture       => fold.svg(svg, m)
        case picture: Picture      => fold.picture(picture, m, results)
        case clip: Clip            => fold.clip(clip, m, results(0))
        case group: MaskGroup      => fold.mask(group, m, results.init, results.last)
        case anim: AnimatedPicture => fold.animation(anim, m, results)
        case wrap: Wrap            => fold.wrap(wrap, m, results.headOption)

    val stack              = mutable.Stack(frame(element, parent))
    @tailrec def walk(): A =
      val top = stack.top
      if top.started < top.parts.size then
        stack.push(frame(top.parts(top.started), top.under))
        top.started += 1
        walk()
      else
        val value = finish(stack.pop())
        if stack.isEmpty then value
        else
          stack.top.results += value
          walk()
    walk()

  /** The packed ARGB colors `element` paints with. They are the paints of its shapes' fills and
    * strokes, of its bitmaps' pixel sources and of every frame of its animations. A mask lends only
    * its opacity, so only the masked content's paints count, and an SVG picture is opaque to
    * recoloring. The pictures of picture sources are walked as
    * `smile.infrastructure.Traversal.foldSources` walks them.
    */
  def paints(element: Element): Set[Int] =
    foldSources(element)(Paints(_)).foldLeft(Set.empty[Int])(_ ++ _)

  /** What the fold that `of` makes folds of `element` and of the picture of every picture source
    * reached from it, one value for each, lazily. The fold calls the function it is made with on
    * the picture of each picture source it meets, and each distinct picture is folded once however
    * many bitmaps share it, so a chain of pictures that each draw the bitmap before twice is folded
    * once per level. The pictures are walked with a stack of their own, however deep they nest.
    */
  def foldSources[A](element: Element)(of: (Element => Unit) => Fold[A]): Iterator[A] =
    val seen    = new java.util.IdentityHashMap[Element, Element]
    val pending = mutable.Stack(element)
    val f       = of: picture =>
      if !seen.containsKey(picture) then
        val _ = seen.put(picture, picture)
        val _ = pending.push(picture)
    new Iterator[A]:
      def hasNext: Boolean = pending.nonEmpty
      def next(): A        = Traversal.fold(pending.pop(), f)

  /** The pictures of the picture sources of the bitmaps in `element`, in its masks and every frame
    * of its animations too. The picture sources inside those pictures are left out.
    */
  def sourcePictures(element: Element): Vector[Element] = fold(element, SourcePictures)

  private object SourcePictures extends Fold[Vector[Element]]:
    def foldsMasks: Boolean                                = true
    def empty: Vector[Element]                             = Vector()
    def shape(shape: Shape, m: Affine): Vector[Element]    = Vector()
    def bitmap(bitmap: Bitmap, m: Affine): Vector[Element] = bitmap.source.source match
      case BitmapSource.Content(picture) => Vector(picture)
      case BitmapSource.Pixels(_)        => Vector()
    def svg(svg: SvgPicture, m: Affine): Vector[Element] = Vector()
    def picture(picture: Picture, m: Affine, parts: Vector[Vector[Element]]): Vector[Element] =
      parts.flatten
    def clip(clip: Clip, m: Affine, content: Vector[Element]): Vector[Element] = content
    def mask(
        group: MaskGroup,
        m: Affine,
        masks: Vector[Vector[Element]],
        masked: Vector[Element]
    ): Vector[Element] = masks.flatten ++ masked
    def animation(
        animation: AnimatedPicture,
        m: Affine,
        frames: Vector[Vector[Element]]
    ): Vector[Element] = frames.flatten
    def wrap(wrap: Wrap, m: Affine, content: Option[Vector[Element]]): Vector[Element] =
      content.getOrElse(Vector())
  end SourcePictures

  /** `element` with every color it paints with replaced through `table`, which maps packed ARGB
    * colors, and its geometry, affines and anchors as they are. Masks keep their own colors, since
    * they lend only their opacity, and SVG pictures are returned unchanged.
    */
  def mapPaints(element: Element, table: Int => Int): Element = fold(element, MappedPaints(table))

  /** The paints of an element outside the pictures of its picture sources, which it gives `meet` as
    * it meets them, since their paints are those of pictures of their own.
    */
  private final class Paints(meet: Element => Unit) extends Fold[Set[Int]]:
    def empty: Set[Int] = Set.empty

    def shape(shape: Shape, m: Affine): Set[Int] =
      shape.fill.fold(Set.empty[Int])(_.paint.paints) ++
        shape.stroke.fold(Set.empty[Int])(_.paint.paints)

    def bitmap(bitmap: Bitmap, m: Affine): Set[Int] = bitmap.source.source match
      case BitmapSource.Pixels(buffer)   => buffer.argbData.toSet
      case BitmapSource.Content(picture) =>
        meet(picture)
        bitmap.source.checkpoint.fold(Set.empty[Int])(_.pixels.argbData.toSet)

    def svg(svg: SvgPicture, m: Affine): Set[Int] = Set.empty

    def picture(picture: Picture, m: Affine, parts: Vector[Set[Int]]): Set[Int] =
      parts.foldLeft(Set.empty[Int])(_ ++ _)

    def clip(clip: Clip, m: Affine, content: Set[Int]): Set[Int] = content

    def foldsMasks: Boolean = false

    def mask(group: MaskGroup, m: Affine, masks: Vector[Set[Int]], masked: Set[Int]): Set[Int] =
      masked

    def animation(animation: AnimatedPicture, m: Affine, frames: Vector[Set[Int]]): Set[Int] =
      frames.foldLeft(Set.empty[Int])(_ ++ _)

    def wrap(wrap: Wrap, m: Affine, content: Option[Set[Int]]): Set[Int] =
      content.getOrElse(Set.empty)
  end Paints

  private final class MappedPaints(table: Int => Int) extends Fold[Element]:
    def empty: Element = Empty

    def shape(shape: Shape, m: Affine): Element =
      new Shape(
        shape.shape,
        shape.fill.map(_.mapPaints(table)),
        shape.stroke.map(_.mapPaints(table)),
        shape.affine,
        shape.localAnchor
      )

    def bitmap(bitmap: Bitmap, m: Affine): Element =
      new Bitmap(bitmap.source.mapPaints(table), bitmap.affine, bitmap.localAnchor)

    def svg(svg: SvgPicture, m: Affine): Element = svg

    def picture(picture: Picture, m: Affine, parts: Vector[Element]): Element =
      // Colors do not move anything, so the parts keep the box the picture has cached for them.
      new Picture(
        new Picture.Parts(parts, Some(picture.parts.box)),
        picture.affine,
        picture.localAnchor
      )

    def clip(clip: Clip, m: Affine, content: Element): Element =
      new Clip(clip.rect, content, clip.affine, clip.localAnchor)

    def foldsMasks: Boolean = false

    def mask(group: MaskGroup, m: Affine, masks: Vector[Element], masked: Element): Element =
      new MaskGroup(group.masks, masked, group.affine, group.localAnchor)

    def animation(animation: AnimatedPicture, m: Affine, frames: Vector[Element]): Element =
      new AnimatedPicture(
        frames,
        animation.frameDelays,
        animation.startTime,
        animation.affine,
        animation.localAnchor
      )

    def wrap(wrap: Wrap, m: Affine, content: Option[Element]): Element =
      new Wrap(
        wrap.period,
        content.getOrElse(wrap.child),
        wrap.dx,
        wrap.dy,
        wrap.affine,
        wrap.localAnchor
      )
  end MappedPaints
end Traversal
