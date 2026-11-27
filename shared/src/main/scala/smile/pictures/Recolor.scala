package smile.pictures

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import smile.RenderSettings
import smile.colors.Color
import smile.infrastructure.{Completeness, PlatformSpecific, Traversal}
import smile.modeling.{Affine, Bounds, Pos}

/** Recolors a picture with a function on packed ARGB colors, as a bitmap's color operation does,
  * while keeping the picture's shapes where the function lets it.
  *
  * The result's pixels are always exactly those of the picture rendered with the given settings and
  * transformed pixel by pixel, which is what
  * `picture.toBitmap(settings).transformARGB(f, settings)` gives, or for a bitmap that only its own
  * operations have placed, its own pixels for the settings transformed, as
  * `bitmap.transformARGB(f, settings)` gives. The function is called exactly as there, once for
  * each pixel of that render, row by row, top row first.
  *
  * Recoloring goes in four steps:
  *
  *   1. **Observe.** While the function is called, a table of what it gave for each color is kept,
  *      along with whether it ever gave two answers for one color. A picture that is only a bitmap
  *      of pixels, holds an animation or covers no whole pixel falls back whatever the function
  *      gives, so for it no table is kept.
  *   1. **Fall back.** The result is a bitmap of the pixels alone when the function did that, when
  *      the picture paints with a color that no pixel showed it, when the table has more than 4096
  *      colors, when the picture is only a bitmap of pixels, when it holds an animation, when it
  *      covers no whole pixel, or when the rebuilt picture would nest more than 8 pictures deep
  *      inside bitmaps of pictures. An SVG picture's document is not recolored, so a picture that
  *      shows one falls back too, at the verification below, whenever the function changes a color
  *      the document shows.
  *   1. **Rebuild.** Otherwise the picture is rebuilt with the colors of its fills, strokes, text,
  *      gradient stops and bitmap pixels replaced through the table. A mask lends only its opacity,
  *      so it stays as it is. When the function gave transparent pixels a visible color, a
  *      rectangle of that color goes behind the picture, on the pixels of its frame. The rebuilt
  *      picture is clipped to that frame, the whole pixels of the render.
  *   1. **Verify.** The rebuilt picture is rendered with the same settings. When its pixels are the
  *      observed ones, the result is a bitmap of the rebuilt picture whose checkpoint is the
  *      observed pixels for the settings, which an output with those settings takes and replays any
  *      later operation on. Any other output renders the rebuilt picture, a `Direct` one at its own
  *      scale. Otherwise the result is the bitmap of the pixels alone.
  */
object Recolor:

  /** The most colors the table may have for the picture to be rebuilt. */
  private val MaximumColors = 4096

  /** The deepest pictures may nest inside bitmaps of pictures in a rebuilt picture. That bounds
    * what a picture recolored again and again keeps alive where it cannot stay as shallow as it
    * started, such as when it is placed among other elements before every recolor.
    */
  private val MaximumDepth = 8

  /** Recolors `picture` with `f`.
    *
    * @param f
    *   A function from a packed ARGB pixel to its replacement.
    * @param settings
    *   The settings the picture is rendered with, whose pixels `f` transforms. A bitmap that only
    *   its own operations have placed is its own pixels, computed with these settings, as a
    *   bitmap's color operations compute them.
    * @return
    *   A bitmap whose pixels are the transformed ones, anchored where the picture is.
    */
  def apply(picture: Element, f: Int => Int, settings: RenderSettings): Bitmap =
    val single = picture match
      case bitmap: Bitmap if Clipping.isTranslation(bitmap.affine) => Some(bitmap)
      case _                                                       => None
    val base     = single.getOrElse(picture.toBitmap(settings))
    val ofPixels = single.exists:
      _.source.source match
        case BitmapSource.Pixels(_) => true
        case _                      => false
    val hasVisibleArea = PixelSource.frameOf(picture).isDefined

    // These fall back whatever the function gives, so it is not observed.
    if ofPixels || !hasVisibleArea || animates(picture) then base.transformARGB(f, settings)
    else
      val observer = new Observer(f)
      val fallback = base.transformARGB(observer, settings)
      val observed = fallback.nativeBuffer()
      if observer.gaveUp then fallback
      else
        val table    = observer.table
        val frame    = pixelBox(base)
        val prepared = prepare(picture, single, frame)
        // A color no pixel showed cannot be mapped, because the function never saw it. A paint that
        // paints nothing needs no mapping, and it is left as it is. Only the paints the rebuilt
        // picture keeps count. The checkpoint of an earlier recolor is left out.
        val unseen = Traversal
          .paints(prepared)
          .exists(paint => (paint >>> 24) != 0 && !table.contains(paint))
        if unseen || deeperThan(prepared, MaximumDepth) then fallback
        else
          val lookup: Int => Int = pixel => table.getOrElse(pixel, pixel)
          // The frame's corner is moved onto a whole pixel, as every rendering lays its pixels, and
          // the bitmap is moved back by what that took.
          val corner   = frame.upperLeftCorner
          val (dx, dy) = (corner.x - corner.x.floor, corner.y - corner.y.floor)
          val mapped   = withBackground(Traversal.mapPaints(prepared, lookup), frame, table)
          val rebuilt  = new Clip(
            frame.moveBy(-dx, -dy),
            Element.placedBy(mapped, Affine.translation(-dx, -dy)),
            Affine.Identity,
            Pos.Origin
          )
          val now               = System.currentTimeMillis()
          val (rendered, whole) = Completeness.tracked:
            PlatformSpecific.renderPlan(rebuilt, 1.0, settings, now).buffer(settings, now)
          val same =
            rendered.width == observed.width && rendered.height == observed.height &&
              java.util.Arrays.equals(rendered.argbData, observed.argbData)
          // Pixels that left something out, as an SVG not decoded yet, are no checkpoint.
          if !same || !whole then fallback
          else
            val recolored = PixelSource.checkpointed(rebuilt, observed, settings)
            val hinted    =
              if base.resampling == recolored.hint then recolored
              else recolored.withResampling(base.resampling)
            val placed = new Bitmap(hinted, Affine.translation(dx, dy), hinted.boundary.center)
            Rasters.anchoredAs(placed, picture)
  end apply

  /** Where the pixels of `bitmap`, which its affine only moves, lie in the world. */
  private def pixelBox(bitmap: Bitmap): Bounds =
    bitmap.source.boundary.moveBy(bitmap.affine.e, bitmap.affine.f)

  /** The picture to rebuild, before its colors are replaced and before it is clipped to `frame`,
    * where the pixels of its render lie. Those are one bitmap's own pixels, or the whole pixels any
    * other picture's bounds cover.
    *
    * A single bitmap is rebuilt in one of three ways. A bitmap of a picture with no operations
    * recorded, such as the result of an earlier recolor, is rebuilt as that picture, moved to where
    * the bitmap is. A bitmap of a picture with a checkpoint and operations recorded since, such as
    * an earlier recolor operated on, keeps its operations. They go on the bitmap inside its picture
    * when the picture is only that (see `collapsed`), and otherwise on the picture, without the
    * checkpoint. The checkpoint, the pixels an earlier recolor observed, is left behind, as the
    * verification decides on the new ones. Both ways keep a picture recolored again and again one
    * level deep, and both need a bitmap that keeps every step since its picture. Any other bitmap
    * is rebuilt as a picture of the bitmap.
    */
  private def prepare(picture: Element, single: Option[Bitmap], frame: Bounds): Element =
    single match
      case Some(bitmap) =>
        val source = bitmap.source.moveBy(bitmap.affine.e, bitmap.affine.f)
        source.source match
          case BitmapSource.Content(content) if source.steps.isEmpty && source.complete =>
            val drawn  = PixelSource.frameOf(content).fold(frame)(identity)
            val offset = frame.upperLeftCorner - drawn.upperLeftCorner
            // An earlier recolor's picture is clipped to its own frame, which the new frame
            // replaces, so that the clips do not nest one deeper with every recolor.
            val inner = content match
              case clip: Clip if clip.affine == Affine.Identity && clip.rect == drawn => clip.child
              case other                                                              => other
            if offset.x == 0 && offset.y == 0 then inner else inner.moveBy(offset.x, offset.y)
          case BitmapSource.Content(content) if source.checkpoint.isDefined && source.complete =>
            // An earlier recolor with operations since. Its operations go on the bitmap inside it
            // when there is one, and otherwise on its picture, without its checkpoint. Either way
            // the pictures do not nest one level deeper with every recolor.
            val operated = collapsed(source, content).getOrElse(source.withoutCheckpoint)
            Rasters.of(operated)
          case _ => Rasters.of(source)
      case None => picture

  /** The bitmap inside `content` with the operations of `bitmap` recorded on it, laid where
    * `bitmap` is, when `content` is only that bitmap, clipped to its own pixels at one buffer pixel
    * per logical pixel. That is the picture an earlier recolor of a bitmap with operations rebuilt,
    * and recording the operations on the bitmap inside it gives the same pixels as recording them
    * on a bitmap of it.
    */
  private def collapsed(bitmap: PixelSource, content: Element): Option[PixelSource] =
    content match
      case clip: Clip if clip.affine == Affine.Identity =>
        clip.child match
          case inner: Bitmap if Clipping.isTranslation(inner.affine) =>
            val placed          = inner.source.moveBy(inner.affine.e, inner.affine.f)
            val (width, height) = placed.pixelSize
            val fits            =
              clip.rect == placed.boundary &&
                placed.boundary.width == width.toDouble &&
                placed.boundary.height == height.toDouble
            if !fits then None
            else
              // The bitmap inside keeps its checkpoint and the pixels the last verification
              // computed, which its new steps start from, so that its own history stays short.
              val operated = replayed(placed, bitmap.steps)
              val same     =
                operated.pixelSize == bitmap.pixelSize &&
                  operated.boundary.width == bitmap.boundary.width &&
                  operated.boundary.height == bitmap.boundary.height
              if !same then None
              else
                val offset = bitmap.boundary.upperLeftCorner - operated.boundary.upperLeftCorner
                val moved  =
                  if offset.x == 0 && offset.y == 0 then operated
                  else operated.moveBy(offset.x, offset.y)
                Some(if moved.hint == bitmap.hint then moved else moved.withResampling(bitmap.hint))
          case _ => None
      case _ => None

  /** `start` with `steps` recorded on it in order, each around its own center, as the operations
    * that recorded them would.
    */
  private def replayed(start: PixelSource, steps: Vector[BitmapStep]): PixelSource =
    steps.foldLeft(start): (bitmap, step) =>
      step match
        case BitmapStep.Scale(width, height) =>
          bitmap.scaleTo(width, height, bitmap.boundary.center)
        case BitmapStep.Rotate(angle) => bitmap.rotateAround(angle, bitmap.boundary.center)
        case BitmapStep.FlipH         => bitmap.flipHorizontally
        case BitmapStep.FlipV         => bitmap.flipVertically
        case BitmapStep.FlipD         => bitmap.flipDiagonally
        case BitmapStep.Crop(left, top, right, bottom) =>
          // A crop takes world coordinates and rounds them down to whole pixels, so the middle of
          // each pixel names it whatever the rounding of the corner.
          val corner = bitmap.boundary.upperLeftCorner
          bitmap.crop(
            corner.x + left + 0.5,
            corner.y + top + 0.5,
            corner.x + right + 0.5,
            corner.y + bottom + 0.5
          )
        case BitmapStep.WrapShift(dx, dy) => bitmap.wrapShift(dx, dy)

  /** Whether pictures nest deeper than `limit` inside bitmaps of pictures in `element`, in masks
    * and every frame of animations too. It looks at most `limit + 1` levels down, at each distinct
    * picture once a level however many bitmaps share it.
    */
  private def deeperThan(element: Element, limit: Int): Boolean =
    // The second parameter holds the distinct pictures that lie inside as many nested picture
    // sources as the first parameter says, along some path of them.
    @tailrec def reaches(level: Int, pictures: Iterable[Element]): Boolean =
      if pictures.isEmpty then false
      else if level > limit then true
      else
        val next = new java.util.IdentityHashMap[Element, Element]
        for picture <- pictures; source <- Traversal.sourcePictures(picture) do
          val _ = next.put(source, source)
        reaches(level + 1, next.keySet.asScala)
    reaches(0, Seq(element))

  /** `picture` with a rectangle of the color the table gives transparent pixels behind it, on the
    * pixels of `frame`, when that color is visible.
    */
  private def withBackground(picture: Element, frame: Bounds, table: Map[Int, Int]): Element =
    table.get(0) match
      case Some(color) if (color >>> 24) != 0 =>
        val background = Rectangle(
          frame.width,
          frame.height,
          frame.center,
          fillWith(new Color(color)),
          None
        )
        picture.addToBack(background)
      case _ => picture

  /** Whether `element` holds an animation anywhere, whose pixels change with time, in the pictures
    * of its picture sources too, which are walked as `smile.infrastructure.Traversal.foldSources`
    * walks them.
    */
  private def animates(element: Element): Boolean =
    Traversal.foldSources(element)(Animates(_)).contains(true)

  /** Whether an element holds an animation outside the pictures of its picture sources, which it
    * gives `meet` as it meets them.
    */
  private final class Animates(meet: Element => Unit) extends Traversal.Fold[Boolean]:
    def empty: Boolean                             = false
    def shape(shape: Shape, m: Affine): Boolean    = false
    def bitmap(bitmap: Bitmap, m: Affine): Boolean =
      bitmap.source.source match
        case BitmapSource.Content(picture) => meet(picture)
        case BitmapSource.Pixels(_)        => ()
      false
    def svg(svg: SvgPicture, m: Affine): Boolean                              = false
    def picture(picture: Picture, m: Affine, parts: Vector[Boolean]): Boolean = parts.contains(true)
    def clip(clip: Clip, m: Affine, content: Boolean): Boolean                = content
    def foldsMasks: Boolean                                                   = true
    def mask(group: MaskGroup, m: Affine, masks: Vector[Boolean], masked: Boolean): Boolean =
      masked || masks.contains(true)
    // Any animation animates, so no frame is folded.
    override def framesOf(animation: AnimatedPicture): Vector[Int] = Vector()
    def animation(animation: AnimatedPicture, m: Affine, frames: Vector[Boolean]): Boolean = true
    def wrap(wrap: Wrap, m: Affine, content: Option[Boolean]): Boolean = content.contains(true)
  end Animates

  /** Calls `f` and keeps what it gave for each color, until it has given two answers for one color
    * or answered for more than `MaximumColors` colors, when it gives up keeping them.
    */
  private final class Observer(f: Int => Int) extends (Int => Int):
    private val answers = mutable.HashMap.empty[Int, Int]

    // The last pixel and the answer for it, which a run of one color repeats without the table.
    private var lastPixel  = 0
    private var lastAnswer = 0
    private var seenAPixel = false

    var gaveUp: Boolean = false

    def apply(pixel: Int): Int =
      val answer  = f(pixel)
      val repeats = seenAPixel && pixel == lastPixel && answer == lastAnswer
      if !gaveUp && !repeats then
        answers.get(pixel) match
          case Some(previous) => if previous != answer then gaveUp = true
          case None           =>
            answers.update(pixel, answer)
            if answers.size > MaximumColors then gaveUp = true
        lastPixel = pixel
        lastAnswer = answer
        seenAPixel = true
      answer

    /** What `f` gave for each color it was called with. */
    def table: Map[Int, Int] = answers.toMap
  end Observer
end Recolor
