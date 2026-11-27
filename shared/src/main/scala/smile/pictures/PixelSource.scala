package smile.pictures

import smile.{RenderSettings, ResamplingMode}
import smile.colors.Color
import smile.infrastructure.{
  BufferAdapter,
  Completeness,
  DeviceGrid,
  DrawPlan,
  NativeBuffer,
  PlatformSpecific,
  RasterCache,
  RasterKey,
  ResourceFactory,
  Traversal
}
import smile.modeling.*

import scala.collection.mutable

/** Builds the pixel sources of bitmaps (see [[smile.pictures.Bitmap]]). */
private[smile] object PixelSource:

  /** How far, in pixels, the moves that turns adding up to none leave may be off none by rounding
    * alone. That is two steps of the grid of 2^-20 pixel that every move is rounded to.
    */
  private[smile] val TurnRounding: Double = 2.0 / 1048576

  def apply(
      width: Int,
      height: Int,
      contentGenerator: (Int, Int) => Color
  ): PixelSource =
    val buffer = BufferAdapter(width, height)
    buffer.setColorsByLocation(contentGenerator)
    fromBuffer(buffer, Bounds(Pos.Origin, width, height))

  /** Creates a bitmap from packed ARGB pixels in row-major order. The pixels are copied into native
    * storage, so subsequent changes to the supplied array do not affect the bitmap. Dimensions must
    * be positive, their product must fit in an `Int`, and exactly `width * height` pixels must be
    * supplied.
    */
  def fromARGB(width: Int, height: Int, pixels: Array[Int]): PixelSource =
    val pixelCount = packedPixelCount(width, height)
    require(
      pixels.length == pixelCount,
      "The pixels array must have exactly width * height items"
    )
    val buffer = BufferAdapter(width, height)
    buffer.setARGBData(pixels)
    fromBuffer(buffer, Bounds(Pos.Origin, width, height))

  /** Creates a bitmap by generating packed ARGB pixels without allocating a `Color` per pixel. The
    * generator is called once per pixel, from left to right in each row, top row first. Dimensions
    * must be positive and their product must fit in an `Int`.
    */
  def generateARGB(width: Int, height: Int, generator: (Int, Int) => Int): PixelSource =
    val _      = packedPixelCount(width, height)
    val buffer = BufferAdapter(width, height)
    buffer.setARGBByLocation(generator)
    fromBuffer(buffer, Bounds(Pos.Origin, width, height))

  private def packedPixelCount(width: Int, height: Int): Int =
    require(width > 0 && height > 0, "PixelSource width and height must be positive")
    val count = width.toLong * height.toLong
    require(count <= Int.MaxValue, "PixelSource width * height must fit in an Int")
    count.toInt

  /** Creates a `PixelSource` from a sequence of colors, assuming row-wise population.
    */
  def apply(width: Int, height: Int, colors: Seq[Color]): PixelSource =
    require(
      colors.length == (width * height),
      "The colors sequence must have exactly width * height items"
    )
    val buffer = BufferAdapter(width, height)
    buffer.setColorsFromSeq(colors)
    fromBuffer(buffer, Bounds(Pos.Origin, width, height))

  def apply(sourceResourcePath: String): PixelSource =
    val image = ResourceFactory().bufferAdapterFromPath(sourceResourcePath)
    fromBuffer(image, Bounds(Pos.Origin, image.width, image.height))

  /** Loads a bitmap asynchronously from a path. Works on both platforms. On Scala.js it is the only
    * way to load an image for the first time, because browser decoding is asynchronous, and it also
    * enables the synchronous `apply` for the same path afterwards.
    */
  def load(sourceResourcePath: String): scala.concurrent.Future[PixelSource] =
    ResourceFactory()
      .loadBufferAdapter(sourceResourcePath)
      .map(image => fromBuffer(image, Bounds(Pos.Origin, image.width, image.height)))(using
        scala.concurrent.ExecutionContext.parasitic
      )

  /** The whole pixels at one buffer pixel per logical pixel that cover the bounds of `element`,
    * which a pixel source of it lays its pixels on, or `None` when it has no bounds or they cover
    * no whole pixel.
    */
  private[smile] def frameOf(element: Element): Option[Bounds] =
    element.bounds.flatMap: bounds =>
      val box = DeviceGrid(1).covering(bounds)
      Option.when(box.width >= 1 && box.height >= 1):
        Bounds(
          box.left.toDouble,
          box.top.toDouble,
          (box.left + box.width).toDouble,
          (box.top + box.height).toDouble
        )

  /** A source of `element`'s pixels that renders it only when its pixels are asked for, with the
    * settings of the output that asks. Its pixels are the whole pixels that cover the element's
    * bounds, as rendering the element gives them.
    */
  def of(element: Element): PixelSource =
    frameOf(element) match
      case None        => Empty
      case Some(frame) => ofFrame(BitmapSource.Content(element), frame)

  /** A bitmap of `content` whose pixels at one buffer pixel per logical pixel for `settings` are
    * `pixels`, as if a color operation had given them. They are its checkpoint. An output with
    * other settings, or one in `Direct` mode, renders `content`. Operations on the bitmap replay on
    * `pixels` for `settings`, as they would on a bitmap of those pixels.
    *
    * @param content
    *   The picture the bitmap is of, whose frame has the size of `pixels`.
    * @param pixels
    *   The pixels, which nothing may change afterwards.
    */
  private[smile] def checkpointed(
      content: Element,
      pixels: NativeBuffer,
      settings: RenderSettings
  ): PixelSource =
    frameOf(content) match
      case Some(frame)
          if (frame.width.toInt, frame.height.toInt) == (pixels.width, pixels.height) =>
        ofFrame(BitmapSource.Content(content), frame)
          .copied(newCheckpoint = Some(Checkpoint(settings, pixels, 0)))
      case frame =>
        throw new IllegalArgumentException(
          s"The pixels (${pixels.width} by ${pixels.height}) must have the size of the " +
            s"picture's frame ($frame)"
        )

  /** A source of a picture `source` that renders into `frame`, a box of whole pixels. The picture's
    * pixel at column x is the world's from x to x + 1 past the frame's corner, so the pixels lie on
    * the pixel grid and are drawn as they are.
    */
  private def ofFrame(source: BitmapSource, frame: Bounds): PixelSource =
    val (width, height) = (frame.width.toInt, frame.height.toInt)
    started(source, (width, height), frame, RasterPlan.of(frame, width, height))

  /** A bitmap of the given pixels, which nothing may change afterwards. */
  private[smile] def fromBuffer(buffer: NativeBuffer, bounds: Bounds): PixelSource =
    started(
      BitmapSource.Pixels(buffer),
      (buffer.width, buffer.height),
      bounds,
      RasterPlan.of(bounds, buffer.width, buffer.height)
    )

  /** A bitmap of `source`, whose pixels are `size`, laid on `bounds` as `plan` says with no
    * operations recorded.
    */
  private def started(
      source: BitmapSource,
      size: (Int, Int),
      bounds: Bounds,
      plan: RasterPlan
  ): PixelSource =
    new PixelSource(
      source,
      Vector.empty,
      bounds,
      ResamplingHint.Auto,
      plan,
      None,
      size,
      resampled = false,
      complete = true,
      checkpoint = None,
      new PixelMemo,
      new SourceIdentity
    )

  /** `angle`, in degrees, as the nearest multiple of 90 when it is that close within the tolerance
    * of [[smile.modeling.Affine]], measured in radians as the turn's sine would be, and otherwise
    * as it is. A multiple of 360 becomes 0, and any other keeps its sign.
    */
  private[smile] def snappedAngle(angle: Double): Double =
    val right = Math.rint(angle / 90) * 90
    if math.toRadians((angle - right).abs) <= Affine.Tolerance then right % Angles.FullTurn + 0.0
    else angle

  /** The most tiles a bitmap's plan has before the bitmap is flattened into its pixels. */
  private val MaximumTiles = 8

  /** The most operations a bitmap keeps after its checkpoint before it takes a new one. */
  private val MaximumSteps = 32

  /** The densest a picture source is rendered at, in buffer pixels per logical pixel. */
  private val MaximumDensity = 4.0

  /** The step the density of a picture source is rounded up to, so that a picture moved or scaled a
    * little is not rendered again.
    */
  private val DensityStep = 0.25

  /** The density a picture source is rendered at where it is drawn at `wanted` device pixels per
    * pixel of the source. It is rounded up to a multiple of a quarter and is at most 4.
    */
  private[smile] def contentDensity(wanted: Double): Double =
    val rounded = ((wanted - 1e-9) / DensityStep).ceil * DensityStep
    rounded.max(DensityStep).min(MaximumDensity)

  /** The grid, in source pixels, that the parts of a picture source drawn through the shared draw
    * plan are rounded out to. A multiple of four, so its lines lie on whole buffer pixels at every
    * density.
    */
  private[smile] val ContentTile = 256

  /** A picture source as the bitmaps made from it share it, which names its renderings in
    * `smile.infrastructure.RasterCache.shared`. Bitmaps made from one bitmap share its identity,
    * and a bitmap of another picture, or of the same picture made separately, has its own.
    *
    * It also fixes the instant every rendering of the source shows its animations at, which is the
    * instant of its first rendering. So a rendering that the cache let go and that is asked for
    * again, or one at another density, shows the same frame, and what was kept never changes the
    * pixels.
    */
  private[smile] final class SourceIdentity:
    private var firstInstant: Option[Long] = None

    /** How many pixels the renderings of this source have had in all, which tests read. Updates
      * from threads rendering at once may be lost.
      */
    @volatile private[smile] var renderedPixelCount: Long = 0

    /** The instant this source is rendered at. It is `asked` when this is its first rendering, and
      * the instant of its first rendering otherwise.
      */
    def instant(asked: Long): Long = synchronized:
      if firstInstant.isEmpty then firstInstant = Some(asked)
      firstInstant.getOrElse(asked)

  /** Names one rendering of a picture source by the part of it rendered, in its pixels, a density
    * and settings.
    */
  private[smile] type SourceKey = (SourceRegion, Double, RenderSettings)

  /** The part of a picture source rendered, from column `left` and row `top` to before column
    * `right` and row `bottom` of its pixels.
    */
  private[smile] final case class SourceRegion(left: Int, top: Int, right: Int, bottom: Int)
      derives CanEqual

  /** The plan of the rendering `key` of a picture source of `picture` at the instant `timeMillis`.
    * It draws the region of the picture, moved so that its frame's corner, a whole pixel, is at the
    * origin, where the region's corner lies on a whole device pixel at every density.
    */
  private def planOf(picture: Element, key: SourceKey, timeMillis: Long): DrawPlan.Plan =
    val (region, density, settings) = key
    val corner                      = frameOf(picture).fold(Pos.Origin)(_.upperLeftCorner)
    val part                        = new Clip(
      Bounds(region.left, region.top, region.right, region.bottom),
      picture.moveBy(-corner.x, -corner.y),
      Affine.Identity,
      Pos.Origin
    )
    DrawPlan.planned(part, DeviceGrid(density), settings, timeMillis)

  /** Renders each rendering of a picture source that drawing `plan`, made at the instant
    * `timeMillis`, with `settings` asks for and that is not kept yet into
    * `smile.infrastructure.RasterCache.shared`, at the instant of its source (see
    * `smile.pictures.PixelSource.SourceIdentity`), after the renderings that it asks for in turn,
    * however deep picture sources nest in each other. So each finds the renderings it draws kept
    * while they fit in the cache, and no rendering runs inside another. They are found with a stack
    * of their own, from the plan of the rendering that asks for them, which is made once and then
    * drawn. A rendering asked for twice is made once, as the second ask finds it kept. One that the
    * cache cannot keep, as one larger than its budget, is rendered again inside the drawing that
    * asks for it, with the same pixels and more work.
    */
  private def renderSources(
      plan: DrawPlan.Plan,
      settings: RenderSettings,
      timeMillis: Long
  ): Unit =
    // Each rendering still to make, with the instant of the plan that asks for it, or, once the
    // renderings it asks for lie above it, with its own plan and the instant that plan is made at.
    val pending = mutable.Stack[(PixelSource, SourceKey, Long, Option[DrawPlan.Plan])]()
    def askedBy(plan: DrawPlan.Plan, settings: RenderSettings, instant: Long): Unit =
      for (source, key) <- DrawPlan.sourceRenderings(plan, settings) do
        pending.push((source, key, instant, None))
    askedBy(plan, settings, timeMillis)
    while pending.nonEmpty do
      pending.pop() match
        case (source, key, instant, Some(plan)) => val _ = source.rendered(key, plan, instant)
        case (source, key, asked, None)         =>
          source.source match
            case BitmapSource.Content(picture) if source.keptRendering(key).isEmpty =>
              val instant = source.renderings.instant(asked)
              val plan    = planOf(picture, key, instant)
              pending.push((source, key, instant, Some(plan)))
              askedBy(plan, key._3, instant)
            case _ => ()

  /** The pixels for `settings` that a bitmap's steps after its first `stepCount` replay on. They
    * are the pixels that a read of the bitmap it was made from computed, the pixels a color
    * operation gave a recolored picture, or the pixels the bitmap took when its steps reached
    * `MaximumSteps`. Nothing may change them.
    */
  private[smile] final case class Checkpoint(
      settings: RenderSettings,
      pixels: NativeBuffer,
      stepCount: Int
  )

  /** Whether `step` resamples the pixels, as a scaling or a turn does. */
  private def resamples(step: BitmapStep): Boolean = step match
    case BitmapStep.Scale(_, _) | BitmapStep.Rotate(_) => true
    case _                                             => false

  /** A fresh empty bitmap. A new instance is returned on every access because the platform image of
    * a buffer can be drawn on through its `get`, so a shared singleton could be corrupted.
    */
  def Empty: PixelSource = fromBuffer(BufferAdapter.Empty, Nowhere)

  /** The bounds of an empty source, which has no area and lies at the origin. */
  private val Nowhere = Bounds(0, 0, 0, 0)

  /** The pixels a bitmap has computed, for the last two settings they were asked for. Bitmaps that
    * differ only in where they are share one, because their pixels are the same.
    *
    * Two threads may use it at once, as a painting thread and the program that made the bitmap do.
    * Each entry list is immutable and published through a volatile field, and a thread that
    * computed pixels another thread has stored meanwhile takes the stored ones, so the same
    * settings keep giving the same buffer.
    */
  private[smile] final class PixelMemo[Key](using CanEqual[Key, Key]):
    @volatile private var entries: List[(Key, NativeBuffer)] = Nil

    /** The pixels stored for `key`, without computing them. */
    def peek(key: Key): Option[NativeBuffer] =
      entries.collectFirst { case (stored, pixels) if stored == key => pixels }

    /** The pixels stored last for a key that `wanted` accepts, with their key. */
    def newest(wanted: Key => Boolean): Option[(Key, NativeBuffer)] =
      entries.find((key, _) => wanted(key))

    /** The pixels stored for `key`, or those `compute` gives, stored unless something was left out
      * of them (see `smile.infrastructure.Completeness`), so that they are computed again.
      */
    def getOrCompute(key: Key, compute: Key => NativeBuffer): NativeBuffer =
      entries.collectFirst { case (stored, pixels) if stored == key => pixels } match
        case Some(pixels) => pixels
        case None         =>
          val (computed, whole) = Completeness.tracked(compute(key))
          if !whole then computed
          else
            synchronized:
              entries.collectFirst { case (stored, pixels) if stored == key => pixels } match
                case Some(stored) => stored
                case None         =>
                  entries = (key, computed) :: entries.take(1)
                  computed
  end PixelMemo
end PixelSource

/** The pixels of a [[smile.pictures.Bitmap]]. They are a source of pixels and the bitmap's own
  * operations applied to it since, laid on its own frame, which the bitmap's affine places in the
  * world. A "bitmap" below is this source.
  *
  * The source is either pixels that exist already (a loaded image, generated pixels, or the result
  * of a color operation) or a picture to be rendered. A picture recolored by
  * [[smile.pictures.Recolor]] is both. Outputs with the recolor's settings take the pixels the
  * color operation gave, and others render the recolored picture. A bitmap's operations (scaling,
  * turning, flipping, cropping and wraparound shifts) are recorded and applied when its pixels are
  * needed. Its boundary is worked out at once, as is any check that an operation's arguments are
  * valid. Crops in a row fold into one, and so do shifts.
  *
  * An output that draws the bitmap does so with its own settings, through the shared draw plan, in
  * one of two ways that [[smile.ResamplingMode]] chooses between:
  *
  *   - `Direct`, the default, draws the source's pixels once, with the scaling and turning of all
  *     the operations composed into one map, at the output's own scale. A picture source is
  *     rendered at the density the output needs. A bitmap that is only moved, flipped, cropped,
  *     shifted or turned by right angles is drawn on whole pixels, so that it stays crisp.
  *   - `Stepwise` draws the bitmap's pixels at one buffer pixel per logical pixel, with the
  *     operations replayed in order by the buffer algorithms.
  *
  * Its pixels, from a call such as `smile.pictures.PixelSource.buffer` or
  * `smile.pictures.PixelSource.getColor`, are what an output with the same settings draws at one
  * buffer pixel per logical pixel. A bitmap keeps the pixels of the last two settings it was asked
  * with. In `Direct` mode, pixels read after a chain of scalings and turnings come from one
  * resampling of the source, as an output's drawing does. Ask for `Stepwise` pixels to reproduce a
  * chain of eager resamplings.
  *
  * A bitmap may have a checkpoint (see `smile.pictures.PixelSource.Checkpoint`). It holds pixels
  * for one settings after some of its steps, which a read with those settings replays only the
  * later steps on. A bitmap made by an operation takes the newest `Stepwise` pixels that the bitmap
  * it is made from has computed as its checkpoint, so a bitmap changed and read on every tick
  * replays one operation per tick, and it keeps every step, so a read with other settings replays
  * from the source as if nothing had been read. Only a bitmap that would keep more than 32 steps
  * drops the steps before its checkpoint, and pixels for any settings may then be that checkpoint.
  * After that, reads with other settings replay on the checkpoint. Its source and the way its
  * operations lay the source on the world stay whole, so a `Direct` output draws it as sharply as
  * ever. An operation computes pixels only when its result would keep more than 32 steps after a
  * checkpoint and has no newer pixels, when it takes a new checkpoint for the same settings, or
  * draw more than 8 tiles, when it becomes a bitmap of its pixels. A bitmap without a checkpoint
  * keeps its steps, which its first read replays with the reader's settings. The color and merge
  * operations compute by definition.
  */
private[smile] final class PixelSource private[smile] (
    private[smile] val source: BitmapSource,
    private[smile] val steps: Vector[BitmapStep],
    bounds: Bounds,
    private[smile] val hint: ResamplingHint,
    private val recordedPlan: RasterPlan,
    private val unshiftedPlan: Option[RasterPlan],
    private[smile] val pixelSize: (Int, Int),
    resampled: Boolean,
    private[smile] val complete: Boolean,
    private[smile] val checkpoint: Option[PixelSource.Checkpoint],
    memo: PixelSource.PixelMemo[RenderSettings],
    private val renderings: PixelSource.SourceIdentity
):

  /** Where this source lays its pixels in the world, before any affine of a bitmap places it. */
  lazy val boundary: Bounds = bounds

  /** Whether this is the empty source, which has no area and no pixels to lay. */
  private[smile] def isEmpty: Boolean =
    boundary.width == 0 || boundary.height == 0

  /** How many operations the last replay of this bitmap's pixels applied, which tests read. */
  @volatile private[smile] var stepsLastReplayed: Int = 0

  /** How this bitmap's operations lay its source on the world, exactly. A turned bitmap's source
    * reaches its turned corners, past the boundary of its floored buffer.
    */
  private[smile] def exactPlan: RasterPlan = recordedPlan

  /** The polygons this source's plan draws, in the frame it lays its pixels in. Each tile that
    * draws any gives one, the source's whole pixel rectangle under the tile's map, cut by its clip.
    * The source is immutable, so they are worked out once, and a bitmap moved on every frame
    * measures its bounds from them.
    */
  private[smile] lazy val drawnPolygons: Vector[Vector[Pos]] =
    val (width, height) = (wholeSource.right.toDouble, wholeSource.bottom.toDouble)
    recordedPlan.tiles.flatMap: tile =>
      val corners = Vector(Pos(0, 0), Pos(width, 0), Pos(width, height), Pos(0, height))
        .map(tile.toWorld(_))
      val drawn = tile.clip.fold(corners)(RasterPlan.intersectConvex(corners, _))
      if drawn.isEmpty then None else Some(drawn)

  /** The box around `smile.pictures.PixelSource.drawnPolygons`, or `None` when there are none. */
  private[smile] lazy val drawnBox: Option[Bounds] =
    Element.union(drawnPolygons.iterator.map(polygon => Some(Rasters.boxAround(polygon))))

  /** How an output draws this bitmap's source when it draws it directly. The source lies as its
    * operations lay it on the world and is drawn only inside its boundary, as its pixels are.
    */
  private[smile] lazy val plan: RasterPlan = recordedPlan.clippedTo(boundary)

  /** This bitmap's pixels at one buffer pixel per logical pixel, computed with `settings` and kept
    * for the next time they are asked for with the same settings. They are what an output with
    * these settings draws at that scale.
    *
    * The buffer must not be changed. A loop over many pixels is far faster through its `argbData`
    * than through `smile.pictures.PixelSource.getColor`.
    *
    * @param timeMillis
    *   The instant a picture source is rendered at where its pixels are not kept yet, so that each
    *   animation in it shows its frame at that instant.
    */
  def buffer(settings: RenderSettings, timeMillis: Long): NativeBuffer =
    source match
      case BitmapSource.Pixels(buffer) if steps.isEmpty && checkpoint.isEmpty => buffer
      // Kept in the memo too, so that a bitmap made from this one starts from them.
      case BitmapSource.Content(_) if steps.isEmpty && complete && start(settings).isEmpty =>
        memo.getOrCompute(settings, sourcePixels(_, timeMillis))
      case _ => memo.getOrCompute(settings, compute(_, timeMillis))

  /** How this bitmap would like to be resampled where an output in `Direct` mode draws it at
    * another size than its pixels'. A `Stepwise` output ignores it. See
    * [[smile.pictures.ResamplingHint]].
    */
  def resampling: ResamplingHint = hint

  /** A copy of this bitmap with another resampling hint. `Pixelated` draws nearest neighbor and
    * `Smooth` bilinear in `Direct` mode, whatever the settings say, and `Auto` follows the
    * settings' `bufferScalingMethod`. The pixels of a `Stepwise` output are the same for every
    * hint. Operations on the copy keep the hint.
    */
  def withResampling(newHint: ResamplingHint): PixelSource = copied(newHint = newHint)

  /** Whether an output with `settings` draws this bitmap's source through `plan`. Otherwise it
    * draws its `smile.pictures.PixelSource.buffer`. It does in `Direct` mode when the source is a
    * picture, which is then rendered at the output's scale, when the bitmap is scaled or turned so
    * that its pixels no longer lie on a pixel grid, and when it is shifted into several tiles.
    */
  private[smile] def drawsDirectly(settings: RenderSettings): Boolean =
    settings.resamplingMode == ResamplingMode.Direct && drawnFromItsSource

  private lazy val drawnFromItsSource: Boolean =
    val content = source match
      case BitmapSource.Content(_) => true
      case BitmapSource.Pixels(_)  => false
    plan.isFinite &&
    (content || plan.tiles.length > 1 || (resampled && !recordedPlan.keepsPixelGrid))

  /** The part of a picture source that a drawing needs where it draws the source's pixels from
    * `left` to `right` and from `top` to `bottom`, rendered at `density` buffer pixels per logical
    * pixel with `settings`, at the instant `timeMillis` unless they are kept. Gives the region of
    * the source those pixels lie in, and its pixels. `None` for a source of pixels.
    *
    * The region is widened by a source pixel and the two buffer pixels the widest interpolation
    * reads past an edge, and then out to the grid of `PixelSource.ContentTile` source pixels, so
    * that a drawing that scrolls a little needs the same region, which the cache of renderings
    * keeps.
    */
  private[smile] def contentPixelsCovering(
      left: Double,
      top: Double,
      right: Double,
      bottom: Double,
      density: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): Option[(PixelSource.SourceRegion, NativeBuffer)] =
    source match
      case BitmapSource.Content(picture) =>
        val key = renderingCovering(left, top, right, bottom, density, settings)
        Some((key._1, contentPixels(picture, key, timeMillis)))
      case BitmapSource.Pixels(_) => None

  /** The rendering of this bitmap's picture source that
    * `smile.pictures.PixelSource.contentPixelsCovering` gives the pixels of, as its region, density
    * and settings.
    */
  private[smile] def renderingCovering(
      left: Double,
      top: Double,
      right: Double,
      bottom: Double,
      density: Double,
      settings: RenderSettings
  ): PixelSource.SourceKey =
    val whole                                 = wholeSource
    val margin                                = 1 + 2 / density
    val tile                                  = PixelSource.ContentTile.toDouble
    def lower(value: Double, limit: Int): Int =
      (((value - margin) / tile).floor * tile).max(0.0).min(limit.toDouble).toInt
    def higher(value: Double, limit: Int): Int =
      (((value + margin) / tile).ceil * tile).max(0.0).min(limit.toDouble).toInt
    val cut = PixelSource.SourceRegion(
      lower(left, whole.right),
      lower(top, whole.bottom),
      higher(right, whole.right),
      higher(bottom, whole.bottom)
    )
    val region = if cut.right <= cut.left || cut.bottom <= cut.top then whole else cut
    (region, density, settings)

  /** The rendering of this bitmap's picture source that its `smile.pictures.PixelSource.buffer` for
    * `settings` is computed from, unless it is kept or starts from a checkpoint. It is the whole
    * source at one buffer pixel per logical pixel. `None` for a source of pixels. A painter asks
    * for the pixels of a bitmap it does not draw directly, so they are computed when they are not
    * kept.
    */
  private[smile] def renderingForPixels(settings: RenderSettings): Option[PixelSource.SourceKey] =
    source match
      case BitmapSource.Content(_)
          if complete && memo.peek(settings).isEmpty && start(settings).isEmpty =>
        Some((wholeSource, 1.0, settings))
      case _ => None

  /** The checkpoint, when it is for `settings` and the pixels for `settings` are a replay of the
    * steps.
    */
  private def start(settings: RenderSettings): Option[PixelSource.Checkpoint] =
    checkpoint.filter(c => c.settings == settings && !drawsDirectly(settings))

  /** How many pixels the renderings of this bitmap's picture source have had in all, for every
    * bitmap made from it, which tests read.
    */
  private[smile] def renderedSourcePixelCount: Long = renderings.renderedPixelCount

  /** The whole of a picture source, in its pixels. */
  private[smile] lazy val wholeSource: PixelSource.SourceRegion = source match
    case BitmapSource.Content(picture) =>
      PixelSource
        .frameOf(picture)
        .fold(PixelSource.SourceRegion(0, 0, 0, 0)): frame =>
          PixelSource.SourceRegion(0, 0, frame.width.toInt, frame.height.toInt)
    case BitmapSource.Pixels(buffer) => PixelSource.SourceRegion(0, 0, buffer.width, buffer.height)

  /** The source's pixels, at one buffer pixel per logical pixel of the source, computed with
    * `settings`. A picture is rendered once per settings for every bitmap made from it while the
    * rendering is kept. Every rendering of it, at any density and with any settings, and also one
    * rendered again after the cache let it go, shows the picture at the instant of its first
    * rendering, which is `timeMillis` when this is that rendering.
    */
  private[smile] def sourcePixels(settings: RenderSettings, timeMillis: Long): NativeBuffer =
    source match
      case BitmapSource.Pixels(buffer)   => buffer
      case BitmapSource.Content(picture) =>
        contentPixels(picture, (wholeSource, 1.0, settings), timeMillis)

  /** The rendering `key` of `picture`, this bitmap's picture source. It is a region of it rendered
    * at a density with settings at the instant of the source's first rendering, which is
    * `timeMillis` when this is that rendering, and kept for every bitmap made from it in
    * `smile.infrastructure.RasterCache.shared`. Its pixels are those of the whole source's
    * rendering it covers. The renderings of picture sources that it draws are rendered first (see
    * `smile.pictures.PixelSource.renderSources`), so that a picture that holds a bitmap of a
    * picture that holds another, however many times over, renders without one rendering inside
    * another while the cache keeps them.
    */
  private def contentPixels(
      picture: Element,
      key: PixelSource.SourceKey,
      timeMillis: Long
  ): NativeBuffer =
    keptRendering(key).getOrElse:
      val instant = renderings.instant(timeMillis)
      val plan    = PixelSource.planOf(picture, key, instant)
      PixelSource.renderSources(plan, key._3, instant)
      rendered(key, plan, instant)

  /** The rendering `key` of this bitmap's picture source, when the cache keeps it. */
  private def keptRendering(key: PixelSource.SourceKey): Option[NativeBuffer] =
    RasterCache.shared.peek(cacheKey(key))

  /** The rendering `key` of this bitmap's picture source, drawn from `plan`, its plan made at the
    * instant `timeMillis`, and kept as `smile.pictures.PixelSource.contentPixels` keeps it.
    */
  private def rendered(
      key: PixelSource.SourceKey,
      plan: DrawPlan.Plan,
      timeMillis: Long
  ): NativeBuffer =
    val (_, density, settings) = key
    RasterCache.shared.getOrCompute(cacheKey(key)):
      val pixels = PlatformSpecific
        .renderPlan(plan, density, settings, timeMillis)
        .buffer(settings, timeMillis)
      renderings.renderedPixelCount += pixels.width.toLong * pixels.height
      pixels

  /** The key the rendering `key` of this bitmap's picture source is kept under. */
  private def cacheKey(key: PixelSource.SourceKey): RasterKey =
    val (region, density, settings) = key
    RasterKey.Rendering(renderings, region, density, settings)

  /** Where this bitmap's pixels lie in the world. That is its boundary, except for a picture's
    * pixels that no scaling or turning has resampled, which lie on whole logical pixels from the
    * boundary's upper left corner, as a picture is rendered. Flips mirror across its center, and
    * shifts wrap around inside it.
    */
  private[smile] lazy val pixelArea: Bounds =
    source match
      case BitmapSource.Content(_) if !resampled =>
        val corner = boundary.upperLeftCorner
        Bounds(corner, corner + (pixelSize._1.toDouble, pixelSize._2.toDouble))
      case _ => boundary

  /** The pixels an output with `settings` draws at one buffer pixel per logical pixel, with a
    * picture source rendered at the instant `timeMillis` unless it is kept. A bitmap drawn directly
    * is drawn as a picture of its own. Otherwise its steps are replayed. Those after its checkpoint
    * are replayed when that is for `settings`, and every one on its source while it keeps every
    * step since the source. Otherwise those after its checkpoint for other settings are replayed,
    * on the only pixels its earlier steps left.
    */
  private def compute(settings: RenderSettings, timeMillis: Long): NativeBuffer =
    if drawsDirectly(settings) then drawn(settings, timeMillis)
    else
      start(settings).orElse(checkpoint.filter(_ => !complete)) match
        case Some(from) => replay(from.pixels, from.stepCount, settings)
        case None       => replay(sourcePixels(settings, timeMillis), 0, settings)

  /** This bitmap drawn with `settings` as a picture of its own, on as many pixels as its replay
    * has, laid from its boundary's upper left corner.
    */
  private def drawn(settings: RenderSettings, timeMillis: Long): NativeBuffer =
    // Drawn with its boundary's corner at the origin, so that its pixels are whole ones there.
    val corner          = boundary.upperLeftCorner
    val (width, height) = pixelSize
    val picture         = new Clip(
      Bounds(0, 0, width.toDouble, height.toDouble),
      new Bitmap(this, Affine.translation(-corner.x, -corner.y), boundary.center),
      Affine.Identity,
      Pos.Origin
    )
    PlatformSpecific.renderPlan(picture, 1.0, settings, timeMillis).buffer(settings, timeMillis)

  /** `start` with the steps after the first `done` replayed on it in order. */
  private def replay(start: NativeBuffer, done: Int, settings: RenderSettings): NativeBuffer =
    val remaining = steps.drop(done)
    stepsLastReplayed = remaining.length
    remaining.foldLeft(start)((pixels, step) => step.replayOn(pixels, settings))

  /** This bitmap with the given parts replaced, and a new memo unless one is given. */
  private def copied(
      newSource: BitmapSource = source,
      newSteps: Vector[BitmapStep] = steps,
      newBounds: Bounds = boundary,
      newHint: ResamplingHint = hint,
      newPlan: RasterPlan = recordedPlan,
      newUnshiftedPlan: Option[RasterPlan] = unshiftedPlan,
      newPixelSize: (Int, Int) = pixelSize,
      newResampled: Boolean = resampled,
      newComplete: Boolean = complete,
      newCheckpoint: Option[PixelSource.Checkpoint] = checkpoint,
      newMemo: PixelSource.PixelMemo[RenderSettings] = new PixelSource.PixelMemo,
      newRenderings: PixelSource.SourceIdentity = renderings
  ): PixelSource =
    new PixelSource(
      newSource,
      newSteps,
      newBounds,
      newHint,
      newPlan,
      newUnshiftedPlan,
      newPixelSize,
      newResampled,
      newComplete,
      newCheckpoint,
      newMemo,
      newRenderings
    )

  /** A bitmap with the same pixels at other bounds of the same size. It shares the pixels computed
    * already, and so the start of the bitmaps made from it.
    */
  private def at(newBounds: Bounds): PixelSource =
    val offset = newBounds.upperLeftCorner - boundary.upperLeftCorner
    copied(
      newBounds = newBounds,
      newPlan = recordedPlan.movedBy(offset.x, offset.y),
      newUnshiftedPlan = unshiftedPlan.map(_.movedBy(offset.x, offset.y)),
      newMemo = memo
    )

  /** This bitmap with one more operation recorded, which lays the bitmap on `newBounds` and its
    * source as `newPlan` says. The newest `Stepwise` pixels this bitmap has computed, read once
    * now, become its checkpoint after this bitmap's steps, which it keeps with the new one. Where
    * it would keep more than `MaximumSteps` steps, or this bitmap no longer keeps all of its own,
    * the checkpoint replaces those steps and only the new step is kept. In the first of those
    * cases, pixels computed for any settings serve as the checkpoint when there are no `Stepwise`
    * ones, so that a bitmap changed and read on every tick in `Direct` mode stays bounded too. With
    * no pixels to start from, it keeps this bitmap's checkpoint and steps.
    */
  private def recording(step: BitmapStep, newBounds: Bounds, newPlan: RasterPlan): PixelSource =
    val full  = steps.length >= PixelSource.MaximumSteps
    val start = memo
      .newest(_.resamplingMode == ResamplingMode.Stepwise)
      .orElse(if full then memo.newest(_ => true) else None)
    val (newCheckpoint, earlier, newComplete) = start match
      case Some((settings, pixels)) if full || !complete =>
        (Some(PixelSource.Checkpoint(settings, pixels, 0)), Vector.empty, complete && steps.isEmpty)
      case Some((settings, pixels)) =>
        (Some(PixelSource.Checkpoint(settings, pixels, steps.length)), steps, complete)
      case None => (checkpoint, steps, complete)
    copied(
      newSteps = earlier :+ step,
      newBounds = newBounds,
      newPlan = newPlan,
      newUnshiftedPlan = None,
      newPixelSize = step.sizeAfter(pixelSize._1, pixelSize._2),
      newResampled = resampled || PixelSource.resamples(step),
      newComplete = newComplete,
      newCheckpoint = newCheckpoint
    ).cappedIfDue(flatteningSettings)

  /** A bitmap of the same source whose steps are `newSteps`, which fold into or drop this one's
    * last step, or add one. It keeps the checkpoint while the steps it stands for stay.
    */
  private def withSteps(
      newSteps: Vector[BitmapStep],
      newBounds: Bounds,
      newPlan: RasterPlan,
      newUnshiftedPlan: Option[RasterPlan],
      newPixelSize: (Int, Int)
  ): PixelSource =
    copied(
      newSteps = newSteps,
      newBounds = newBounds,
      newPlan = newPlan,
      newUnshiftedPlan = newUnshiftedPlan,
      newPixelSize = newPixelSize,
      newCheckpoint = checkpoint.filter(c => newSteps.startsWith(steps.take(c.stepCount)))
    ).cappedIfDue(flatteningSettings)

  /** The settings a bitmap made from this one is flattened with when it draws too many tiles. They
    * are those of the newest pixels this bitmap has computed, or of its checkpoint. Without either,
    * `smile.RenderSettings.Default`, which is exact for a source of pixels that no step resampled.
    */
  private def flatteningSettings: RenderSettings =
    memo
      .newest(_ => true)
      .map(_._1)
      .orElse(checkpoint.map(_.settings))
      .getOrElse(RenderSettings.Default)

  /** This bitmap, or, when it draws more than `MaximumTiles` tiles, a bitmap of its pixels for
    * `flattening`, or, when it keeps more than `MaximumSteps` steps and has a checkpoint, this
    * bitmap with its pixels for the checkpoint's settings as a new checkpoint and no steps. A
    * bitmap with no checkpoint computes nothing here. It keeps its steps, which its first read
    * replays with the reader's settings. That keeps the memory and the work of a bitmap bounded
    * however many operations it goes through, except while an SVG document it draws is still
    * decoding in the browser. Pixels that leave it out are not kept, so the bitmap is flattened
    * only once they are whole (see `smile.infrastructure.Completeness`). A crop to a small part of
    * a large source is never flattened, so that it stays sharp at every scale. It keeps the source
    * alive, as the picture it was cropped from does.
    */
  private def cappedIfDue(flattening: => RenderSettings): PixelSource =
    if plan.tiles.length > PixelSource.MaximumTiles then
      // Pixels that left something out are not kept. The bitmap stays as it is until they are
      // whole.
      val (pixels, whole) = Completeness.tracked(buffer(flattening, System.currentTimeMillis()))
      if whole then withPixels(pixels, boundary) else this
    else
      checkpoint match
        case Some(last) if steps.length > PixelSource.MaximumSteps =>
          // These pixels are whole. A checkpoint is only ever taken from whole pixels, and these
          // replay on it or draw what drew whole then.
          val pixels = buffer(last.settings, System.currentTimeMillis())
          copied(
            newSteps = Vector.empty,
            newComplete = false,
            newCheckpoint = Some(PixelSource.Checkpoint(last.settings, pixels, 0)),
            newMemo = memo
          )
        case _ => this

  /** A bitmap of the given pixels, made from this one. */
  private def withPixels(buffer: NativeBuffer, newBounds: Bounds): PixelSource =
    PixelSource.fromBuffer(buffer, newBounds).copied(newHint = hint)

  /** This bitmap with every color of its source replaced through `table`. That is each pixel of a
    * pixel source, or every paint of a picture source, and each pixel of its checkpoint. Its
    * operations, boundary and hint stay as they are.
    */
  private[smile] def mapPaints(table: Int => Int): PixelSource =
    val mapped = source match
      case BitmapSource.Pixels(buffer)   => BitmapSource.Pixels(buffer.transformARGB(table))
      case BitmapSource.Content(picture) =>
        BitmapSource.Content(Traversal.mapPaints(picture, table))
    copied(
      newSource = mapped,
      newCheckpoint = checkpoint.map(c => c.copy(pixels = c.pixels.transformARGB(table))),
      newRenderings = new PixelSource.SourceIdentity
    )

  /** This bitmap without its checkpoint and with no pixels computed, so that every output, and
    * every bitmap made from it, computes its pixels from its source. Only a bitmap that keeps every
    * step since its source can do without its checkpoint.
    */
  private[smile] def withoutCheckpoint: PixelSource =
    require(
      complete,
      "Only a bitmap that keeps every step since its source can drop its checkpoint"
    )
    copied(newCheckpoint = None)

  /** Moves this bitmap's pixels right by `dx` and down by `dy`, with wraparound. The pixels that
    * leave it on one side come back on the other, so the bitmap keeps its size and place. Negative
    * offsets move the pixels left and up. The offsets are in the bitmap's pixels, as crop
    * coordinates are.
    *
    * A picture's bitmap stays the picture shifted, so an output draws it sharply at any scale, and
    * a bitmap shifted again and again stays one shift of at most four tiles.
    */
  def wrapShift(dx: Int, dy: Int): PixelSource =
    val (width, height)                           = pixelSize
    val (base, baseSteps, (previousX, previousY)) = (steps.lastOption, unshiftedPlan) match
      case (Some(BitmapStep.WrapShift(x, y)), Some(unshifted)) => (unshifted, steps.init, (x, y))
      case _                                                   => (recordedPlan, steps, (0, 0))
    val shiftX = Math.floorMod(previousX + Math.floorMod(dx, width), width)
    val shiftY = Math.floorMod(previousY + Math.floorMod(dy, height), height)

    if shiftX == 0 && shiftY == 0 then
      // Shifts that add up to none leave the bitmap as it was before them.
      if baseSteps.length == steps.length then this
      else withSteps(baseSteps, boundary, base, None, pixelSize)
    else
      withSteps(
        baseSteps :+ BitmapStep.WrapShift(shiftX, shiftY),
        boundary,
        base.wrapShifted(
          pixelArea,
          shiftX * pixelArea.width / width,
          shiftY * pixelArea.height / height
        ),
        Some(base),
        pixelSize
      )
  end wrapShift

  def moveBy(xOffset: Double, yOffset: Double): PixelSource =
    at(boundary.moveBy(xOffset, yOffset))

  /** Each call is a separate read from the platform image, which in a browser is a canvas
    * read-back, so a loop over many pixels is far faster through `buffer(settings, timeMillis)`'s
    * `argbData` or `smile.pictures.PixelSource.transformARGB`.
    */
  def getColor(x: Int, y: Int, settings: RenderSettings = RenderSettings.Default): Color =
    buffer(settings, System.currentTimeMillis()).pixelColor(x, y)

  /** Merges this `PixelSource` with another `PixelSource` using a merging function on packed ARGB
    * integers, which boxes no [[smile.colors.Color]] per pixel. The pixels of both are computed
    * with `settings`, and the merge runs at once.
    *
    * @return
    *   The merged bitmap, anchored at this bitmap's upper left corner and sized to the element-wise
    *   minimum of the two bitmaps' dimensions.
    */
  def mergeARGBWith(
      another: PixelSource,
      pixelMerger: (Int, Int) => Int,
      settings: RenderSettings
  ): PixelSource =
    val now           = System.currentTimeMillis()
    val thesePixels   = buffer(settings, now)
    val anotherPixels = another.buffer(settings, now)
    val resultWidth   = thesePixels.width.min(anotherPixels.width)
    val resultHeight  = thesePixels.height.min(anotherPixels.height)
    // The merged pixels are anchored at this bitmap's upper left corner, so the
    // result keeps that corner fixed even when the other bitmap is smaller.
    val upperLeftCorner = boundary.upperLeftCorner
    withPixels(
      thesePixels.mergeARGBWith(anotherPixels, pixelMerger),
      Bounds(upperLeftCorner, upperLeftCorner + (resultWidth.toDouble, resultHeight.toDouble))
    )
  end mergeARGBWith

  def setColorsByLocation(generator: (Int, Int) => Color): PixelSource =
    // The new buffer keeps this bitmap's bounds, so the result sits where this bitmap sits.
    val (width, height) = pixelSize
    val newBuffer       = BufferAdapter(width, height)
    newBuffer.setColorsByLocation(generator)
    withPixels(newBuffer, boundary)

  /** Transforms each pixel of this bitmap with a function on packed ARGB integers, which boxes no
    * [[smile.colors.Color]] per pixel. The pixels are computed with `settings`, and the function
    * runs at once.
    */
  def transformARGB(transformer: Int => Int, settings: RenderSettings): PixelSource =
    withPixels(
      buffer(settings, System.currentTimeMillis()).transformARGB(transformer),
      boundary
    )
  end transformARGB

  /** Scales this bitmap by given horizontal and vertical factors in relation to a given point. The
    * factors apply to the bitmap's logical size, its boundary's. A negative factor mirrors the
    * pixels, like it mirrors any other element.
    */
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): PixelSource =
    scaleTo(
      horizontalFactor * boundary.width,
      verticalFactor * boundary.height,
      relativityPoint
    )

  /** Scales this bitmap to a given width and height in relation to a given point. A negative size
    * is that size mirrored.
    */
  def scaleTo(targetWidth: Double, targetHeight: Double, relativityPoint: Pos): PixelSource =
    if targetWidth.toInt == 0 || targetHeight.toInt == 0 then return PixelSource.Empty

    val newCenter =
      boundary.center.scaleBy(
        targetWidth / boundary.width,
        targetHeight / boundary.height,
        relativityPoint
      )

    // The mirroring is a flip before the scaling, and the buffers only ever scale to a positive
    // size, because the two platforms' buffers did not agree on what a negative one means.
    val mirrored = (targetWidth < 0, targetHeight < 0) match
      case (false, false) => this
      case (true, false)  => flipHorizontally
      case (false, true)  => flipVertically
      case (true, true)   => flipDiagonally

    val newBounds = Bounds(newCenter, targetWidth.abs, targetHeight.abs)
    mirrored.recording(
      BitmapStep.Scale(targetWidth.abs, targetHeight.abs),
      newBounds,
      mirrored.recordedPlan.followedBy(Affine.between(mirrored.boundary, newBounds))
    )
  end scaleTo

  /** Rotates this bitmap around a given point. Its center travels around the point, and its pixels
    * are turned by the same angle and resampled onto a new buffer.
    *
    * The new boundary is that buffer's, centered where the old center went. The buffer holds the
    * turned corners' extent rounded down to whole pixels, with the turned pixels in its middle, so
    * the boundary can be a fraction of a pixel smaller than the box around the turned corners but
    * shares its center. In exchange, the bitmap is drawn pixel for pixel at its buffer's size.
    */
  def rotateAround(angle: Double, centerOfRotation: Pos): PixelSource =
    val (width, height)       = pixelSize
    val step                  = BitmapStep.Rotate(angle)
    val (newWidth, newHeight) = step.sizeAfter(width, height)

    // A bitmap rendered for a dense display has more buffer pixels than logical ones, and the
    // turned buffer is as dense.
    val logicalWidth  = newWidth * boundary.width / width
    val logicalHeight = newHeight * boundary.height / height

    recording(
      step,
      Bounds(boundary.center.rotateAround(angle, centerOfRotation), logicalWidth, logicalHeight),
      recordedPlan.followedBy(Affine.rotation(angle, centerOfRotation))
    )
  end rotateAround

  /** The bitmap from before the turns this one's last step folds, set on the bitmaps that
    * `turnedAround` makes, so that turns that add up to none give it back.
    */
  @volatile private var beforeTurns: Option[PixelSource] = None

  /** This bitmap turned by `angle` degrees around `centerOfRotation`, as
    * `smile.pictures.PixelSource.rotateAround` turns it, except that a turn right after a turn
    * folds into one step by their total angle, which replays as one turn of the pixels from before
    * the first. However many times a bitmap is turned, it then records one step for it.
    *
    * A total within the tolerance of [[smile.modeling.Affine]] of a right angle is that right
    * angle, so that the replay is exact, and a total of whole turns leaves no turn recorded. It
    * gives the bitmap from before the first turn, moved by whatever the turns moved it.
    */
  private[smile] def turnedAround(angle: Double, centerOfRotation: Pos): PixelSource =
    (steps.lastOption, beforeTurns) match
      case (Some(BitmapStep.Rotate(previous)), Some(unturned)) =>
        val total  = PixelSource.snappedAngle((previous + angle) % Angles.FullTurn)
        val before = steps.init
        val plan   = recordedPlan.followedBy(Affine.rotation(angle, centerOfRotation))
        if total == 0 then
          // After the rotations cancel, the first pixel gives the remaining translation. Rounding
          // each turn to the 2^-20 grid can leave one or two grid steps of error after turning
          // back.
          val (now, was)       = (plan.tiles.head.toWorld, unturned.recordedPlan.tiles.head.toWorld)
          def moved(v: Double) = if v.abs <= PixelSource.TurnRounding then 0.0 else v
          val (dx, dy)         = (moved(now.e - was.e), moved(now.f - was.f))
          if dx == 0 && dy == 0 then unturned else unturned.moveBy(dx, dy)
        else
          // The size of the pixels before the turns, which the steps before them give.
          val start = unturned.pixelSize
          // The buffer pixels per logical pixel, which a turn keeps on each axis.
          val (perX, perY) =
            (boundary.width / pixelSize._1, boundary.height / pixelSize._2)
          val step    = BitmapStep.Rotate(total)
          val newSize = step.sizeAfter(start._1, start._2)
          val turned  = withSteps(
            before :+ step,
            Bounds(
              boundary.center.rotateAround(angle, centerOfRotation),
              newSize._1 * perX,
              newSize._2 * perY
            ),
            plan,
            None,
            newSize
          )
          turned.beforeTurns = Some(unturned)
          turned
      case _ =>
        val snapped = PixelSource.snappedAngle(angle % Angles.FullTurn)
        if snapped == 0 then this
        else
          val turned = rotateAround(snapped, centerOfRotation)
          turned.beforeTurns = Some(this)
          turned
  end turnedAround

  /** Reflects the bitmap across a vertical axis through its center. */
  def flipHorizontally: PixelSource =
    flipping(BitmapStep.FlipH, horizontally = true, vertically = false)

  /** Reflects the bitmap across a horizontal axis through its center. */
  def flipVertically: PixelSource =
    flipping(BitmapStep.FlipV, horizontally = false, vertically = true)

  /** Rotates the bitmap by 180 degrees around the center of its bounds. */
  def flipDiagonally: PixelSource =
    flipping(BitmapStep.FlipD, horizontally = true, vertically = true)

  /** This bitmap with a flip recorded, which mirrors it across its own center. */
  private def flipping(step: BitmapStep, horizontally: Boolean, vertically: Boolean): PixelSource =
    recording(
      step,
      boundary,
      recordedPlan.followedBy(Affine.mirroring(horizontally, vertically, pixelArea.center))
    )

  def crop(
      upperLeftXInPixels: Double,
      upperLeftYInPixels: Double,
      lowerRightXInPixels: Double,
      lowerRightYInPixels: Double
  ): PixelSource =
    // The crop coordinates are in the same world space as the boundary, so they are translated
    // into buffer-local pixel indices.
    val originX         = boundary.upperLeftCorner.x
    val originY         = boundary.upperLeftCorner.y
    val (width, height) = pixelSize

    val upperLeftX  = (upperLeftXInPixels - originX).floor
    val upperLeftY  = (upperLeftYInPixels - originY).floor
    val lowerRightX = (lowerRightXInPixels - originX).floor
    val lowerRightY = (lowerRightYInPixels - originY).floor

    val xMin = upperLeftX.min(lowerRightX)
    val xMax = upperLeftX.max(lowerRightX)
    val yMin = upperLeftY.min(lowerRightY)
    val yMax = upperLeftY.max(lowerRightY)

    require(xMin >= 0, s"X coordinate ${xMin + originX} is out of bounds")
    require(xMax <= width, s"X coordinate ${xMax + originX} is out of bounds")
    require(yMin >= 0, s"Y coordinate ${yMin + originY} is out of bounds")
    require(yMax <= height, s"Y coordinate ${yMax + originY} is out of bounds")

    val resultingWidth  = xMax - xMin
    val resultingHeight = yMax - yMin
    if resultingWidth < 1 || resultingHeight < 1 then return PixelSource.Empty

    val newBounds = Bounds(
      Pos(xMin + originX, yMin + originY),
      Pos(xMax + originX, yMax + originY)
    )

    val (left, top, right, bottom) = (xMin.toInt, yMin.toInt, xMax.toInt, yMax.toInt)
    steps.lastOption match
      // A crop of a crop is one crop of the pixels the first one cropped.
      case Some(BitmapStep.Crop(previousLeft, previousTop, _, _)) =>
        withSteps(
          steps.init :+ BitmapStep.Crop(
            previousLeft + left,
            previousTop + top,
            previousLeft + right,
            previousTop + bottom
          ),
          newBounds,
          recordedPlan.clippedTo(newBounds),
          None,
          (right - left, bottom - top)
        )
      case _ =>
        recording(
          BitmapStep.Crop(left, top, right, bottom),
          newBounds,
          recordedPlan.clippedTo(newBounds)
        )
  end crop

end PixelSource
