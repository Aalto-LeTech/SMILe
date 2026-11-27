package smile.infrastructure

import scala.annotation.tailrec
import scala.collection.mutable

import smile.{RenderSettings, ResamplingMode}
import smile.modeling.{Affine, Bounds, Pos, RasterInterpolation, RasterPlan, RasterTile}
import smile.modeling.{PathShape, PointShape, TextShape}
import smile.pictures.{
  AnimatedPicture,
  Bitmap,
  Clip,
  Element,
  MaskGroup,
  Picture,
  Rasters,
  Shape,
  SvgPicture,
  Wrap
}
import smile.pictures.{PixelSource, BitmapSource, ResamplingHint, StrokeStyle}

/** The shared draw plan. It turns an element, the time and an output's scale and settings into
  * `smile.infrastructure.DrawOp` operations in device pixels, which every backend interprets the
  * same way.
  *
  * Every decision about whole device pixels is made here, once, on the composed map from a local
  * frame to device pixels, with the rules of `smile.infrastructure.DeviceGrid`:
  *
  *   - An axis-aligned clip is a hard clip of the device pixels its rectangle covers. A turned or
  *     sheared clip is a layer with an antialiased clip.
  *   - A mask is a layer of the device pixels the group covers.
  *   - A bitmap whose map copies its pixels one to one onto device pixels has its device corner
  *     moved to the nearest whole device pixel and is `snapped`, which means it is copied as it is.
  *     Such a map is a move, quarter turns and mirrors, at one device pixel per source pixel. Every
  *     other bitmap is resampled with its hint at its exact place, so a sprite scaled a little
  *     every frame, or a 1x sprite on a 1.5x screen, never jumps to a whole pixel.
  *   - A point that only moves is the device pixel its corner snaps to. Any other point is the path
  *     of its 1x1 square under its map. The square's device corner is moved to the nearest whole
  *     device pixel whenever the map keeps the axes, scaled or not, where a bitmap's is moved only
  *     when it is copied one to one.
  *   - A wrap draws copies of its content under a clip of its period. Where its map keeps the axes
  *     and the period is a whole number of device pixels from a whole device pixel, the content is
  *     drawn up to four times, moved by whole device pixels. Otherwise the content is drawn into
  *     one tile of the period rounded to whole device pixels. The tile is drawn from an offset
  *     snapped to whole pixels and stepped by exactly its size as often as it takes to cover the
  *     clip, so copies abut and never overlap. A plan made for SVG output takes a tile for a period
  *     on whole device pixels too and marks it `vector`, so that the SVG backend repeats it as one
  *     pattern fill and keeps its content as vectors.
  *
  * Vector geometry, strokes and text are never snapped.
  */
private[smile] object DrawPlan:

  /** The device pixels that `element` covers on `grid`, and the operations that draw it with
    * `settings` at the instant `timeMillis`, back first. The affines in the operations take local
    * frames to the device pixels of the whole grid. A backend draws the box's corner at its origin
    * by moving everything by minus the corner. The empty picture covers no pixels and draws
    * nothing.
    *
    * With `svg`, the plan is one for SVG output. Every wrap is then a
    * `smile.infrastructure.DrawOp.Tiles`, which the SVG backend draws as one pattern fill, and the
    * tiles of a wrap whose period is on whole device pixels are marked `vector`, so that their
    * content stays vectors. Without it, such a wrap is drawn as moved copies of its content.
    */
  def of(
      element: Element,
      grid: DeviceGrid,
      settings: RenderSettings,
      timeMillis: Long,
      svg: Boolean = false
  ): (DeviceBox, Vector[DrawOp]) =
    val box = element.bounds.fold(DeviceBox(0, 0, 0, 0))(grid.covering)
    (box, Traversal.fold(element, new Planner(settings, timeMillis, svg), grid.toDevice))

  /** How many layers and tiles a plan from `smile.infrastructure.DrawPlan.planned` nests by default
    * before it draws the deeper ones ahead, as slots.
    */
  val MaxNesting: Int = 16

  /** A layer that a plan draws ahead into a buffer of the device pixels `region`, the part of its
    * box that can show, as a painter draws a layer's buffer. A
    * `smile.infrastructure.DrawOp.Prepared` of the slot composites that buffer.
    */
  final case class Slot(layer: DrawOp.Layer, region: DeviceBox) derives CanEqual

  /** The device pixels `box` that an element covers, and the operations that draw it. The `slots`
    * are drawn ahead in order, and `ops` are drawn onto the box, back first. A slot's layer and the
    * ops refer only to earlier slots, and to each slot exactly once.
    */
  final case class Plan(box: DeviceBox, slots: Vector[Slot], ops: Vector[DrawOp]) derives CanEqual

  /** The plan of `element` on `grid` with `settings` at the instant `timeMillis`, as
    * `smile.infrastructure.DrawPlan.of` makes it, with its nesting bounded by `nesting` (see
    * `smile.infrastructure.DrawPlan.bounded`), so that a painter that recurses once per layer, hard
    * clip and tile draws a tree of any depth. `svg` asks for a plan for SVG output, as it does in
    * `of`.
    */
  def planned(
      element: Element,
      grid: DeviceGrid,
      settings: RenderSettings,
      timeMillis: Long,
      nesting: Int = MaxNesting,
      svg: Boolean = false
  ): Plan =
    val (box, ops)     = of(element, grid, settings, timeMillis, svg)
    val (slots, drawn) = bounded(ops, box, nesting)
    Plan(box, slots, drawn)

  /** The deepest chain of layers, hard clips and tiles in `ops`, each of which counts once, walked
    * with a stack of its own.
    */
  private[infrastructure] def nestingOf(ops: Vector[DrawOp]): Int =
    var deepest = 0
    // Each operation still to measure, with the number of layers, hard clips and tiles around it.
    val pending = mutable.Stack.from(ops.map(_ -> 0))
    while pending.nonEmpty do
      val (op, around) = pending.pop()
      val inner        = op match
        case layer: DrawOp.Layer      => Some(DrawOp.parts(layer).flatten)
        case DrawOp.HardClip(_, ops)  => Some(ops)
        case DrawOp.Tiles(tile, _, _) => Some(DrawOp.parts(tile).flatten)
        case _                        => None
      inner.foreach: ops =>
        deepest = deepest.max(around + 1)
        pending.pushAll(ops.map(_ -> (around + 1)))
    deepest

  /** Whether `ops`, from `smile.infrastructure.DrawPlan.of`, put something on whole device pixels,
    * where the plan for another scale would put it on others. That is a point, as a pixel or as its
    * square from a snapped corner where its map keeps the axes, a hard clip, a layer of a mask or
    * of a turned clip, whose box is whole device pixels, tiles, a snapped image, and an image whose
    * map keeps the axes and that has a clip, on the device pixels whose centers it covers. Paths,
    * text, SVG documents and other images are drawn at their exact place. Hard clips, layers and
    * tiles are the only operations that hold others, and they round themselves, so only `ops` are
    * read.
    */
  def roundsToDevicePixels(ops: Vector[DrawOp]): Boolean =
    ops.exists:
      case _: DrawOp.Pixel | _: DrawOp.HardClip | _: DrawOp.Layer | _: DrawOp.Tiles => true
      case DrawOp.Path(m, shape, _, _) => (shape eq PointSquare) && m.keepsAxes
      case image: DrawOp.Image => image.snapped || (image.m.keepsAxes && image.clip.isDefined)
      case _                   => false

  /** Whether `ops` draw each of `slots` (see `smile.infrastructure.DrawPlan.Plan`), by the number
    * of the slot. A slot is drawn when `ops` refer to it or a drawn slot does. They are found from
    * the last slot back, since a slot refers only to earlier ones. The tiles that `opens` is false
    * for are not looked into, and neither are the slots that only they draw. Each operation is
    * walked with a stack of its own, and none is walked when there are no slots.
    */
  private[infrastructure] def drawnSlots(
      slots: Vector[Slot],
      ops: Vector[DrawOp],
      opens: DrawOp.Tiles => Boolean = _ => true
  ): Array[Boolean] =
    val drawn                           = new Array[Boolean](slots.size)
    def mark(ops: Vector[DrawOp]): Unit =
      val pending = mutable.Stack.from(ops)
      while pending.nonEmpty do
        pending.pop() match
          case DrawOp.Prepared(slot, _)  => drawn(slot) = true
          case layer: DrawOp.Layer       => pending.pushAll(DrawOp.parts(layer).flatten)
          case DrawOp.HardClip(_, inner) => pending.pushAll(inner)
          case tiles: DrawOp.Tiles       =>
            if opens(tiles) then pending.pushAll(DrawOp.parts(tiles.tile).flatten)
          case _ => ()
    if slots.nonEmpty then
      mark(ops)
      for i <- slots.indices.reverse if drawn(i) do mark(Vector(slots(i).layer))
    drawn

  /** The slots and the operations that draw `ops` onto the device pixels `target` exactly as they
    * do, nested at most about twice `nesting`. That is `(Vector(), ops)`, the same `ops`, when they
    * nest no deeper than `nesting` (see `smile.infrastructure.DrawPlan.nestingOf`).
    *
    * A deeper plan is rebuilt in one walk from the top, with a stack of its own, which tracks the
    * device pixels each operation can show on as a painter does:
    *
    *   - A layer or a tile that is the `nesting`-th on its path from the top of the plan, or of the
    *     slot it is in, is drawn ahead as the next slot, after the slots inside it. A layer is
    *     replaced by a `smile.infrastructure.DrawOp.Prepared` of its region, the part of its box
    *     that can show, and a tile, whose copies stay where they are, by a layer that copies the
    *     slot onto its cleared buffer.
    *   - A hard clip directly inside a hard clip is one hard clip of both boxes, between hard clips
    *     of the outer box around the operations before and after it, and a hard clip of no pixels
    *     is left out with its operations, as is a layer or a tile of no pixels, which a painter
    *     draws nothing of. Hard clips on whole device pixels intersect exactly on every backend, so
    *     this draws exactly what the nested clips drew, and it keeps the hard clips between two
    *     layers to one.
    */
  private[infrastructure] def bounded(
      ops: Vector[DrawOp],
      target: DeviceBox,
      nesting: Int
  ): (Vector[Slot], Vector[DrawOp]) =
    if nestingOf(ops) <= nesting then (Vector(), ops)
    else
      val slots = mutable.ArrayBuffer[Slot]()
      // The operation that takes the place of the given layer, which is drawn ahead as the next
      // slot into a buffer of the given region.
      def prepared(layer: DrawOp.Layer, region: DeviceBox): DrawOp =
        slots += Slot(layer, region)
        DrawOp.Prepared(slots.size - 1, region)

      val stack = mutable.Stack(new Bounding(Rebuilt.Plan, Vector(ops), target, 0))

      // Starts bounding the operation op, met among the parts of the frame outer.
      def begin(outer: Bounding, op: DrawOp): Unit =
        // Whether a layer or tile here reaches the nesting limit on its path, which makes it a
        // slot, and the layers and tiles above its parts, counted again from the slot's own.
        val hoisted = outer.layers + 1 >= nesting
        val layers  = if hoisted then 1 else outer.layers + 1
        op match
          case DrawOp.HardClip(box, inner) =>
            val clip = outer.rebuilt match
              case Rebuilt.Clip(around, _) => intersection(around, box)
              case _                       => box
            if hasPixels(clip) then
              // A clip inside a clip ends the outer one's run, and its own runs follow it.
              val into = outer.rebuilt match
                case Rebuilt.Clip(around, into) => endRun(outer, around, into); into
                case _                          => outer.adding
              val visible = intersection(outer.visible, clip)
              stack.push(
                new Bounding(Rebuilt.Clip(clip, into), Vector(inner), visible, outer.layers)
              )
          case layer: DrawOp.Layer =>
            val region = intersection(layer.box, outer.visible)
            if hasPixels(region) then
              stack.push(
                new Bounding(
                  Rebuilt.Layered(layer, region, hoisted),
                  DrawOp.parts(layer),
                  region,
                  layers
                )
              )
          case tiles: DrawOp.Tiles =>
            val tile = tiles.tile
            if hasPixels(tile.box) then
              stack.push(
                new Bounding(Rebuilt.Tiled(tiles, hoisted), DrawOp.parts(tile), tile.box, layers)
              )
          case _ => outer.add(Vector(op))

      // The operations that the given frame is rebuilt into, its parts bounded. A hard clip's
      // have gone into the operations around it already.
      def finish(frame: Bounding): Vector[DrawOp] =
        frame.rebuilt match
          case Rebuilt.Plan            => frame.drawnParts.head
          case Rebuilt.Clip(box, into) =>
            endRun(frame, box, into)
            Vector()
          case Rebuilt.Layered(layer, region, hoisted) =>
            val drawn = DrawOp.withParts(layer, frame.drawnParts)
            Vector(if hoisted then prepared(drawn, region) else drawn)
          case Rebuilt.Tiled(tiles, hoisted) =>
            val tile  = DrawOp.withParts(tiles.tile, frame.drawnParts)
            val drawn =
              if !hoisted then tile
              else new DrawOp.Layer(tile.box, None, Vector(), Vector(prepared(tile, tile.box)))
            Vector(DrawOp.Tiles(drawn, tiles.copies, tiles.vector))

      @tailrec def walk(): Vector[DrawOp] =
        val top = stack.top
        if top.hasNext then
          begin(top, top.next())
          walk()
        else
          val drawn = finish(stack.pop())
          if stack.isEmpty then drawn
          else
            stack.top.add(drawn)
            walk()
      val drawn = walk()
      (slots.toVector, drawn)
  end bounded

  /** What an operation that `smile.infrastructure.DrawPlan.bounded` walks is rebuilt into. */
  private enum Rebuilt:
    case Plan

    /** A hard clip of `box`, which takes in the hard clips directly inside it. Its runs of other
      * operations, each under a hard clip of `box`, go straight `into` the operations of the
      * nearest operation around it that is not a hard clip, between the runs of the clips inside
      * it, so that a chain of clips is rebuilt in time linear in its length.
      */
    case Clip(box: DeviceBox, into: mutable.Builder[DrawOp, Vector[DrawOp]])

    /** `layer`, whose pixels `region` can show, drawn ahead as a slot when `hoisted`. */
    case Layered(layer: DrawOp.Layer, region: DeviceBox, hoisted: Boolean)

    /** `tiles`, whose tile is drawn ahead as a slot when `hoisted`. */
    case Tiled(tiles: DrawOp.Tiles, hoisted: Boolean)

  /** An operation that `smile.infrastructure.DrawPlan.bounded` walks, rebuilt into `rebuilt`. Its
    * `parts` (see `smile.infrastructure.DrawOp.parts`) can show on the device pixels `visible`,
    * under `layers` layers and tiles from the top of the plan or of the slot it is in. `results`
    * holds each part's operations bounded so far.
    */
  private final class Bounding(
      val rebuilt: Rebuilt,
      parts: Vector[Vector[DrawOp]],
      val visible: DeviceBox,
      val layers: Int
  ):
    val results: Vector[mutable.Builder[DrawOp, Vector[DrawOp]]] =
      parts.map(_ => Vector.newBuilder[DrawOp])

    private val pending = parts.iterator.zipWithIndex.flatMap((ops, i) => ops.iterator.map(i -> _))
    private var part    = 0

    def hasNext: Boolean = pending.hasNext

    /** The next operation of the parts, whose part takes what is added until the next one. */
    def next(): DrawOp =
      val (i, op) = pending.next()
      part = i
      op

    /** Each part's operations, bounded. */
    def drawnParts: Vector[Vector[DrawOp]] = results.map(_.result())

    /** The operations of the part of the last operation taken, bounded so far. */
    def adding: mutable.Builder[DrawOp, Vector[DrawOp]] = results(part)

    def add(ops: Vector[DrawOp]): Unit =
      val _ = adding ++= ops
  end Bounding

  /** Ends the run of operations that `clip`, the frame of a hard clip of `box`, has gathered. Adds
    * them `into` its operations under a hard clip of `box`, and starts a new run.
    */
  private def endRun(
      clip: Bounding,
      box: DeviceBox,
      into: mutable.Builder[DrawOp, Vector[DrawOp]]
  ): Unit =
    val run = clip.results.head.result()
    if run.nonEmpty then into += DrawOp.HardClip(box, run)
    clip.results.head.clear()

  private[infrastructure] def hasPixels(box: DeviceBox): Boolean = box.width > 0 && box.height > 0

  /** How a backend with `settings` resamples the pixels of `image`. That is nearest neighbor for a
    * `Pixelated` hint, bilinear for a `Smooth` one, and for `Auto`, the settings'
    * `bufferTransformMethod` where the image's map turns the axes and their `bufferScalingMethod`
    * elsewhere.
    */
  def interpolation(image: DrawOp.Image, settings: RenderSettings): RasterInterpolation =
    image.interpolation match
      case ResamplingHint.Pixelated => RasterInterpolation.Nearest
      case ResamplingHint.Smooth    => RasterInterpolation.Bilinear
      case ResamplingHint.Auto      =>
        RasterPlan(Vector(RasterTile(image.m, None))).interpolation(settings)

  /** The pixels that `image` draws where only the device pixels `visible` can show, with `settings`
    * in a drawing at the instant `timeMillis`, and the map that takes them to device pixels. The
    * pixels are the replayed pixels of a `Stepwise` output, the buffer of a pixel source, or the
    * part of a picture source that the device pixels shown need, rendered at the density its map
    * needs. A picture source not rendered yet is rendered at that instant. `None` when no device
    * pixel shows, or when the map of a picture source cannot be undone.
    */
  private[infrastructure] def imagePixels(
      image: DrawOp.Image,
      visible: DeviceBox,
      settings: RenderSettings,
      timeMillis: Long
  ): Option[(NativeBuffer, Affine)] =
    shownOf(image, visible).flatMap: shown =>
      if image.replayed then Some((image.source.buffer(settings, timeMillis), image.m))
      else
        image.source.source match
          case BitmapSource.Pixels(buffer) => Some((buffer, image.m))
          case BitmapSource.Content(_)     => contentPixels(image, shown, settings, timeMillis)

  /** The device pixels that `image` can show on where only the device pixels `visible` can. They
    * are those that its tile's clip covers, or `None` when there are none.
    */
  private def shownOf(image: DrawOp.Image, visible: DeviceBox): Option[DeviceBox] =
    val shown = image.clip.fold(visible): corners =>
      intersection(visible, DeviceGrid.coveringDevice(boxAround(corners)))
    Option.when(hasPixels(shown))(shown)

  /** The pixels of the picture source of `image` that the device pixels `shown` need, rendered at
    * the density its map needs at the instant `timeMillis` unless they are kept, and the map that
    * takes them to device pixels, or `None` when the map cannot be undone.
    */
  private def contentPixels(
      image: DrawOp.Image,
      shown: DeviceBox,
      settings: RenderSettings,
      timeMillis: Long
  ): Option[(NativeBuffer, Affine)] =
    sourceNeeds(image, shown).flatMap: (needed, density) =>
      image.source
        .contentPixelsCovering(
          needed.upperLeftCorner.x,
          needed.upperLeftCorner.y,
          needed.lowerRightCorner.x,
          needed.lowerRightCorner.y,
          density,
          settings,
          timeMillis
        )
        .map: (region, pixels) =>
          // The region's pixels are each that much smaller, and start at its corner.
          val toRegion = Affine
            .scaling(1 / density, 1 / density)
            .andThen(Affine.translation(region.left, region.top))
          (pixels, toRegion.andThen(image.m))

  /** The box of the source pixels of `image` that the device pixels `shown` show, and the density
    * its map needs them rendered at, or `None` when the map cannot be undone.
    */
  private def sourceNeeds(image: DrawOp.Image, shown: DeviceBox): Option[(Bounds, Double)] =
    image.m.inverseOption.map: toSource =>
      val corners = Vector(
        Pos(shown.left, shown.top),
        Pos(shown.left + shown.width, shown.top),
        Pos(shown.left + shown.width, shown.top + shown.height),
        Pos(shown.left, shown.top + shown.height)
      ).map(toSource(_))
      (boxAround(corners), PixelSource.contentDensity(image.m.largestAxisScale))

  /** The renderings of picture sources that a painter asks for, kept or not, while it draws `plan`
    * with `settings`. For each image of a picture source that shows, that is its source and the
    * rendering of it that `smile.infrastructure.DrawPlan.imagePixels` takes the image's pixels
    * from, or replays them from. The operations are walked with a stack of their own, each with the
    * device pixels it can show on as the painters narrow them. A slot's are narrowed to its region,
    * and a layer's and a hard clip's to their boxes. Every slot is walked, since a plan draws all
    * of its slots.
    */
  private[smile] def sourceRenderings(
      plan: Plan,
      settings: RenderSettings
  ): Vector[(PixelSource, PixelSource.SourceKey)] =
    val found   = Vector.newBuilder[(PixelSource, PixelSource.SourceKey)]
    val pending = mutable.Stack.from(plan.ops.map(_ -> plan.box))
    for slot <- plan.slots do
      pending.pushAll(DrawOp.parts(slot.layer).flatten.map(_ -> slot.region))
    while pending.nonEmpty do
      pending.pop() match
        case (image: DrawOp.Image, visible) =>
          for shown <- shownOf(image, visible) do
            val source = image.source
            val asked  =
              if image.replayed then source.renderingForPixels(settings)
              else
                source.source match
                  case BitmapSource.Pixels(_)  => None
                  case BitmapSource.Content(_) =>
                    sourceNeeds(image, shown).map: (needed, density) =>
                      source.renderingCovering(
                        needed.upperLeftCorner.x,
                        needed.upperLeftCorner.y,
                        needed.lowerRightCorner.x,
                        needed.lowerRightCorner.y,
                        density,
                        settings
                      )
            found ++= asked.map(source -> _)
        case (layer: DrawOp.Layer, visible) =>
          val region = intersection(layer.box, visible)
          if hasPixels(region) then pending.pushAll(DrawOp.parts(layer).flatten.map(_ -> region))
        case (DrawOp.HardClip(box, ops), visible) =>
          pending.pushAll(ops.map(_ -> intersection(visible, box)))
        case (DrawOp.Tiles(tile, _, _), _) =>
          if hasPixels(tile.box) then pending.pushAll(DrawOp.parts(tile).flatten.map(_ -> tile.box))
        case _ => ()
    found.result()

  /** The source pixels of an image of `width` by `height` pixels, which `m` takes to device pixels,
    * that reach into the device polygon `clip`, as a box of whole source pixels within the image.
    * Where `m` keeps the axes, they are the ones the centers of the clip's device pixels lie on,
    * and elsewhere, the ones the clip touches. `None` where that is the whole image, or where `m`
    * cannot be undone. A backend that resamples a cropped image draws it from these pixels alone,
    * with their edges repeated, so that nothing outside the crop is sampled into it.
    */
  private[infrastructure] def sourceWindow(
      clip: Vector[Pos],
      m: Affine,
      width: Int,
      height: Int
  ): Option[DeviceBox] =
    m.inverseOption.flatMap: toSource =>
      val box =
        if !m.keepsAxes then
          // A source pixel the clip only grazes, by the float error of mapping it back, is left
          // out. The edge pixels next to it are repeated in its place.
          val touched = boxAround(clip.map(toSource(_)))
          val (l, t)  = (
            math.floor(touched.upperLeftCorner.x + WindowTolerance).toInt,
            math.floor(touched.upperLeftCorner.y + WindowTolerance).toInt
          )
          DeviceBox(
            l,
            t,
            math.ceil(touched.lowerRightCorner.x - WindowTolerance).toInt - l,
            math.ceil(touched.lowerRightCorner.y - WindowTolerance).toInt - t
          )
        else
          // The clip is on whole device pixels (see smile.infrastructure.DrawOp.Image), and the
          // pixels it keeps show the source pixels their centers lie on.
          val pixels  = boxAround(clip)
          val centers = Element.mapBox(
            toSource,
            Bounds(
              pixels.upperLeftCorner.x + 0.5,
              pixels.upperLeftCorner.y + 0.5,
              (pixels.lowerRightCorner.x - 0.5).max(pixels.upperLeftCorner.x + 0.5),
              (pixels.lowerRightCorner.y - 0.5).max(pixels.upperLeftCorner.y + 0.5)
            )
          )
          val (l, t) = (
            math.floor(centers.upperLeftCorner.x + WindowTolerance).toInt,
            math.floor(centers.upperLeftCorner.y + WindowTolerance).toInt
          )
          DeviceBox(
            l,
            t,
            math.floor(centers.lowerRightCorner.x - WindowTolerance).toInt + 1 - l,
            math.floor(centers.lowerRightCorner.y - WindowTolerance).toInt + 1 - t
          )
      val (l, t) = (box.left.max(0), box.top.max(0))
      val (r, b) = ((box.left + box.width).min(width), (box.top + box.height).min(height))
      if r <= l || b <= t || (l == 0 && t == 0 && r == width && b == height) then None
      else Some(DeviceBox(l, t, r - l, b - t))

  /** How far, in source pixels, `smile.infrastructure.DrawPlan.sourceWindow` lets a clip mapped
    * back onto the source miss a pixel edge. The maps of a bitmap's tiles carry float error far
    * larger than a double's.
    */
  private val WindowTolerance = 1e-4

  /** The device pixels both `a` and `b` cover, which may be none. */
  private[infrastructure] def intersection(a: DeviceBox, b: DeviceBox): DeviceBox =
    val (left, top) = (a.left.max(b.left), a.top.max(b.top))
    val right       = (a.left + a.width).min(b.left + b.width)
    val bottom      = (a.top + a.height).min(b.top + b.height)
    DeviceBox(left, top, (right - left).max(0), (bottom - top).max(0))

  /** The box around `points`, of which there is at least one. */
  private[infrastructure] def boxAround(points: Vector[Pos]): Bounds =
    Bounds(
      points.iterator.map(_.x).min,
      points.iterator.map(_.y).min,
      points.iterator.map(_.x).max,
      points.iterator.map(_.y).max
    )

  /** Where a gradient's element-local coordinates (see [[smile.colors.LinearGradient]]) start in
    * the local frame of a `smile.infrastructure.DrawOp.Path` or `smile.infrastructure.DrawOp.Text`.
    * That is the upper left corner of `box` widened by the radius of `stroke`, as an element's box
    * reserves room for its stroke. `box` is a path's local box or a text's layout box. Every
    * backend paints gradients from this point, under the operation's affine.
    */
  def paintOrigin(box: Bounds, stroke: Option[StrokeStyle]): Pos =
    val radius = stroke.fold(0.0)(_.width / 2)
    Pos(box.upperLeftCorner.x - radius, box.upperLeftCorner.y - radius)

  /** The 1x1 square of a point, centered at its local origin, as a path. */
  private val PointSquare = PathShape(IArray(-0.5, -0.5, 0.5, -0.5, 0.5, 0.5, -0.5, 0.5), true)

  private val NoBox = DeviceBox(0, 0, 0, 0)

  private def isTranslation(m: Affine): Boolean = m.a == 1 && m.b == 0 && m.c == 0 && m.d == 1

  /** Whether `m` copies a pixel grid one to one onto the device grid. It keeps the axes and takes a
    * pixel to exactly one device pixel, so that it moves, turns by right angles or mirrors the
    * pixels and resamples none of them.
    */
  private def copiesOneToOne(m: Affine): Boolean =
    def unit(v: Double) = DeviceGrid.isWhole(v) && math.rint(v.abs) == 1
    (m.b == 0 && m.c == 0 && unit(m.a) && unit(m.d)) ||
    (m.a == 0 && m.d == 0 && unit(m.b) && unit(m.c))

  /** The move that takes the upper left corner of the device box `box` to the nearest whole device
    * pixel.
    */
  private def cornerSnap(box: Bounds): Affine =
    val (x, y) = (box.upperLeftCorner.x, box.upperLeftCorner.y)
    Affine.translation(DeviceGrid.snapDevice(x) - x, DeviceGrid.snapDevice(y) - y)

  /** How a wrap is drawn, in device pixels. */
  private enum WrapLayout:
    /** Its content, planned under the wrap's own map, drawn moved by each of `moves`. */
    case Copies(moves: Vector[(Int, Int)])

    /** A tile of `width` by `height` device pixels. The plan maps its content into tile coordinates
      * with `toTile` and draws the result at each of `copies`.
      */
    case Tiled(toTile: Affine, width: Int, height: Int, copies: Vector[Affine])

  private final class Planner(settings: RenderSettings, timeMillis: Long, svg: Boolean)
      extends Traversal.Fold[Vector[DrawOp]]:

    def empty: Vector[DrawOp] = Vector()

    def shape(shape: Shape, m: Affine): Vector[DrawOp] =
      if shape.fill.isEmpty && shape.stroke.isEmpty then Vector()
      else
        shape.shape match
          case text: TextShape =>
            Vector(DrawOp.Text(m, text.layout, shape.fill, shape.stroke.map(_.forGlyphs)))
          case _: PointShape =>
            shape.fill.toVector.map: fill =>
              if isTranslation(m) then
                // The pixel whose corner the point's corner, half a pixel up and left, snaps to.
                DrawOp.Pixel(
                  DeviceGrid.snapDevice(m.e - 0.5).toInt,
                  DeviceGrid.snapDevice(m.f - 0.5).toInt,
                  fill.paint
                )
              else
                val snap =
                  if m.keepsAxes then cornerSnap(Element.mapBox(m, PointSquare.localBox))
                  else Affine.Identity
                DrawOp.Path(m.andThen(snap), PointSquare, shape.fill, None)
          // A path of one point paints nothing on every backend, as the one-point polygon is
          // documented to. SVG's renderers would draw the caps of its stroke at some sizes.
          case path: PathShape if path.xy.length < 4 => Vector()
          case other                                 =>
            Vector(DrawOp.Path(m, other, shape.fill, shape.stroke))

    def bitmap(bitmap: Bitmap, m: Affine): Vector[DrawOp] =
      val source = bitmap.source
      val plan   = source.exactPlan
      if settings.resamplingMode == ResamplingMode.Stepwise || !plan.isFinite then
        replayed(bitmap, m)
      else
        val tiles    = plan.tiles.filterNot(_.isEmpty)
        val composed = tiles.map(_.toWorld.andThen(m))
        // Only pixels copied one to one are moved onto whole device pixels. Resampled ones are
        // drawn where they are, so that a bitmap scaled a little at a time moves smoothly.
        val snap =
          if !composed.forall(copiesOneToOne) then Affine.Identity
          else Rasters.boundsThrough(source, m).fold(Affine.Identity)(cornerSnap)
        val placed          = m.andThen(snap)
        val (width, height) =
          (source.wholeSource.right.toDouble, source.wholeSource.bottom.toDouble)
        tiles
          .zip(composed)
          .map: (tile, toDevice) =>
            val drawn  = toDevice.andThen(snap)
            val corner = drawn.boxBounds(0, 0, width, height).upperLeftCorner
            // One snap moves every tile alike, so that the tiles keep their seams. A tile counts
            // as snapped only where that also puts its own corner on a whole device pixel.
            val snapped = copiesOneToOne(toDevice) &&
              DeviceGrid.isWhole(corner.x) && DeviceGrid.isWhole(corner.y)
            val clip = tile.clip.map(_.map(placed(_)))
            DrawOp.Image(
              source,
              drawn,
              if drawn.keepsAxes then centersClip(clip, drawn, width, height) else clip,
              snapped,
              source.resampling,
              replayed = false
            )

    /** The clip of an image of `width` by `height` source pixels that `drawn` lays on the device
      * keeping the axes. It is the device pixels whose centers lie inside both `clip` and the
      * image's extent, as a rectangle on whole device pixels (see
      * `smile.infrastructure.DeviceGrid.centersCovered`). `None` where there is no clip and the
      * extent is on whole device pixels already, so that a sprite on the grid needs no clip.
      *
      * The snap that moves a bitmap's corner onto a whole device pixel moves its clip and its
      * extent by up to half a device pixel, which can take an edge past the device pixels the
      * bitmap's bounds cover, but never past the centers of the pixels just outside them. So
      * rounded this way, the clip keeps every backend's ink within those pixels, whether its clips
      * are antialiased or not. A clip that is not a rectangle on the device's axes is kept as it
      * is.
      */
    private def centersClip(
        clip: Option[Vector[Pos]],
        drawn: Affine,
        width: Double,
        height: Double
    ): Option[Vector[Pos]] =
      val extent                     = drawn.boxBounds(0, 0, width, height)
      val (left, top, right, bottom) =
        (
          extent.upperLeftCorner.x,
          extent.upperLeftCorner.y,
          extent.lowerRightCorner.x,
          extent.lowerRightCorner.y
        )
      clip match
        case Some(corners) if !isUpright(corners)                             => clip
        case None if Seq(left, top, right, bottom).forall(DeviceGrid.isWhole) => None
        case _                                                                =>
          val box    = clip.fold(extent)(boxAround)
          val pixels = DeviceGrid.centersCovered(
            box.upperLeftCorner.x.max(left),
            box.upperLeftCorner.y.max(top),
            box.lowerRightCorner.x.min(right),
            box.lowerRightCorner.y.min(bottom)
          )
          val (l, t) = (pixels.left.toDouble, pixels.top.toDouble)
          val (r, b) = (l + pixels.width, t + pixels.height)
          Some(Vector(Pos(l, t), Pos(r, t), Pos(r, b), Pos(l, b)))
    end centersClip

    /** Whether every edge of the polygon `corners` runs along one of the device's axes. */
    private def isUpright(corners: Vector[Pos]): Boolean =
      corners.nonEmpty && corners
        .zip(corners.drop(1) :+ corners.head)
        .forall: (from, to) =>
          (from.x - to.x).abs < 1e-9 || (from.y - to.y).abs < 1e-9

    /** The replayed pixels of `bitmap`'s source, laid where they lie in its frame, under `m`. */
    private def replayed(bitmap: Bitmap, m: Affine): Vector[DrawOp] =
      val source          = bitmap.source
      val (width, height) = source.pixelSize
      if width <= 0 || height <= 0 then Vector()
      else
        val area     = source.pixelArea
        val toDevice = Affine.between(Bounds(0, 0, width, height), area).andThen(m)
        val oneToOne = copiesOneToOne(toDevice)
        val snap     = if oneToOne then cornerSnap(Element.mapBox(m, area)) else Affine.Identity
        val drawn    = toDevice.andThen(snap)
        Vector(
          DrawOp.Image(
            source,
            drawn,
            if drawn.keepsAxes then centersClip(None, drawn, width, height) else None,
            oneToOne,
            source.resampling,
            replayed = true
          )
        )

    def svg(svg: SvgPicture, m: Affine): Vector[DrawOp] = Vector(DrawOp.Foreign(svg.document, m))

    def picture(picture: Picture, m: Affine, parts: Vector[Vector[DrawOp]]): Vector[DrawOp] =
      // The first child is in front, so it is drawn last.
      parts.reverseIterator.flatten.toVector

    def clip(clip: Clip, m: Affine, content: Vector[DrawOp]): Vector[DrawOp] =
      if content.isEmpty then content
      else clipped(m, clip.rect, content)

    def foldsMasks: Boolean = true

    def mask(
        group: MaskGroup,
        m: Affine,
        masks: Vector[Vector[DrawOp]],
        masked: Vector[DrawOp]
    ): Vector[DrawOp] =
      if masked.isEmpty then masked
      else
        val box = group.boundsThrough(m).fold(NoBox)(DeviceGrid.coveringDevice)
        Vector(DrawOp.Layer(box, None, masks.zip(group.masks.map(_._2)), masked))

    override def framesOf(animation: AnimatedPicture): Vector[Int] =
      Vector(animation.frameIndexAt(timeMillis))

    def animation(
        animation: AnimatedPicture,
        m: Affine,
        frames: Vector[Vector[DrawOp]]
    ): Vector[DrawOp] = frames.head

    override def wrapContent(wrap: Wrap, m: Affine): Option[Affine] =
      wrapLayout(wrap, m) match
        case WrapLayout.Copies(_)              => Some(m)
        case WrapLayout.Tiled(toTile, _, _, _) => Some(toTile)

    def wrap(wrap: Wrap, m: Affine, content: Option[Vector[DrawOp]]): Vector[DrawOp] =
      val ops   = content.getOrElse(Vector())
      val drawn = wrapLayout(wrap, m) match
        case WrapLayout.Copies(moves) =>
          // A copy draws only on the device pixels of the wrap's clip, so only the parts of it
          // that meet them are kept, and nested wraps keep only the pieces of each other that show.
          val shown = DeviceGrid.coveringDevice(Element.mapBox(m, wrap.period))
          for
            (dx, dy) <- moves
            op       <- ops
            copy     <- op.movedBy(dx, dy, shown)
          yield copy
        case WrapLayout.Tiled(_, width, height, copies) =>
          if ops.isEmpty then Vector()
          else
            val tile = new DrawOp.Layer(DeviceBox(0, 0, width, height), None, Vector(), ops)
            Vector(DrawOp.Tiles(tile, copies, vector = svg && wholePeriod(wrap, m)))
      if drawn.isEmpty then drawn else clipped(m, wrap.period, drawn)

    /** Whether `m` keeps the axes and puts the period of `wrap` on whole device pixels, a whole
      * number of them on each side from a whole device pixel. Copies of the content abut cleanly
      * there. A plan for SVG marks the tile of such a wrap `vector`.
      */
    private def wholePeriod(wrap: Wrap, m: Affine): Boolean =
      val area = Element.mapBox(m, wrap.period)
      m.keepsAxes && DeviceGrid.isWhole(area.width) &&
      DeviceGrid.isWhole(area.height) &&
      DeviceGrid.isWhole(area.upperLeftCorner.x) && DeviceGrid.isWhole(area.upperLeftCorner.y)

    /** How `wrap` is drawn under `m`. It is moved copies of its content where the period is on
      * whole device pixels and the plan is not for SVG, and tiles everywhere else.
      */
    private def wrapLayout(wrap: Wrap, m: Affine): WrapLayout =
      val period          = wrap.period
      val (width, height) = (period.width, period.height)
      // Where m takes the local move by (x, y), in device pixels.
      def device(x: Double, y: Double): (Double, Double) =
        (m.a * x + m.c * y, m.b * x + m.d * y)
      val area = Element.mapBox(m, period)
      val clip = DeviceGrid.coveringDevice(area)
      // Copies of the content abut cleanly only where the period's edges are on whole device
      // pixels. Content that reaches an edge between pixels covers the pixel there partly in two
      // copies, and two half-covered pixels drawn over each other leave a seam. Elsewhere, a tile
      // puts the period on whole pixels of its own. A plan for SVG takes a tile there too, since
      // the SVG backend repeats a tile as one pattern fill and antialiases no copy's edge.
      if !svg && wholePeriod(wrap, m) then
        // The copy moved by the offset, and the ones a period before it where it is not 0.
        val columns  = if wrap.dx == 0 then Vector(0) else Vector(0, 1)
        val rows     = if wrap.dy == 0 then Vector(0) else Vector(0, 1)
        val (sx, sy) = device(wrap.dx, wrap.dy)
        val (wx, wy) = device(width, 0)
        val (hx, hy) = device(0, height)
        val (x, y)   = (DeviceGrid.wholeDevice(sx), DeviceGrid.wholeDevice(sy))
        WrapLayout.Copies(
          for
            column <- columns
            row    <- rows
          yield (
            x - column * DeviceGrid.wholeDevice(wx) - row * DeviceGrid.wholeDevice(hx),
            y - column * DeviceGrid.wholeDevice(wy) - row * DeviceGrid.wholeDevice(hy)
          )
        )
      else if m.keepsAxes then
        // The tile is the period rounded to whole device pixels, on the device's own axes.
        val (tileWidth, tileHeight) = (
          DeviceGrid.wholeDevice(area.width).max(1),
          DeviceGrid.wholeDevice(area.height).max(1)
        )
        val toTile   = Affine.between(area, Bounds(0, 0, tileWidth, tileHeight))
        val (ox, oy) = device(wrap.dx, wrap.dy)
        val xs       = steps(
          DeviceGrid.wholeDevice(area.upperLeftCorner.x),
          DeviceGrid.wholeDevice(ox * tileWidth / area.width),
          tileWidth,
          clip.left,
          clip.left + clip.width
        )
        val ys = steps(
          DeviceGrid.wholeDevice(area.upperLeftCorner.y),
          DeviceGrid.wholeDevice(oy * tileHeight / area.height),
          tileHeight,
          clip.top,
          clip.top + clip.height
        )
        WrapLayout.Tiled(
          m.andThen(toTile),
          tileWidth,
          tileHeight,
          for x <- xs; y <- ys yield Affine.translation(x, y)
        )
      else
        // The tile is the period in its own frame, its sides rounded to whole device pixels,
        // and each copy is turned onto the device with the wrap.
        val (tileWidth, tileHeight) = (
          DeviceGrid.wholeDevice(math.hypot(m.a, m.b) * width).max(1),
          DeviceGrid.wholeDevice(math.hypot(m.c, m.d) * height).max(1)
        )
        val tileBox  = Bounds(0, 0, tileWidth, tileHeight)
        val toTile   = Affine.between(period, tileBox)
        val fromTile = Affine.between(tileBox, period).andThen(m)
        val xs       = steps(
          0,
          DeviceGrid.wholeDevice(wrap.dx * tileWidth / width),
          tileWidth,
          0,
          tileWidth
        )
        val ys = steps(
          0,
          DeviceGrid.wholeDevice(wrap.dy * tileHeight / height),
          tileHeight,
          0,
          tileHeight
        )
        WrapLayout.Tiled(
          toTile,
          tileWidth,
          tileHeight,
          for x <- xs; y <- ys yield Affine.translation(x, y).andThen(fromTile)
        )
    end wrapLayout

    /** The starts of the copies of a tile `size` pixels long along an axis, at `origin` moved by
      * `offset` and every whole multiple of `size` from there, that reach into the pixels from
      * `low` to `high`.
      */
    private def steps(origin: Int, offset: Int, size: Int, low: Int, high: Int): Vector[Int] =
      // The start at or before the pixel low and less than a whole tile before it.
      val first = low - Math.floorMod(low - origin - offset, size)
      Vector.iterate(first, (high - first + size - 1) / size)(_ + size)

    /** `content` seen only inside the local rectangle `rect` under `m`. That is a hard clip of the
      * device pixels the rectangle covers where `m` keeps the axes, and otherwise a layer with the
      * rectangle as its antialiased clip.
      */
    private def clipped(m: Affine, rect: Bounds, content: Vector[DrawOp]): Vector[DrawOp] =
      val box = DeviceGrid.coveringDevice(Element.mapBox(m, rect))
      if m.keepsAxes then Vector(DrawOp.HardClip(box, content))
      else Vector(DrawOp.Layer(box, Some(ClipShape(m, rect)), Vector(), content))
  end Planner
end DrawPlan
