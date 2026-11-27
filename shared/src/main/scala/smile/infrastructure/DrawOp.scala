package smile.infrastructure

import scala.annotation.tailrec
import scala.collection.mutable

import smile.colors.{Color, Paint}
import smile.modeling.{Affine, Bounds, Pos}
import smile.modeling.{LocalShape, PathShape, TextLayout}
import smile.pictures.SvgDocument
import smile.pictures.{PixelSource, FillStyle, ResamplingHint, StrokeStyle}

/** A clip that is not axis-aligned in device pixels. It is the rectangle `rect` of a local frame
  * that `m` takes to device pixels.
  */
private[smile] final case class ClipShape(m: Affine, rect: Bounds) derives CanEqual:

  /** The clip's rectangle as an opaque path, which, drawn as a mask, keeps only what it covers,
    * antialiased at its edges.
    */
  def asMask: Vector[DrawOp] =
    val (left, top, right, bottom) = (
      rect.upperLeftCorner.x,
      rect.upperLeftCorner.y,
      rect.lowerRightCorner.x,
      rect.lowerRightCorner.y
    )
    Vector(
      DrawOp.Path(
        m,
        PathShape(IArray(left, top, right, top, right, bottom, left, bottom), true),
        Some(FillStyle(Color(0, 0, 0))),
        None
      )
    )

/** One drawing operation of the shared draw plan, in device pixels, which every backend interprets
  * the same way (see `smile.infrastructure.DrawPlan`).
  *
  * Every affine takes a local frame to device pixels, so a backend draws under it with its own
  * `setTransform` and rounds nothing. The plan has already made every decision about whole device
  * pixels. Nested operations are drawn in order, the first one at the back.
  */
private[smile] enum DrawOp derives CanEqual:

  /** The local `shape` filled with `fill` and outlined with `stroke`, both in the local frame,
    * under `m`. The stroke scales with the shape, unevenly under an uneven scaling.
    */
  case Path(m: Affine, shape: LocalShape, fill: Option[FillStyle], stroke: Option[StrokeStyle])

  /** The glyphs of `layout`, laid out around its local origin, under `m`, which mirrors them when
    * it mirrors. The stroke is already the one for glyphs, with round joins.
    */
  case Text(m: Affine, layout: TextLayout, fill: Option[FillStyle], stroke: Option[StrokeStyle])

  /** The pixels of `source` under `m`, inside the convex polygon `clip` in device pixels when there
    * is one.
    *
    * Where `m` keeps the axes, the clip is a rectangle on whole device pixels. It holds those whose
    * centers lie inside both the tile's clip and the image's extent (see
    * `smile.infrastructure.DeviceGrid.centersCovered`). Every pixel drawn lies within it, so a
    * backend draws nothing outside the clip and need not clip to the extent again. That clip is
    * left out where there is none and the extent is on whole device pixels. An axis-aligned
    * `smile.pictures.Clip` covers every device pixel it touches instead (see
    * `smile.infrastructure.DrawOp.HardClip`). Where `m` turns the axes, the clip is the tile's own,
    * and the image's pixels end at its extent.
    *
    * When `replayed` is false, `m` takes the pixels of the source's whole region, as one tile of
    * the source's own plan lays them. That is the buffer of a pixel source, or a picture source
    * rendered at one pixel per logical pixel, which a backend renders at the density `m` needs
    * instead. When it is true, for a `Stepwise` output, `m` takes the pixels of
    * `source.buffer(settings)`, the source's steps replayed.
    *
    * `snapped` tells that `m` copies the pixels one to one onto whole device pixels, so they are
    * copied as they are, with no resampling. Every other image is resampled as `interpolation` and
    * the settings say (see `smile.infrastructure.DrawPlan.interpolation`).
    */
  case Image(
      source: PixelSource,
      m: Affine,
      clip: Option[Vector[Pos]],
      snapped: Boolean,
      interpolation: ResamplingHint,
      replayed: Boolean
  )

  /** A static SVG document under `m`, which takes the document's own pixels to device pixels. */
  case Foreign(document: SvgDocument, m: Affine)

  /** The one device pixel at column `x` and row `y`, painted over with `paint`. A gradient paints
    * its average color (see [[smile.colors.LinearGradient.averageColor]]), since one pixel has no
    * room for it to run.
    */
  case Pixel(x: Int, y: Int, paint: Paint)

  /** `ops` drawn into a layer of the device pixels `box` and composited back 1:1.
    *
    * Within the layer, `clip` keeps only its rectangle, antialiased, and then each of `masks` in
    * order, its operations drawn into a layer of their own, keeps only what is left where they are
    * opaque, or for a mask whose flag says it is inverted, where they are not.
    */
  case Layer(
      box: DeviceBox,
      clip: Option[ClipShape],
      masks: Vector[(Vector[DrawOp], Boolean)],
      ops: Vector[DrawOp]
  )

  /** `ops` drawn with nothing outside the device pixels `box`, a hard clip with no antialiasing. */
  case HardClip(box: DeviceBox, ops: Vector[DrawOp])

  /** `tile` rendered once into a buffer of its own and that buffer drawn once for each of `copies`,
    * which take the buffer's pixels to device pixels.
    *
    * The tile's box starts at the origin and its operations are in the buffer's own pixels. Its
    * content is mapped into exactly that box, a whole number of pixels on each side, which is the
    * period it repeats with rounded to whole device pixels. The copies step by exactly that many
    * pixels, from an offset snapped to whole pixels, so they abut on whole pixels and never
    * overlap, and together they cover the clip they are drawn under. Where the wrap keeps the axes,
    * the buffer's pixels are device pixels and every copy is a move by whole device pixels.
    *
    * The SVG backend draws the copies as one pattern fill of the tile. `vector` marks the tile of a
    * wrap whose period is on whole device pixels, in a plan made for SVG output (see
    * `smile.infrastructure.DrawPlan.of`). The SVG backend keeps the content of such a tile as
    * vectors, where it otherwise embeds a tile's pixels. Raster painters ignore the mark and render
    * the tile into a buffer as they do any other, also when they render it as part of a tile that
    * the SVG backend embeds as pixels.
    */
  case Tiles(tile: Layer, copies: Vector[Affine], vector: Boolean = false)

  /** The pixels of the plan's slot `slot` (see `smile.infrastructure.DrawPlan.Plan`), already drawn
    * into a buffer of the device pixels `region`, composited back 1:1 at `region`, as a layer's
    * buffer is.
    */
  case Prepared(slot: Int, region: DeviceBox)

  /** This operation moved by whole device pixels, `dx` to the right and `dy` down, where only the
    * device pixels `shown` can show. The result leaves out the layers and hard clips inside it
    * whose boxes, moved, meet none of the pixels that the boxes around them leave of `shown`, which
    * draw nothing there. `None` when the operation itself is such a layer or hard clip. Not moved,
    * an operation whose own box lies within `shown` is kept as it is, since what it holds outside
    * `shown` draws nothing there either. An operation that draws a slot,
    * `smile.infrastructure.DrawOp.Prepared`, cannot be moved, since the operations of its slot
    * would have to move with it. A plan is moved before it is bounded.
    *
    * The layers and hard clips inside it are moved bottom-up with a stack of their own, so that an
    * operation of any depth moves without recursion.
    */
  def movedBy(dx: Int, dy: Int, shown: DeviceBox): Option[DrawOp] =
    // Whether this operation's own box, if it has one, lies within the box shown.
    val boxShown = this match
      case layer: Layer   => DrawPlan.intersection(layer.box, shown) == layer.box
      case HardClip(b, _) => DrawPlan.intersection(b, shown) == b
      case _              => true
    if dx == 0 && dy == 0 && boxShown then Some(this)
    else
      val move                         = Affine.translation(dx, dy)
      def box(b: DeviceBox): DeviceBox = b.copy(left = b.left + dx, top = b.top + dy)

      // The operations that move along with the given operation. For a layer they are its masks'
      // operations, in order, and then its own.
      def inside(op: DrawOp): Vector[DrawOp] = op match
        case layer: Layer     => DrawOp.parts(layer).flatten
        case HardClip(_, ops) => ops
        case _                => Vector()

      // The pixels that the operations inside the given operation can show on once it is moved.
      // The operation itself can show on the given box, which a layer or a hard clip narrows to
      // its own moved box.
      def within(op: DrawOp, around: DeviceBox): DeviceBox = op match
        case layer: Layer   => DrawPlan.intersection(box(layer.box), around)
        case HardClip(b, _) => DrawPlan.intersection(box(b), around)
        case _              => around

      // The given operation after the move. The second argument is the operations inside it,
      // already moved.
      def moved(op: DrawOp, inner: Vector[DrawOp]): DrawOp = op match
        case Path(m, shape, fill, stroke)  => Path(m.andThen(move), shape, fill, stroke)
        case Text(m, layout, fill, stroke) => Text(m.andThen(move), layout, fill, stroke)
        case image: Image                  =>
          image.copy(
            m = image.m.andThen(move),
            clip = image.clip.map(_.map(p => Pos(p.x + dx, p.y + dy)))
          )
        case Foreign(document, m) => Foreign(document, m.andThen(move))
        case Pixel(x, y, paint)   => Pixel(x + dx, y + dy, paint)
        case layer: Layer         =>
          val starts = DrawOp.parts(layer).scanLeft(0)(_ + _.size)
          val parts  = starts.zip(starts.tail).map((from, until) => inner.slice(from, until))
          DrawOp
            .withParts(layer, parts)
            .copy(box = box(layer.box), clip = layer.clip.map(c => c.copy(m = c.m.andThen(move))))
        case HardClip(b, _)              => HardClip(box(b), inner)
        case Tiles(tile, copies, vector) => Tiles(tile, copies.map(_.andThen(move)), vector)
        case Prepared(_, _)              =>
          // Its slot's own operations would have to move with it.
          throw new IllegalStateException("A slot of a bounded plan cannot be moved")

      // An operation whose inner operations are being moved, with those moved so far. The inner
      // operations can show only on the box shown.
      final class Moving(val op: DrawOp, val shown: DeviceBox):
        val inner: Vector[DrawOp]                            = inside(op)
        val results: mutable.Builder[DrawOp, Vector[DrawOp]] = Vector.newBuilder
        var next                                             = 0

      val stack                   = mutable.Stack(new Moving(this, within(this, shown)))
      @tailrec def walk(): DrawOp =
        val top = stack.top
        if top.next < top.inner.size then
          val op = top.inner(top.next)
          top.next += 1
          op match
            case _: Layer | _: HardClip =>
              val region = within(op, top.shown)
              if DrawPlan.hasPixels(region) then stack.push(new Moving(op, region))
            case _ => top.results += moved(op, Vector())
          walk()
        else
          val frame  = stack.pop()
          val result = moved(frame.op, frame.results.result())
          if stack.isEmpty then result
          else
            stack.top.results += result
            walk()
      this match
        case _: Layer | _: HardClip if !DrawPlan.hasPixels(stack.top.shown) => None
        case _                                                              => Some(walk())
end DrawOp

private[smile] object DrawOp:

  /** The operations a layer draws. Its masks' come first, in order, and then its own. */
  def parts(layer: DrawOp.Layer): Vector[Vector[DrawOp]] = layer.masks.map(_._1) :+ layer.ops

  /** `layer` with the operations of `parts` (see `smile.infrastructure.DrawOp.parts`) in place of
    * its own.
    */
  def withParts(layer: DrawOp.Layer, parts: Vector[Vector[DrawOp]]): DrawOp.Layer =
    val masks = layer.masks.zip(parts).map { case ((_, inverted), ops) => (ops, inverted) }
    layer.copy(masks = masks, ops = parts.last)
end DrawOp
