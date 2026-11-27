package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import smile.RenderSettings
import smile.colors.{Color, LinearGradient, Paint}
import smile.infrastructure.{JSBufferAdapter, JSPaint, JSTextMetrics}
import smile.modeling.{Affine, Bounds, Pos, RasterInterpolation, RasterPlan}
import smile.modeling.{EllipseArcShape, LocalShape, PathShape, PointShape, TextLayout, TextShape}
import smile.pictures.Element
import smile.pictures.{PixelSource, FillStyle, StrokeStyle}

import scala.collection.mutable
import scala.scalajs.js

/** Renders the shared draw plan (see `smile.infrastructure.DrawPlan`) onto a 2D canvas, as
  * `smile.infrastructure.JVMPlanRenderer` renders it with Java2D.
  *
  * Each operation is drawn under its own affine, set with `setTransform` after the move that puts
  * the corner of the canvas it is drawn into at the origin, so strokes, gradients and glyphs are
  * drawn in their local frame and stretch, turn and mirror with it. Nothing is rounded here. The
  * plan has made every decision about whole device pixels. Every operation saves the context's
  * state and restores it when it is done, so that no clip, transform or style outlives it.
  *
  * Layers (masks, turned clips and wrap tiles) are drawn into canvases of their own device pixels,
  * taken from a pool, and composited back 1:1. A mask is drawn into a second layer and laid over
  * the content with `destination-in`, or `destination-out` when it is inverted, and a turned clip
  * is such a mask of its antialiased rectangle.
  */
private[smile] object CanvasPlanRenderer:

  /** `element` at `scale` device pixels per logical pixel, drawn with `settings` at the instant
    * `timeMillis`. The result is a bitmap whose pixels are the device pixels its bounds cover and
    * whose bounds are those pixels in logical units. The empty picture gives an empty bitmap.
    *
    * The canvas it is drawn into is the library's own kind (see
    * `smile.infrastructure.JSBufferAdapter.contextOf`).
    */
  def render(
      element: Element,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long = System.currentTimeMillis(),
      nesting: Int = DrawPlan.MaxNesting
  ): PixelSource =
    render(
      DrawPlan.planned(element, DeviceGrid(scale), settings, timeMillis, nesting),
      scale,
      settings,
      timeMillis
    )

  /** `plan`, a plan on the grid of `scale` device pixels per logical pixel made at the instant
    * `timeMillis`, drawn with `settings`, as the `Element` overload draws the element it is the
    * plan of.
    */
  def render(
      plan: DrawPlan.Plan,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): PixelSource =
    val box = plan.box
    if box.width <= 0 || box.height <= 0 then PixelSource.Empty
    else
      val buffer = JSBufferAdapter.drawn(box.width, box.height):
        drawInto(_, plan, box, box, settings, timeMillis)
      val logical = Bounds(
        box.left / scale,
        box.top / scale,
        (box.left + box.width) / scale,
        (box.top + box.height) / scale
      )
      PixelSource.fromBuffer(buffer, logical)

  /** `element` at `scale` device pixels per logical pixel, drawn with `settings` at the instant
    * `timeMillis` onto the device pixels of the canvas of `target` as its bitmap drawn there would
    * be. The corner of the plan's device box lands at the translation of the context's transform
    * rounded to a whole pixel, and only the inside of the context's clip is drawn. The rest of the
    * context's transform is not used, and its state is left as it was. The empty picture draws
    * nothing.
    */
  def renderInto(
      target: dom.CanvasRenderingContext2D,
      element: Element,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): Unit =
    val plan      = DrawPlan.planned(element, DeviceGrid(scale), settings, timeMillis)
    val transform = target.asInstanceOf[js.Dynamic].getTransform()
    val originX   = Math.rint(transform.e.asInstanceOf[Double]).toInt - plan.box.left
    val originY   = Math.rint(transform.f.asInstanceOf[Double]).toInt - plan.box.top
    val canvas    = DeviceBox(-originX, -originY, target.canvas.width, target.canvas.height)
    val visible   = DrawPlan.intersection(plan.box, canvas)
    if visible.width > 0 && visible.height > 0 then
      target.save()
      try
        // What the context was drawing with gives way to what a fresh canvas draws with, so that
        // the pixels are those that the method render gives. The painter sets the rest of the
        // context's state itself. That is the transform, paints, line geometry, smoothing, font,
        // alignment, baseline and text rendering. The direction and language are set to those of a
        // canvas in no document, where a canvas in a page inherits the page's. Engines that lack a
        // property ignore it.
        target.globalAlpha = 1
        target.globalCompositeOperation = "source-over"
        target.setLineDash(js.Array())
        target.lineDashOffset = 0
        target.shadowColor = "rgba(0, 0, 0, 0)"
        target.shadowBlur = 0
        target.shadowOffsetX = 0
        target.shadowOffsetY = 0
        val state = target.asInstanceOf[js.Dynamic]
        state.filter = "none"
        state.imageSmoothingQuality = "low"
        state.direction = "ltr"
        state.lang = ""
        state.letterSpacing = "0px"
        state.wordSpacing = "0px"
        state.fontKerning = "auto"
        state.fontStretch = "normal"
        state.fontVariantCaps = "normal"
        drawInto(target, plan, canvas, visible, settings, timeMillis)
      finally target.restore()
  end renderInto

  /** Draws `plan` with `settings` at the instant `timeMillis` onto `ctx`, a canvas of the plan's
    * device pixels `target`, whose pixel `(0, 0)` is the corner of `target`, where only the plan's
    * device pixels `visible` can show.
    */
  private def drawInto(
      ctx: dom.CanvasRenderingContext2D,
      plan: DrawPlan.Plan,
      target: DeviceBox,
      visible: DeviceBox,
      settings: RenderSettings,
      timeMillis: Long
  ): Unit =
    new Painter(settings, LayerPool.shared, timeMillis)
      .drawAll(ctx, plan.slots, plan.ops, target, visible)

  /** `ops`, with the `slots` they draw ahead (see `smile.infrastructure.DrawPlan.Plan`), drawn with
    * `settings` at the instant `timeMillis` into a buffer of the device pixels `box`, whose pixel
    * `(0, 0)` is the corner of the box. This is the raster of a wrap tile that SVG output embeds
    * (see `smile.infrastructure.SvgPlanRenderer`).
    */
  private[infrastructure] def paint(
      slots: Vector[DrawPlan.Slot],
      ops: Vector[DrawOp],
      box: DeviceBox,
      settings: RenderSettings,
      timeMillis: Long
  ): JSBufferAdapter =
    val drawn = DrawPlan.drawnSlots(slots, ops)
    JSBufferAdapter.drawn(box.width.max(1), box.height.max(1)):
      new Painter(settings, LayerPool.shared, timeMillis).drawAll(_, slots, ops, box, box, drawn(_))

  /** A canvas to draw a layer into, with its context. */
  private[infrastructure] final class Surface(val canvas: html.Canvas):
    val ctx: dom.CanvasRenderingContext2D = JSBufferAdapter.contextOf(canvas)
    def width: Int                        = canvas.width
    def height: Int                       = canvas.height

  /** Draws operations with `settings` at the instant `timeMillis`, taking the canvases of layers
    * from `pool`.
    */
  private final class Painter(settings: RenderSettings, pool: LayerPool, timeMillis: Long):

    /** The pooled surfaces of the slots drawn ahead that no operation has composited yet, by the
      * number of the slot.
      */
    private val prepared = mutable.Map[Int, Surface]()

    /** Draws the `slots` of a plan (see `smile.infrastructure.DrawPlan.Plan`) that `drawn` is true
      * for by their number, all of them unless it says otherwise, ahead, in order, and then `ops`
      * onto `ctx`, a canvas of the device pixels `target`, whose pixel `(0, 0)` is the corner of
      * `target`, where only the device pixels `visible` can show. A plan's operations draw all of
      * its slots, and a wrap tile that SVG output embeds draws only its own of them.
      */
    def drawAll(
        ctx: dom.CanvasRenderingContext2D,
        slots: Vector[DrawPlan.Slot],
        ops: Vector[DrawOp],
        target: DeviceBox,
        visible: DeviceBox,
        drawn: Int => Boolean = _ => true
    ): Unit =
      try
        for (slot, i) <- slots.zipWithIndex if drawn(i) do
          val region  = slot.region
          val surface = pool.acquire(region.width + 2, region.height + 2)
          prepared(i) = surface
          paintLayer(surface, slot.layer, region)
        drawOps(ctx, ops, target, visible)
      finally
        prepared.values.foreach(pool.release)
        prepared.clear()

    /** Draws `ops` onto `ctx`, a canvas whose pixel `(0, 0)` is the corner of `target`, where only
      * the device pixels `visible` can show.
      */
    private def drawOps(
        ctx: dom.CanvasRenderingContext2D,
        ops: Vector[DrawOp],
        target: DeviceBox,
        visible: DeviceBox
    ): Unit =
      ops.foreach(draw(ctx, _, target, visible))

    /** Draws `op` onto `ctx`, a canvas whose pixel `(0, 0)` is the corner of `target`, where only
      * the device pixels `visible` can show.
      */
    private def draw(
        ctx: dom.CanvasRenderingContext2D,
        op: DrawOp,
        target: DeviceBox,
        visible: DeviceBox
    ): Unit =
      ctx.save()
      try
        op match
          case DrawOp.Path(m, shape, fill, stroke) =>
            setUnder(ctx, target, m)
            shape match
              case text: TextShape => paintText(ctx, text.layout, fill, stroke)
              case _               =>
                val path = trace(ctx, shape)
                JSPaint.canvasPaint(
                  ctx,
                  DrawPlan.paintOrigin(shape.localBox, stroke),
                  fill,
                  stroke,
                  path
                )
          case DrawOp.Text(m, layout, fill, stroke) =>
            setUnder(ctx, target, m)
            paintText(ctx, layout, fill, stroke)
          case image: DrawOp.Image =>
            DrawPlan
              .imagePixels(image, visible, settings, timeMillis)
              .foreach((pixels, m) => drawPixels(ctx, image, target, pixels, m))
          case DrawOp.Foreign(document, m) =>
            setUnder(ctx, target, Affine.Identity)
            document.drawTo(ctx, m)
          case DrawOp.Pixel(x, y, paint) =>
            setUnder(ctx, target, Affine.Identity)
            ctx.fillStyle = (paint match
              case color: Color             => color
              case gradient: LinearGradient => gradient.averageColor
            ).toString
            ctx.fillRect(x, y, 1, 1)
          case layer: DrawOp.Layer =>
            // Only the pixels of the layer that can show are drawn, so a large layer that a clip
            // mostly hides takes a small canvas.
            val region = DrawPlan.intersection(layer.box, visible)
            withLayer(layer, region): surface =>
              copyBack(ctx, target, surface, region)
          case DrawOp.HardClip(box, ops) =>
            setUnder(ctx, target, Affine.Identity)
            ctx.beginPath()
            ctx.rect(box.left, box.top, box.width, box.height)
            ctx.clip()
            drawOps(ctx, ops, target, DrawPlan.intersection(visible, box))
          case DrawOp.Tiles(tile, copies, _) =>
            withLayer(tile, tile.box): surface =>
              val (width, height) = (tile.box.width, tile.box.height)
              for copy <- copies do
                // The tile's pixels are copied as they are where a copy only moves them, by whole
                // pixels, and resampled where it turns or scales them.
                ctx.imageSmoothingEnabled = !onlyMoves(copy)
                setUnder(ctx, target, copy)
                ctx.drawImage(surface.canvas, 1, 1, width, height, 0, 0, width, height)
          case DrawOp.Prepared(slot, region) =>
            // The slot's surface is composited as a layer's is, and only once.
            val surface = prepared(slot)
            prepared -= slot
            try copyBack(ctx, target, surface, region)
            finally pool.release(surface)
        end match
      finally ctx.restore()
    end draw

    /** Runs `work` on a surface that holds the pixels of `layer` within the device pixels `box`
      * from its pixel `(1, 1)` on, drawn into a pooled canvas with its clip and its masks applied.
      * A box of no pixels draws nothing.
      */
    private def withLayer(layer: DrawOp.Layer, box: DeviceBox)(work: Surface => Unit): Unit =
      if box.width > 0 && box.height > 0 then
        val content = pool.acquire(box.width + 2, box.height + 2)
        try
          paintLayer(content, layer, box)
          work(content)
        finally pool.release(content)

    /** Draws the pixels of `layer` within the device pixels `box` into `surface`, from its pixel
      * `(1, 1)` on, with its clip and its masks applied.
      */
    private def paintLayer(surface: Surface, layer: DrawOp.Layer, box: DeviceBox): Unit =
      paintInto(surface, box, layer.ops)
      val masks = layer.clip.map(clip => (clip.asMask, false)) ++ layer.masks
      for (ops, inverted) <- masks do
        val mask = pool.acquire(box.width + 2, box.height + 2)
        try
          paintInto(mask, box, ops)
          val ctx = surface.ctx
          ctx.save()
          try
            ctx.setTransform(1, 0, 0, 1, 0, 0)
            ctx.globalCompositeOperation = if inverted then "destination-out" else "destination-in"
            ctx.imageSmoothingEnabled = false
            val (width, height) = (box.width + 2, box.height + 2)
            ctx.drawImage(mask.canvas, 0, 0, width, height, 0, 0, width, height)
          finally ctx.restore()
        finally pool.release(mask)

    /** Draws `ops` into `surface`, whose pixel `(1, 1)` is the device pixel at the corner of `box`,
      * with nothing drawn outside the box. The surface is cleared first, the box and a border of
      * one pixel around it, which stays clear, so that a copy of the layer that is resampled reads
      * nothing but transparency past each of the box's edges.
      */
    private def paintInto(surface: Surface, box: DeviceBox, ops: Vector[DrawOp]): Unit =
      val ctx = surface.ctx
      ctx.save()
      try
        ctx.setTransform(1, 0, 0, 1, 0, 0)
        ctx.clearRect(0, 0, box.width + 2, box.height + 2)
        ctx.beginPath()
        ctx.rect(1, 1, box.width, box.height)
        ctx.clip()
        val frame = DeviceBox(box.left - 1, box.top - 1, box.width + 2, box.height + 2)
        drawOps(ctx, ops, frame, box)
      finally ctx.restore()

    /** Draws the pixels of the layer `box` from `surface`, where they start at its pixel `(1, 1)`,
      * onto `ctx`, whose pixel `(0, 0)` is the corner of `target`, 1:1.
      */
    private def copyBack(
        ctx: dom.CanvasRenderingContext2D,
        target: DeviceBox,
        surface: Surface,
        box: DeviceBox
    ): Unit =
      setUnder(ctx, target, Affine.Identity)
      ctx.imageSmoothingEnabled = false
      ctx.drawImage(
        surface.canvas,
        1,
        1,
        box.width,
        box.height,
        box.left,
        box.top,
        box.width,
        box.height
      )

    /** Draws `pixels` under `m` onto `ctx`, whose pixel `(0, 0)` is the corner of `target`, as
      * `image` says, inside its clip and resampled as it asks.
      *
      * Snapped pixels are copied with no smoothing, so a crop of them shows exactly its own pixels.
      * Resampled pixels are never drawn as a smoothed sub-rectangle, which a canvas samples outside
      * of. They are drawn with a border that repeats their edge pixels (see
      * `smile.infrastructure.JSBufferAdapter.paddedCanvas`). That is the whole source, or where the
      * clip crops it, the source pixels the clip reaches (see
      * `smile.infrastructure.DrawPlan.sourceWindow`) extracted into a layer of their own, so that
      * nothing outside the crop is sampled into it. Without the border a canvas blends the edge
      * pixels with the transparency past them, and the bitmap's edges come out faded where Java2D
      * and the SVG output draw them whole.
      */
    private def drawPixels(
        ctx: dom.CanvasRenderingContext2D,
        image: DrawOp.Image,
        target: DeviceBox,
        source: NativeBuffer,
        m: Affine
    ): Unit =
      if image.snapped then
        image.clip.foreach(corners => clipTo(ctx, target, corners))
        // The plan put the corner on a whole device pixel. The float error of composing the
        // affines is taken off, so that the pixels are copied as they are.
        ctx.setTransform(
          m.a,
          m.b,
          m.c,
          m.d,
          Math.rint(m.e - target.left),
          Math.rint(m.f - target.top)
        )
        ctx.imageSmoothingEnabled = false
        ctx.drawImage(source.readOnlyCanvas, 0, 0)
      else
        image.clip.flatMap(DrawPlan.sourceWindow(_, m, source.width, source.height)) match
          case None =>
            resample(ctx, image, target, source.paddedCanvas, source.width, source.height, m)
          case Some(window) =>
            val (width, height) = (window.width, window.height)
            val tile            = pool.acquire(width + 2, height + 2)
            try
              val g = tile.ctx
              g.save()
              try
                g.setTransform(1, 0, 0, 1, 0, 0)
                g.imageSmoothingEnabled = false
                // All of it, so that whatever it held past the window is transparent.
                g.clearRect(0, 0, tile.width, tile.height)
                JSBufferAdapter.drawPadded(
                  g,
                  source.readOnlyCanvas,
                  window.left,
                  window.top,
                  width,
                  height
                )
              finally g.restore()
              val toWindow = Affine.translation(window.left, window.top).andThen(m)
              resample(ctx, image, target, tile.canvas, width, height, toWindow)
            finally pool.release(tile)
    end drawPixels

    /** Draws the `width` by `height` pixels that start at pixel `(1, 1)` of `padded`, which has a
      * border of their edge pixels around them, under `m` onto `ctx`, whose pixel `(0, 0)` is the
      * corner of `target`, resampled as `image` asks and inside its clip, and where `m` turns the
      * axes, also inside their own extent, intersected with the clip into one polygon so that an
      * edge the two share is not antialiased twice. Where `m` keeps the axes, the plan's clip
      * already lies within the extent (see `smile.infrastructure.DrawOp.Image`).
      */
    private def resample(
        ctx: dom.CanvasRenderingContext2D,
        image: DrawOp.Image,
        target: DeviceBox,
        padded: html.Canvas,
        width: Int,
        height: Int,
        m: Affine
    ): Unit =
      val extent =
        Vector((0, 0), (width, 0), (width, height), (0, height))
          .map((x, y) => m(Pos(x.toDouble, y.toDouble)))
      val corners = image.clip.fold(extent): clip =>
        if m.keepsAxes then clip else RasterPlan.intersectConvex(clip, extent)
      if corners.length >= 3 then
        clipTo(ctx, target, corners)
        DrawPlan.interpolation(image, settings) match
          case RasterInterpolation.Nearest  => ctx.imageSmoothingEnabled = false
          case RasterInterpolation.Bilinear => ctx.imageSmoothingEnabled = true
          case RasterInterpolation.Bicubic  =>
            // A canvas has no bicubic filter. Its high quality smoothing is the closest it has.
            ctx.imageSmoothingEnabled = true
            ctx.asInstanceOf[js.Dynamic].imageSmoothingQuality = "high"
        setUnder(ctx, target, m)
        // Drawn whole. What the padded canvas holds past the border is outside the clip.
        ctx.drawImage(padded, -1, -1)
    end resample

    /** Fills and strokes the glyphs of `layout`, laid out around the origin of the local frame
      * `ctx` draws in, the stroke behind the fill unless it is on top, with gradients measured from
      * `smile.infrastructure.DrawPlan.paintOrigin`.
      */
    private def paintText(
        ctx: dom.CanvasRenderingContext2D,
        layout: TextLayout,
        fill: Option[FillStyle],
        stroke: Option[StrokeStyle]
    ): Unit =
      val origin = DrawPlan.paintOrigin(layout.layoutBox, stroke)
      val drop   = layout.baselineDrop
      ctx.font = JSTextMetrics.cssFont(layout.font, layout.size, layout.weight)
      ctx.textAlign = "center"
      // Where the font's metrics cannot be had, the canvas centers the line itself.
      ctx.textBaseline = if drop.isDefined then "alphabetic" else "middle"
      // Glyphs drawn under a scaling are laid out as SVG lays them out, which keeps their advances
      // from jittering. Engines that lack the property ignore it.
      ctx.asInstanceOf[js.Dynamic].textRendering = "geometricPrecision"
      val baseline = drop.getOrElse(0.0)
      JSPaint.inPaintOrder(stroke)(
        fill.foreach: style =>
          ctx.fillStyle = paintStyle(ctx, style.paint, origin)
          ctx.fillText(layout.content, 0, baseline)
        ,
        stroke.foreach: style =>
          ctx.strokeStyle = paintStyle(ctx, style.paint, origin)
          JSPaint.applyCanvasStrokeGeometry(ctx, style)
          ctx.strokeText(layout.content, 0, baseline)
      )
    end paintText

    /** `paint` as a canvas paint in the local frame `ctx` draws in, a gradient measured from
      * `origin`.
      */
    private def paintStyle(
        ctx: dom.CanvasRenderingContext2D,
        paint: Paint,
        origin: Pos
    ): js.Any =
      paint match
        case color: Color             => color.toString
        case gradient: LinearGradient => JSPaint.canvasGradientStyle(gradient, origin, ctx)
  end Painter

  /** Sets the transform of `ctx`, a canvas whose pixel `(0, 0)` is the corner of `target`, to `m`
    * followed by the move that puts that corner at the origin.
    */
  private def setUnder(ctx: dom.CanvasRenderingContext2D, target: DeviceBox, m: Affine): Unit =
    ctx.setTransform(m.a, m.b, m.c, m.d, m.e - target.left, m.f - target.top)

  private def onlyMoves(m: Affine): Boolean = m.a == 1 && m.b == 0 && m.c == 0 && m.d == 1

  /** Clips `ctx`, a canvas whose pixel `(0, 0)` is the corner of `target`, to the convex polygon
    * `corners` in device pixels.
    */
  private def clipTo(
      ctx: dom.CanvasRenderingContext2D,
      target: DeviceBox,
      corners: Vector[Pos]
  ): Unit =
    setUnder(ctx, target, Affine.Identity)
    ctx.beginPath()
    corners.headOption.foreach(first => ctx.moveTo(first.x, first.y))
    corners.drop(1).foreach(corner => ctx.lineTo(corner.x, corner.y))
    ctx.closePath()
    ctx.clip()

  /** Whether this environment has `Path2D` objects that take SVG path data, as browsers do and
    * jsdom does not.
    */
  private lazy val hasPath2D: Boolean = js.typeOf(js.Dynamic.global.Path2D) == "function"

  /** The `Path2D`s of partial arcs drawn lately, by their shape, so that an arc drawn every frame
    * is not parsed from its path data every frame. A canvas never changes a `Path2D` it fills or
    * strokes. At most `ArcPathsKept` are kept, and all are dropped when there would be more.
    */
  private val arcPaths = mutable.HashMap.empty[EllipseArcShape, js.Any]

  private val ArcPathsKept = 256

  /** The `Path2D` of the SVG backend's path data for `arc`, or `None` for a whole ellipse. */
  private def arcPath(arc: EllipseArcShape): Option[js.Any] =
    arcPaths
      .get(arc)
      .orElse:
        SvgPlanRenderer
          .arcData(arc)
          .map: data =>
            val path: js.Any = js.Dynamic.newInstance(js.Dynamic.global.Path2D)(data)
            if arcPaths.size >= ArcPathsKept then arcPaths.clear()
            arcPaths(arc) = path
            path

  /** Starts a path of a local shape in the local frame `ctx` draws in. A partial arc is instead
    * returned as a `Path2D` of the SVG backend's path data for it (see
    * `smile.infrastructure.SvgPlanRenderer.arcData`) where there are such objects. A browser
    * flattens the canvas's `ellipse` and SVG's arc command with different curves, and built from
    * the same data, the two backends draw the same one.
    */
  private def trace(ctx: dom.CanvasRenderingContext2D, shape: LocalShape): Option[js.Any] =
    ctx.beginPath()
    shape match
      case path: PathShape =>
        val xy = path.xy
        if xy.length >= 2 then
          ctx.moveTo(xy(0), xy(1))
          var i = 2
          while i < xy.length do
            ctx.lineTo(xy(i), xy(i + 1))
            i += 2
          if path.closed then ctx.closePath()
        None
      case arc: EllipseArcShape =>
        val partial = if hasPath2D then arcPath(arc) else None
        if partial.isDefined then partial
        else if arc.isFullCycle then
          ctx.ellipse(0, 0, arc.rx, arc.ry, 0, 0, 2 * Math.PI)
          // A whole ellipse has no ends, so its stroke gets no caps.
          ctx.closePath()
          None
        else
          // The shape's angles run counterclockwise on screen from three o'clock, measured on the
          // ellipse as if it were a circle stretched onto it. A canvas measures its angles the
          // same way, but clockwise on screen.
          val start = -arc.startAngle.toRadians
          val end   = -(arc.startAngle + arc.arcAngle).toRadians
          ctx.ellipse(0, 0, arc.rx, arc.ry, 0, start, end, arc.arcAngle > 0)
          None
      case _: PointShape =>
        ctx.rect(-0.5, -0.5, 1, 1)
        None
      case _: TextShape => None

  /** The canvases of layers. See [[smile.infrastructure.BufferPool]]. JavaScript runs on one
    * thread, so one pool serves every rendering. The contexts are the library's own kind (see
    * `smile.infrastructure.JSBufferAdapter.contextOf`), like the buffers they are drawn onto.
    */
  private[infrastructure] final class LayerPool(budget: Long) extends BufferPool[Surface](budget):
    protected def create(width: Int, height: Int): Surface =
      val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
      canvas.width = width
      canvas.height = height
      new Surface(canvas)
    protected def widthOf(surface: Surface): Int  = surface.width
    protected def heightOf(surface: Surface): Int = surface.height

  private[infrastructure] object LayerPool:
    val Budget: Long = BufferPool.Budget

    /** The pool every rendering takes its layers from. */
    val shared: LayerPool = new LayerPool(Budget)
end CanvasPlanRenderer
