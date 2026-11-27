package smile.infrastructure

import smile.RenderSettings
import smile.colors.{Color, LinearGradient}
import smile.infrastructure.{AwtConversions, GlyphCache, JVMBufferAdapter}
import smile.modeling.{Affine, Bounds, Pos, RasterInterpolation, RasterPlan}
import smile.modeling.{EllipseArcShape, LocalShape, PathShape, PointShape, TextShape}
import smile.pictures.Element
import smile.pictures.{PixelSource, FillStyle, StrokeStyle}

import java.awt.{AlphaComposite, Graphics2D, Rectangle, RenderingHints, Shape}
import java.awt.geom.{
  AffineTransform,
  Arc2D,
  Ellipse2D,
  NoninvertibleTransformException,
  Path2D,
  Point2D,
  Rectangle2D
}
import java.awt.image.BufferedImage

import scala.collection.mutable

/** Renders the shared draw plan (see `smile.infrastructure.DrawPlan`) with Java2D.
  *
  * Each operation is drawn under its own affine, composed after the move that puts the corner of
  * the buffer it is drawn into at the origin, so strokes, gradients and glyphs are drawn in their
  * local frame and stretch, turn and mirror with it. Nothing is rounded here. The plan has made
  * every decision about whole device pixels.
  *
  * Layers (masks, turned clips and wrap tiles) are drawn into buffers of their own device pixels,
  * taken from one pool shared by every thread, and composited back 1:1. A mask is drawn into a
  * second layer and laid over the content with `DstIn`, or `DstOut` when it is inverted, and a
  * turned clip is such a mask of its antialiased rectangle.
  */
private[smile] object JVMPlanRenderer:

  /** `element` at `scale` device pixels per logical pixel, drawn with `settings` at the instant
    * `timeMillis`. The result is a bitmap whose pixels are the device pixels its bounds cover and
    * whose bounds are those pixels in logical units. The empty picture gives an empty bitmap.
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
      val buffer = new JVMBufferAdapter(box.width, box.height)
      buffer.withGraphics2D(settings)(
        drawInto(_, plan, scale, -box.left, -box.top, box, settings, timeMillis)
      )
      val logical = Bounds(
        box.left / scale,
        box.top / scale,
        (box.left + box.width) / scale,
        (box.top + box.height) / scale
      )
      PixelSource.fromBuffer(buffer, logical)

  /** `element` at `scale` device pixels per logical pixel, drawn with `settings` at the instant
    * `timeMillis` onto the device pixels of `target` as its bitmap drawn there would be. The corner
    * of the plan's device box lands at the translation of the target's transform rounded to a whole
    * pixel, and only the inside of the target's clip is drawn. The rest of the target's transform
    * is not used, and its transform, clip, composite, paint and hints are left as they were. The
    * empty picture draws nothing.
    */
  def renderInto(
      target: Graphics2D,
      element: Element,
      scale: Double,
      settings: RenderSettings,
      timeMillis: Long
  ): Unit =
    val plan = DrawPlan.planned(element, DeviceGrid(scale), settings, timeMillis)
    if plan.box.width > 0 && plan.box.height > 0 then
      val g = target.create().asInstanceOf[Graphics2D]
      try
        val transform = g.getTransform
        val originX   = Math.rint(transform.getTranslateX).toInt - plan.box.left
        val originY   = Math.rint(transform.getTranslateY).toInt - plan.box.top
        // The painter sets whole transforms in device pixels, so the clip is read in them too.
        g.setTransform(new AffineTransform())
        val visible = Option(g.getClipBounds).fold(plan.box): clip =>
          DrawPlan.intersection(
            plan.box,
            DeviceBox(clip.x - originX, clip.y - originY, clip.width, clip.height)
          )
        if visible.width > 0 && visible.height > 0 then
          JVMBufferAdapter.configure(g, settings)
          drawInto(g, plan, scale, originX, originY, visible, settings, timeMillis)
      finally g.dispose()
    end if
  end renderInto

  /** Draws `plan`, a plan on the grid of `scale` device pixels per logical pixel, with `settings`
    * at the instant `timeMillis` onto `g`, whose device pixel `(originX, originY)` is the plan's
    * device pixel `(0, 0)`, where only the plan's device pixels `visible` can show.
    */
  private def drawInto(
      g: Graphics2D,
      plan: DrawPlan.Plan,
      scale: Double,
      originX: Int,
      originY: Int,
      visible: DeviceBox,
      settings: RenderSettings,
      timeMillis: Long
  ): Unit =
    new Painter(settings, LayerPool.shared, timeMillis, scale)
      .drawAll(g, plan.slots, plan.ops, originX, originY, visible)

  /** Draws operations of a plan on the grid of `density` device pixels per logical pixel with
    * `settings` at the instant `timeMillis`, taking the buffers of layers from `pool`.
    */
  private[infrastructure] final class Painter(
      settings: RenderSettings,
      pool: LayerPool,
      timeMillis: Long,
      density: Double
  ):

    /** The pooled buffers of the slots drawn ahead that no operation has composited yet, by the
      * number of the slot.
      */
    private val prepared = mutable.Map[Int, BufferedImage]()

    /** Draws the `slots` of a plan (see `smile.infrastructure.DrawPlan.Plan`) ahead, in order, and
      * then its `ops` onto `g`, whose device pixel `(originX, originY)` is the plan's device pixel
      * `(0, 0)`, where only the plan's device pixels `visible` can show.
      */
    def drawAll(
        g: Graphics2D,
        slots: Vector[DrawPlan.Slot],
        ops: Vector[DrawOp],
        originX: Int,
        originY: Int,
        visible: DeviceBox
    ): Unit =
      try
        for (slot, i) <- slots.zipWithIndex do
          val region = slot.region
          val buffer = pool.acquire(region.width, region.height)
          prepared(i) = buffer
          paintLayer(buffer.getSubimage(0, 0, region.width, region.height), slot.layer, region)
        drawOps(g, ops, originX, originY, visible)
      finally
        prepared.values.foreach(pool.release)
        prepared.clear()

    /** Draws `ops` onto `g`, whose device pixel `(originX, originY)` is the plan's device pixel
      * `(0, 0)`, where only the plan's device pixels `visible` can show.
      */
    private def drawOps(
        g: Graphics2D,
        ops: Vector[DrawOp],
        originX: Int,
        originY: Int,
        visible: DeviceBox
    ): Unit =
      val base = AffineTransform.getTranslateInstance(originX, originY)
      ops.foreach(draw(g, _, base, visible))

    /** Draws `op` onto `g` under `base`, where only the device pixels `visible` can show. */
    private def draw(g: Graphics2D, op: DrawOp, base: AffineTransform, visible: DeviceBox): Unit =
      op match
        case DrawOp.Path(m, shape, fill, stroke) =>
          g.setTransform(under(base, m))
          paint(g, outline(shape), fill, stroke, DrawPlan.paintOrigin(shape.localBox, stroke))
        case DrawOp.Text(m, layout, fill, stroke) =>
          g.setTransform(under(base, m))
          val glyphs = GlyphCache.natural(layout.content, layout.font, layout.size, layout.weight)
          paint(g, glyphs.outline, fill, stroke, DrawPlan.paintOrigin(layout.layoutBox, stroke))
        case image: DrawOp.Image         => drawImage(g, image, base, visible)
        case DrawOp.Foreign(document, m) =>
          g.setTransform(base)
          g.setComposite(AlphaComposite.SrcOver)
          document.render(g, m)
        case DrawOp.Pixel(x, y, paint) =>
          g.setTransform(base)
          g.setComposite(AlphaComposite.SrcOver)
          g.setColor(AwtConversions.colorToAWT(paint match
            case color: Color             => color
            case gradient: LinearGradient => gradient.averageColor))
          g.fillRect(x, y, 1, 1)
        case layer: DrawOp.Layer =>
          // Only the pixels of the layer that can show are drawn, so a large layer that a clip
          // mostly hides takes a small buffer.
          val region = DrawPlan.intersection(layer.box, visible)
          withLayer(layer, region)(composite(g, _, base, region))
        case DrawOp.HardClip(box, ops) =>
          // The clip lives on graphics of its own, so that it ends with them.
          val clipped = g.create().asInstanceOf[Graphics2D]
          try
            clipped.setTransform(base)
            clipped.clip(new Rectangle(box.left, box.top, box.width, box.height))
            val inside = DrawPlan.intersection(visible, box)
            ops.foreach(draw(clipped, _, base, inside))
          finally clipped.dispose()
        case DrawOp.Tiles(tile, copies, _) =>
          withLayer(tile, tile.box): image =>
            // The copies set their interpolation on graphics of their own, which end with them,
            // so that what is drawn after them does not inherit it.
            val copying = g.create().asInstanceOf[Graphics2D]
            try
              copying.setComposite(AlphaComposite.SrcOver)
              for copy <- copies do
                val placed = under(base, copy)
                // The tile's pixels are copied as they are where a copy only moves them, by whole
                // pixels, and resampled where it turns or scales them.
                val moves = (placed.getType & ~AffineTransform.TYPE_TRANSLATION) == 0
                copying.setRenderingHint(
                  RenderingHints.KEY_INTERPOLATION,
                  if moves then RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
                  else RenderingHints.VALUE_INTERPOLATION_BILINEAR
                )
                copying.setTransform(placed)
                val _ = copying.drawImage(image, 0, 0, null)
            finally copying.dispose()
        case DrawOp.Prepared(slot, region) =>
          // The slot's buffer is composited as a layer's is, and only once.
          val buffer = prepared(slot)
          prepared -= slot
          try composite(g, buffer.getSubimage(0, 0, region.width, region.height), base, region)
          finally pool.release(buffer)
      end match
    end draw

    /** Draws `image`, the pixels of a layer within the device pixels `region`, over `g` at `region`
      * under `base`.
      */
    private def composite(
        g: Graphics2D,
        image: BufferedImage,
        base: AffineTransform,
        region: DeviceBox
    ): Unit =
      g.setTransform(base)
      g.setComposite(AlphaComposite.SrcOver)
      val _ = g.drawImage(image, region.left, region.top, null)

    /** Runs `work` on the pixels of `layer` within the device pixels `box`, drawn into a pooled
      * buffer of those pixels with its clip and its masks applied. A box of no pixels draws
      * nothing.
      */
    private def withLayer(layer: DrawOp.Layer, box: DeviceBox)(work: BufferedImage => Unit): Unit =
      if box.width > 0 && box.height > 0 then
        val content = pool.acquire(box.width, box.height)
        try
          val image = content.getSubimage(0, 0, box.width, box.height)
          paintLayer(image, layer, box)
          work(image)
        finally pool.release(content)

    /** Draws the pixels of `layer` within the device pixels `box` into `image`, whose pixel
      * `(0, 0)` is the corner of `box`, with its clip and its masks applied.
      */
    private def paintLayer(image: BufferedImage, layer: DrawOp.Layer, box: DeviceBox): Unit =
      paintInto(image, box, layer.ops)
      val masks = layer.clip.map(clip => (clip.asMask, false)) ++ layer.masks
      for (ops, inverted) <- masks do
        val maskBuffer = pool.acquire(box.width, box.height)
        try
          val maskImage = maskBuffer.getSubimage(0, 0, box.width, box.height)
          paintInto(maskImage, box, ops)
          val g = image.createGraphics()
          try
            g.setComposite(if inverted then AlphaComposite.DstOut else AlphaComposite.DstIn)
            val _ = g.drawImage(maskImage, 0, 0, null)
          finally g.dispose()
        finally pool.release(maskBuffer)

    /** Draws `ops` into `image`, cleared first, whose pixel `(0, 0)` is the device pixel at the
      * corner of `box`.
      */
    private def paintInto(image: BufferedImage, box: DeviceBox, ops: Vector[DrawOp]): Unit =
      val g = image.createGraphics()
      try
        JVMBufferAdapter.configure(g, settings)
        g.setComposite(AlphaComposite.Clear)
        g.fillRect(0, 0, box.width, box.height)
        g.setComposite(AlphaComposite.SrcOver)
        drawOps(g, ops, -box.left, -box.top, box)
      finally g.dispose()

    private def drawImage(
        g: Graphics2D,
        image: DrawOp.Image,
        base: AffineTransform,
        visible: DeviceBox
    ): Unit =
      DrawPlan
        .imagePixels(image, visible, settings, timeMillis)
        .foreach((pixels, m) => drawPixels(g, image, base, visible, pixels, m))

    /** Draws `pixels` under `m` as `image` says, inside its clip and resampled as it asks, as
      * `smile.infrastructure.CanvasPlanRenderer` draws them.
      *
      * Snapped pixels are copied as they are. Resampled ones are drawn whole, or where the clip
      * crops them, from the source pixels the clip reaches alone (see
      * `smile.infrastructure.DrawPlan.sourceWindow`), so that nothing outside the crop is sampled
      * into it. Those pixels get a border that repeats their edge pixels, as on the canvas, so that
      * they cover every device pixel of the clip even where the window ends inside it. Where `m`
      * keeps the axes, the clip is on whole device pixels and Java2D's own clip keeps it.
      *
      * Where `m` turns the axes, the canvas and SVG antialias the pixels' edges, and Java2D, which
      * clips and draws a turned image without antialiasing, would leave them jagged. So a turned
      * bitmap drawn whole and bilinearly, with no source pixel wider than a device pixel, is drawn
      * once from a copy with a transparent border (see
      * `smile.infrastructure.JVMBufferAdapter.fadedImage`). Its edges fade out over at most a
      * device pixel, as far as a canvas antialiases them, and a whole-pixel clip of the device
      * pixels its extent touches keeps the fade's outermost corner within its bounds. Every other
      * turned bitmap, cropped, wider than a device pixel per source pixel, or sampled otherwise, is
      * drawn inside its extent intersected with the clip through a layer with an antialiased mask
      * of that polygon (see `resampleTurned`).
      */
    private def drawPixels(
        g: Graphics2D,
        image: DrawOp.Image,
        base: AffineTransform,
        visible: DeviceBox,
        adapter: NativeBuffer,
        m: Affine
    ): Unit =
      val whole = adapter.readOnlyImage
      if image.snapped then drawSnapped(g, image, base, whole, m)
      else
        image.clip.flatMap(DrawPlan.sourceWindow(_, m, whole.getWidth, whole.getHeight)) match
          case None if m.keepsAxes =>
            scaledAtDensity(adapter, image, m) match
              case Some(scaled) =>
                drawSnapped(g, image, base, scaled.readOnlyImage, Affine.translation(m.e, m.f))
              case None =>
                // Within the clip, which lies within the extent, Java2D repeats the edge pixels
                // as the canvas's border does.
                resample(g, image, base, adapter.premultipliedImage, m, image.clip)
          case None
              if image.clip.isEmpty && greatestScale(m) <= 1 + 1e-9 &&
                DrawPlan.interpolation(image, settings) == RasterInterpolation.Bilinear =>
            // A fade over a source pixel wider than a device pixel would be softer than a
            // canvas's edge, and a nearest or bicubic filter would not fade it as a bilinear one
            // does, so those take the layer.
            val (w, h)  = (whole.getWidth.toDouble, whole.getHeight.toDouble)
            val extent  = Vector(Pos(0, 0), Pos(w, 0), Pos(w, h), Pos(0, h)).map(m(_))
            val covered = DeviceGrid.coveringDevice(DrawPlan.boxAround(extent))
            resample(
              g,
              image,
              base,
              adapter.fadedImage,
              Affine.translation(-1, -1).andThen(m),
              None,
              Some(covered)
            )
          case None =>
            resampleTurned(g, image, base, visible, adapter.borderedImage, m)
          case Some(window) =>
            val toWindow = Affine.translation(window.left, window.top).andThen(m)
            val border   =
              adapter.borderedImage(window.left, window.top, window.width, window.height)
            if toWindow.keepsAxes then
              resample(
                g,
                image,
                base,
                border,
                Affine.translation(-1, -1).andThen(toWindow),
                image.clip
              )
            else resampleTurned(g, image, base, visible, border, toWindow)
    end drawPixels

    /** The pixels of `adapter` scaled as `image` draws them under `m`, kept in
      * `smile.infrastructure.RasterCache.shared`, where `m` draws them at their own size on this
      * output of a density other than 1. That is where `m` scales by the density alone and moves
      * them by whole device pixels. Scaled at the origin, they are the same on every frame, and
      * copied 1:1 to where `m` moves them, they are the pixels that resampling them under `m`
      * gives, at the densities `keepsScaledRasters` accepts.
      *
      * The raster is made and kept from the second ask. A key names one revision of the pixels,
      * density, interpolation and settings. An ask under a key that is not kept yet makes the
      * raster only when the buffer's previous ask was under that key too, and otherwise notes the
      * key on the buffer (`askedBefore`) and gives `None`. So a bitmap made anew for every frame is
      * drawn directly, without a raster that nothing would read again. Which ask makes the raster
      * cannot change the pixels, since at these densities the kept raster is exactly what drawing
      * directly gives.
      *
      * It is `None`, and the pixels are resampled as they are drawn, in each of these cases:
      *
      *   - At other densities.
      *   - For a bitmap the program scales or moves off the device grid, whose raster would be made
      *     again on every frame.
      *   - For pixels whose image has escaped.
      *   - For a scaled size that is not whole.
      *   - For a raster larger than a quarter of the cache's budget.
      *
      * The equality with drawing directly is established for software image targets, which every
      * rendering draws into. A hardware-accelerated surface given to `renderInto` may scale a
      * bitmap drawn directly, such as one whose image has escaped, in a way of its own.
      */
    private def scaledAtDensity(
        adapter: NativeBuffer,
        image: DrawOp.Image,
        m: Affine
    ): Option[NativeBuffer] =
      val (width, height)  = (adapter.width * density, adapter.height * density)
      def whole(v: Double) = v == math.rint(v)
      val atDensity        = keepsScaledRasters(density) && m.a == density && m.d == density &&
        m.b == 0 && m.c == 0 && whole(m.e) && whole(m.f) && whole(width) && whole(height)
      if !atDensity || width * height * 4 > RasterCache.shared.budgetBytes / 4 then None
      else
        val interpolation = DrawPlan.interpolation(image, settings)
        adapter
          .scaledKey(density, interpolation, settings)
          .filter(key => RasterCache.shared.peek(key).isDefined || adapter.askedBefore(key))
          .map: key =>
            RasterCache.shared.getOrCompute(key):
              val scaled = new JVMBufferAdapter(
                new BufferedImage(width.toInt, height.toInt, BufferedImage.TYPE_INT_ARGB_PRE)
              )
              val g = scaled.imageToDrawOn.createGraphics()
              try
                resample(
                  g,
                  image,
                  new AffineTransform(),
                  adapter.premultipliedImage,
                  Affine.scaling(density, density),
                  None
                )
              finally g.dispose()
              scaled
    end scaledAtDensity

    /** Copies `source` under `m`, which the plan put on whole device pixels, inside the image's
      * clip.
      */
    private def drawSnapped(
        g: Graphics2D,
        image: DrawOp.Image,
        base: AffineTransform,
        source: BufferedImage,
        m: Affine
    ): Unit =
      // The clip lives on graphics of its own, so that it ends with them.
      val target = g.create().asInstanceOf[Graphics2D]
      try
        target.setComposite(AlphaComposite.SrcOver)
        image.clip.foreach: corners =>
          target.setTransform(base)
          clipTo(target, corners)
        val placed = under(base, m)
        // The float error of composing the affines is taken off, so that the pixels are copied as
        // they are.
        placed.setTransform(
          placed.getScaleX,
          placed.getShearY,
          placed.getShearX,
          placed.getScaleY,
          Math.rint(placed.getTranslateX),
          Math.rint(placed.getTranslateY)
        )
        target.setRenderingHint(
          RenderingHints.KEY_INTERPOLATION,
          RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
        )
        target.setTransform(placed)
        val _ = target.drawImage(source, 0, 0, null)
      finally target.dispose()

    /** Resamples `source` under `m` inside the polygon `clip` on whole device pixels, where there
      * is one, and inside the device pixels `box`, where there is one.
      */
    private def resample(
        g: Graphics2D,
        image: DrawOp.Image,
        base: AffineTransform,
        source: BufferedImage,
        m: Affine,
        clip: Option[Vector[Pos]],
        box: Option[DeviceBox] = None
    ): Unit =
      val target = g.create().asInstanceOf[Graphics2D]
      try
        target.setComposite(AlphaComposite.SrcOver)
        clip.foreach: corners =>
          target.setTransform(base)
          clipTo(target, corners)
        box.foreach: b =>
          target.setTransform(base)
          target.clipRect(b.left, b.top, b.width, b.height)
        target.setTransform(biased(interpolate(target, image), under(base, m)))
        val _ = target.drawImage(source, 0, 0, null)
      finally target.dispose()

    /** Resamples `bordered`, pixels with a border of one pixel that repeats their edge pixels,
      * under `m`, which turns the axes and takes the pixels inside the border to device pixels,
      * inside their extent and the image's clip, with antialiased edges, where only the device
      * pixels `visible` can show.
      *
      * Where no source pixel is narrower than a device pixel, one layer does. The antialiased
      * polygon is filled first and the pixels drawn over it keeping its coverage (see
      * `resampleTurnedInOneLayer`). Narrower pixels might not reach every device pixel the
      * polygon's edge touches, so they are drawn into a layer and a second layer holding the
      * antialiased polygon is laid over it as a mask.
      */
    private def resampleTurned(
        g: Graphics2D,
        image: DrawOp.Image,
        base: AffineTransform,
        visible: DeviceBox,
        bordered: BufferedImage,
        m: Affine
    ): Unit =
      val (width, height) = (bordered.getWidth - 2, bordered.getHeight - 2)
      val extent          =
        Vector((0, 0), (width, 0), (width, height), (0, height))
          .map((x, y) => m(Pos(x.toDouble, y.toDouble)))
      val corners = image.clip.fold(extent)(RasterPlan.intersectConvex(_, extent))
      if corners.length >= 3 then
        val region =
          DrawPlan.intersection(DeviceGrid.coveringDevice(DrawPlan.boxAround(corners)), visible)
        if region.width > 0 && region.height > 0 && leastScale(m) >= 1 then
          resampleTurnedInOneLayer(g, image, base, region, bordered, m, corners)
        else if region.width > 0 && region.height > 0 then
          val layer = pool.acquire(region.width, region.height)
          val mask  = pool.acquire(region.width, region.height)
          try
            val toRegion = Affine.translation(-region.left, -region.top)
            val content  = layer.getSubimage(0, 0, region.width, region.height)
            val shape    = mask.getSubimage(0, 0, region.width, region.height)
            val drawing  = content.createGraphics()
            try
              drawing.setComposite(AlphaComposite.Clear)
              drawing.fillRect(0, 0, region.width, region.height)
              drawing.setComposite(AlphaComposite.SrcOver)
              val interpolation = interpolate(drawing, image)
              val toDevice      = Affine.translation(-1, -1).andThen(m).andThen(toRegion)
              drawing.setTransform(
                biased(interpolation, under(new AffineTransform(), toDevice))
              )
              val _ = drawing.drawImage(bordered, 0, 0, null)
            finally drawing.dispose()
            paintInto(
              shape,
              region,
              Vector(
                DrawOp.Path(
                  Affine.Identity,
                  PathShape(IArray.from(corners.flatMap(p => Seq(p.x, p.y))), true),
                  Some(FillStyle(Color(0, 0, 0))),
                  None
                )
              )
            )
            val masking = content.createGraphics()
            try
              masking.setComposite(AlphaComposite.DstIn)
              val _ = masking.drawImage(shape, 0, 0, null)
            finally masking.dispose()
            composite(g, content, base, region)
          finally
            pool.release(mask)
            pool.release(layer)
    end resampleTurned

    /** Draws as `resampleTurned` does, into one layer. Only the device pixels the drawn pixels
      * reach take them, so this needs the border to reach every pixel the polygon's antialiased
      * edge touches. That takes a source pixel at least a device pixel wide, which `m` scaling by
      * at least 1 gives.
      *
      * Antialiased, the bordered pixels under `m` replace what the layer held, and then each pixel
      * is scaled by the share of it that the polygon `corners` covers, computed exactly (see
      * `smile.infrastructure.PolygonCoverage`), which also clears every pixel outside it. That
      * costs a fraction of Java2D's antialiased fill of the polygon, and a game turns many sprites
      * on every frame. Without antialiasing, the polygon is filled first and the pixels drawn over
      * it keeping its coverage (`SrcIn`).
      */
    private def resampleTurnedInOneLayer(
        g: Graphics2D,
        image: DrawOp.Image,
        base: AffineTransform,
        region: DeviceBox,
        bordered: BufferedImage,
        m: Affine,
        corners: Vector[Pos]
    ): Unit =
      val layer = pool.acquire(region.width, region.height)
      try
        val content     = layer.getSubimage(0, 0, region.width, region.height)
        val antialiased = settings.drawingIsAntiAliased
        val drawing     = content.createGraphics()
        try
          JVMBufferAdapter.configure(drawing, settings)
          if antialiased then drawing.setComposite(AlphaComposite.Src)
          else
            drawing.setComposite(AlphaComposite.Clear)
            drawing.fillRect(0, 0, region.width, region.height)
            drawing.setComposite(AlphaComposite.SrcOver)
            drawing.setColor(java.awt.Color.BLACK)
            drawing.translate(-region.left, -region.top)
            drawing.fill(polygon(corners))
            drawing.setComposite(AlphaComposite.SrcIn)
          val interpolation = interpolate(drawing, image)
          val toDevice      =
            Affine
              .translation(-1, -1)
              .andThen(m)
              .andThen(Affine.translation(-region.left, -region.top))
          drawing.setTransform(biased(interpolation, under(new AffineTransform(), toDevice)))
          val _ = drawing.drawImage(bordered, 0, 0, null)
        finally drawing.dispose()
        if antialiased then
          PolygonCoverage.mask(
            layer,
            region.width,
            region.height,
            corners.iterator.map(_.x - region.left).toArray,
            corners.iterator.map(_.y - region.top).toArray
          )
        composite(g, content, base, region)
      finally pool.release(layer)

    /** Sets the interpolation that `image` asks for on `g`, and returns it. */
    private def interpolate(g: Graphics2D, image: DrawOp.Image): RasterInterpolation =
      val interpolation = DrawPlan.interpolation(image, settings)
      g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        interpolation match
          case RasterInterpolation.Nearest  => RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
          case RasterInterpolation.Bilinear => RenderingHints.VALUE_INTERPOLATION_BILINEAR
          case RasterInterpolation.Bicubic  => RenderingHints.VALUE_INTERPOLATION_BICUBIC
      )
      interpolation

    /** `placed` moved right and down by `TieBias` of a device pixel where `interpolation` takes the
      * nearest pixel, so that a device pixel whose center falls exactly on the edge between two
      * source pixels takes the one to the left and above, as a browser's canvas does. Java2D takes
      * the other one. Any other center takes the same pixel either way, unless it lies closer than
      * that to an edge.
      */
    private def biased(
        interpolation: RasterInterpolation,
        placed: AffineTransform
    ): AffineTransform =
      if interpolation == RasterInterpolation.Nearest then
        placed.preConcatenate(AffineTransform.getTranslateInstance(TieBias, TieBias))
      placed

    /** Fills and strokes `shape`, which is in the local frame `g` draws in, the stroke behind the
      * fill unless it is on top, with gradients measured from `origin` in that frame.
      */
    private def paint(
        g: Graphics2D,
        shape: Shape,
        fill: Option[FillStyle],
        stroke: Option[StrokeStyle],
        origin: Pos
    ): Unit =
      g.setComposite(AlphaComposite.SrcOver)
      // Java2D colors a pixel with a gradient's value at the pixel's upper left corner, and the
      // canvas and SVG take it at the pixel's center, so the gradient is moved back by half a
      // device pixel, as that measures in the local frame.
      val paintOrigin =
        try
          val half =
            g.getTransform.createInverse().deltaTransform(new Point2D.Double(0.5, 0.5), null)
          Pos(origin.x - half.getX, origin.y - half.getY)
        catch case _: NoninvertibleTransformException => origin
      def paintFill(): Unit =
        fill.foreach: style =>
          g.setPaint(AwtConversions.paintToAWT(style.paint, paintOrigin))
          g.fill(shape)
      def paintStroke(): Unit =
        stroke.foreach: style =>
          g.setStroke(AwtConversions.strokeToAWT(style))
          g.setPaint(AwtConversions.paintToAWT(style.paint, paintOrigin))
          g.draw(shape)
      if stroke.exists(_.onTop) then
        paintFill()
        paintStroke()
      else
        paintStroke()
        paintFill()
    end paint
  end Painter

  /** How far, in device pixels, a nearest-neighbor image is moved so that a sample on the edge
    * between two source pixels takes the one before it (see `Painter.biased`).
    */
  private val TieBias = 1.0 / 1024

  /** Whether bitmaps drawn at their own size on an output of `density` device pixels per logical
    * pixel keep their scaled rasters. They do where it is a power of two above 1, such as 2 or 4.
    *
    * Java2D finds the source position of each device pixel it resamples by stepping from the first
    * one it draws, which is where the clip starts, and those steps are exact only where the density
    * is a power of two. At any other density, such as 1.5 or 3, their float error depends on where
    * the clip starts, so a raster scaled at the origin and copied would differ by a level here and
    * there from drawing the bitmap directly under a clip that starts inside it. At 1 there is
    * nothing to keep, since such a bitmap is copied as it is, and densities below 1, which no
    * display has, are left out.
    */
  private def keepsScaledRasters(density: Double): Boolean =
    density > 1 && density == Math.scalb(1.0, Math.getExponent(density))

  /** The greatest factor by which `m` scales any length, its larger singular value. */
  private def greatestScale(m: Affine): Double =
    val sum         = m.a * m.a + m.b * m.b + m.c * m.c + m.d * m.d
    val determinant = m.a * m.d - m.b * m.c
    math.sqrt((sum + math.sqrt((sum * sum - 4 * determinant * determinant).max(0))) / 2)

  /** The least factor by which `m` scales any length, its smaller singular value. */
  private def leastScale(m: Affine): Double =
    val sum         = m.a * m.a + m.b * m.b + m.c * m.c + m.d * m.d
    val determinant = m.a * m.d - m.b * m.c
    math.sqrt(((sum - math.sqrt((sum * sum - 4 * determinant * determinant).max(0))) / 2).max(0))

  private def under(base: AffineTransform, m: Affine): AffineTransform =
    val composed = new AffineTransform(base)
    composed.concatenate(new AffineTransform(m.a, m.b, m.c, m.d, m.e, m.f))
    composed

  /** Clips `g` to the polygon `corners` in its current frame. The clip is a rectangle where the
    * polygon is one on whole device pixels along the axes, which Java2D keeps far more cheaply than
    * the shape of a path, and the path otherwise. Both keep exactly the pixels whose centers lie
    * inside.
    */
  private def clipTo(g: Graphics2D, corners: Vector[Pos]): Unit =
    rectangleOf(corners) match
      case Some(box) => g.clipRect(box.left, box.top, box.width, box.height)
      case None      => g.clip(polygon(corners))

  /** The device pixels the polygon `corners` covers when it is a rectangle of some area along the
    * axes with its corners on whole device pixels, listed around it from any corner. Four corners
    * on the axes that do not make up such a rectangle, such as a degenerate one that doubles back
    * along an edge, give `None`, and the path keeps exactly what they cover.
    */
  private[infrastructure] def rectangleOf(corners: Vector[Pos]): Option[DeviceBox] =
    def whole(v: Double) = v == math.rint(v) && v.abs < Int.MaxValue / 2
    if corners.length != 4 || !corners.forall(p => whole(p.x) && whole(p.y)) then None
    else
      val left   = corners.iterator.map(_.x).min
      val top    = corners.iterator.map(_.y).min
      val right  = corners.iterator.map(_.x).max
      val bottom = corners.iterator.map(_.y).max
      val box    = Set(Pos(left, top), Pos(right, top), Pos(right, bottom), Pos(left, bottom))
      // Four distinct corners of the box make a rectangle only if each edge runs along an axis.
      // Listed crosswise, they make a bow tie.
      val upright = (0 until 4).forall: i =>
        val (from, to) = (corners(i), corners((i + 1) % 4))
        from.x == to.x || from.y == to.y
      if left < right && top < bottom && corners.toSet == box && upright then
        Some(DeviceBox(left.toInt, top.toInt, (right - left).toInt, (bottom - top).toInt))
      else None

  private def polygon(corners: Vector[Pos]): Path2D =
    val path = new Path2D.Double()
    corners.headOption.foreach(first => path.moveTo(first.x, first.y))
    corners.drop(1).foreach(corner => path.lineTo(corner.x, corner.y))
    path.closePath()
    path

  /** The Java2D outline of a local shape, in its local frame. */
  private def outline(shape: LocalShape): Shape =
    shape match
      case path: PathShape =>
        val p  = new Path2D.Double()
        val xy = path.xy
        p.moveTo(xy(0), xy(1))
        var i = 2
        while i < xy.length do
          p.lineTo(xy(i), xy(i + 1))
          i += 2
        if path.closed then p.closePath()
        p
      case arc: EllipseArcShape =>
        if arc.isFullCycle then new Ellipse2D.Double(-arc.rx, -arc.ry, 2 * arc.rx, 2 * arc.ry)
        else
          // Java2D's angles run counterclockwise on screen from three o'clock, measured on the
          // ellipse as if it were a circle stretched onto it, as the shape's are.
          new Arc2D.Double(
            -arc.rx,
            -arc.ry,
            2 * arc.rx,
            2 * arc.ry,
            arc.startAngle,
            arc.arcAngle,
            Arc2D.OPEN
          )
      case _: PointShape   => new Rectangle2D.Double(-0.5, -0.5, 1, 1)
      case text: TextShape =>
        val layout = text.layout
        GlyphCache.natural(layout.content, layout.font, layout.size, layout.weight).outline

  /** The buffers of layers, shared by every thread. See [[smile.infrastructure.BufferPool]]. */
  private[infrastructure] final class LayerPool(budget: Long)
      extends BufferPool[BufferedImage](budget):
    protected def create(width: Int, height: Int): BufferedImage =
      new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB_PRE)
    protected def widthOf(image: BufferedImage): Int  = image.getWidth
    protected def heightOf(image: BufferedImage): Int = image.getHeight

  private[infrastructure] object LayerPool:
    val Budget: Long = BufferPool.Budget

    /** The pool every rendering takes its layers from. */
    val shared: LayerPool = new LayerPool(Budget)
end JVMPlanRenderer
