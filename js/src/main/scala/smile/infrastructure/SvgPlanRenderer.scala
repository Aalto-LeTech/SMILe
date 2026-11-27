package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import smile.RenderSettings
import smile.colors.{Color, LinearGradient, Paint}
import smile.infrastructure.{FontFamily, JSBufferAdapter}
import smile.modeling.{Affine, Bounds, Pos, RasterInterpolation, RasterPlan}
import smile.modeling.{EllipseArcShape, LocalShape, PathShape, PointShape, TextLayout, TextShape}
import smile.pictures.{Element, SvgDocument}
import smile.pictures.{BitmapSource, FillStyle, StrokeStyle}

import scala.collection.mutable

/** Renders the shared draw plan (see `smile.infrastructure.DrawPlan`) as an SVG document, as
  * `smile.infrastructure.CanvasPlanRenderer` renders it onto a canvas.
  *
  * SVG output is resolution-independent, so the plan is made at one device pixel per logical pixel,
  * and its device pixels are the logical units of the SVG's `viewBox`. Every decision about whole
  * pixels is the plan's at that scale, whatever the density the SVG is shown at, and the output is
  * the same for every `density` but for the resolution of the rasters it embeds. Those are picture
  * sources rendered to pixels and the tiles of wraps whose period is turned or not on whole pixels.
  * Pixel sources are embedded with their own pixels.
  *
  * A wrap is one `pattern` of its tile, filled over the pixels that can show. Written as one
  * element per copy, each copy's edge would be antialiased on its own. The tile keeps the wrap's
  * content as vectors where the plan marks it `vector`, which it does for a period on whole pixels,
  * and where the content embeds an SVG document. Every other tile is a raster of the pixels that
  * the canvas renders of it.
  *
  * Each operation with an affine is drawn under it as `transform="matrix(a b c d e f)"`, so
  * strokes, gradients and glyphs are drawn in their local frame, as on the canvas. Nothing is
  * rounded here. Clips, masks, gradients and embedded images are definitions in `defs` with ids
  * that count up within a rendering after a prefix of its own, which a random token of the page
  * load and the rendering's number make, so that two renderings in one document never share an id,
  * even ones saved from different pages. Each embedded SVG document and each embedded raster is
  * defined once, and every drawing of it is a `use` of its definition. A wrap's tile is a
  * definition too, which its pattern alone uses.
  */
private[smile] object SvgPlanRenderer:

  private val Namespace = "http://www.w3.org/2000/svg"
  private val XLink     = "http://www.w3.org/1999/xlink"
  private val Xmlns     = "http://www.w3.org/2000/xmlns/"

  /** How many renderings have been made, which numbers each rendering's ids. JavaScript runs on one
    * thread.
    */
  private var renderings = 0L

  /** Eight random lowercase letters, drawn once per page load, that every id starts with after
    * `smile`, so that SVGs saved from two pages and put into one document do not share an id, as
    * the count of renderings alone, which starts over on each page, would have them do.
    */
  private[smile] val SessionToken: String =
    String.valueOf(Array.fill(8)(('a' + scala.util.Random.nextInt(26)).toChar))

  /** `element` drawn with `settings` at the instant `timeMillis` as an `svg` element whose
    * `viewBox` is the whole logical pixels its bounds cover, with its rasters embedded at `density`
    * pixels per logical pixel. The empty picture is one transparent pixel.
    */
  def render(
      element: Element,
      settings: RenderSettings,
      density: Double = 1.0,
      timeMillis: Long = System.currentTimeMillis(),
      nesting: Int = DrawPlan.MaxNesting
  ): dom.Element =
    require(density > 0 && density.isFinite, s"The density must be positive and finite: $density")
    renderings += 1
    val plan = DrawPlan.planned(element, DeviceGrid(1), settings, timeMillis, nesting, svg = true)
    val box  = plan.box
    val root = node("svg")
    root.setAttributeNS(Xmlns, "xmlns", Namespace)
    root.setAttributeNS(Xmlns, "xmlns:xlink", XLink)
    if box.width <= 0 || box.height <= 0 then
      attributes(root, "width" -> "1", "height" -> "1", "viewBox" -> "0 0 1 1")
    else
      attributes(
        root,
        "width"   -> box.width.toString,
        "height"  -> box.height.toString,
        "viewBox" -> s"${box.left} ${box.top} ${box.width} ${box.height}",
        "style"   -> "user-select: none; -webkit-user-select: none;"
      )
      val writer =
        new Writer(settings, density, timeMillis, s"smile$SessionToken$renderings-", plan.slots)
      val content = writer.drawPlan(plan.ops, box)
      if writer.defs.childElementCount > 0 then
        val _ = root.appendChild(writer.defs)
      content.foreach(root.appendChild)
    root
  end render

  /** The same as `smile.infrastructure.SvgPlanRenderer.render`, serialized as text. */
  def string(
      element: Element,
      settings: RenderSettings,
      density: Double = 1.0,
      timeMillis: Long = System.currentTimeMillis()
  ): String =
    new dom.XMLSerializer().serializeToString(render(element, settings, density, timeMillis))

  /** Writes the operations of one rendering at the instant `timeMillis`, whose plan draws `slots`
    * ahead (see `smile.infrastructure.DrawPlan.Plan`), with its definitions gathered into `defs`.
    */
  private final class Writer(
      settings: RenderSettings,
      density: Double,
      timeMillis: Long,
      prefix: String,
      slots: Vector[DrawPlan.Slot]
  ):

    val defs: dom.Element = node("defs")

    private var count = 0

    /** The definitions of embedded SVG documents, by document. */
    private val documents = scala.collection.mutable.LinkedHashMap[SvgDocument, String]()

    /** The definitions of embedded rasters. Each is a buffer, the window of it that is embedded, or
      * `None` for all of it, whether its edges are repeated around it, and the id.
      */
    private var rasters = Vector[(NativeBuffer, Option[DeviceBox], Boolean, String)]()

    private def nextId(kind: String): String =
      count += 1
      s"$prefix$kind$count"

    private def define(e: dom.Element): String =
      val _ = defs.appendChild(e)
      e.getAttribute("id")

    /** Whether each slot embeds an SVG document anywhere within it, by the number of the slot. */
    private lazy val slotEmbeds: Array[Boolean] =
      val embeds = new Array[Boolean](slots.size)
      for (slot, i) <- slots.zipWithIndex do embeds(i) = embedsDocument(slot.layer, embeds)
      embeds

    /** The slots as the raster of `tile` at `density` reads them. `scaledBy` draws the tile
      * `density` times as large but the tiles inside it at their own pixels, so the slots the tile
      * draws outside those tiles are scaled with it, and the others are left as they are.
      */
    private def denseSlots(tile: DrawOp.Layer): Vector[DrawPlan.Slot] =
      if density == 1 then slots
      else
        val scaled = DrawPlan.drawnSlots(slots, Vector(tile), opens = _ => false)
        slots.zipWithIndex.map: (slot, i) =>
          if !scaled(i) then slot
          else DrawPlan.Slot(scaledBy(slot.layer, density), covering(slot.region, density))

    /** The elements of the slots built so far, by the number of the slot. */
    private val prepared = mutable.Map[Int, Vector[dom.Element]]()

    /** The elements that draw `ops`, back first, onto the device pixels `visible`, once the slots
      * they draw are built, in order. A slot drawn only within the raster of a wrap tile is drawn
      * into that raster instead.
      */
    def drawPlan(ops: Vector[DrawOp], visible: DeviceBox): Vector[dom.Element] =
      val built =
        DrawPlan.drawnSlots(slots, ops, t => t.vector || embedsDocument(t.tile, slotEmbeds))
      for (slot, i) <- slots.zipWithIndex if built(i) do
        prepared(i) = drawLayer(slot.layer).toVector
      drawAll(ops, visible)

    /** The elements that draw `ops`, back first, where only the device pixels `visible` show. */
    private def drawAll(ops: Vector[DrawOp], visible: DeviceBox): Vector[dom.Element] =
      ops.flatMap(draw(_, visible))

    private def draw(op: DrawOp, visible: DeviceBox): Vector[dom.Element] =
      op match
        case DrawOp.Path(m, shape, fill, stroke) =>
          shape match
            case text: TextShape => Vector(drawText(m, text.layout, fill, stroke))
            case _               =>
              val e = shapeElement(shape)
              paint(e, DrawPlan.paintOrigin(shape.localBox, stroke), fill, stroke)
              Vector(under(m, e))
        case DrawOp.Text(m, layout, fill, stroke) => Vector(drawText(m, layout, fill, stroke))
        case image: DrawOp.Image                  => drawImage(image, visible).toVector
        case DrawOp.Foreign(document, m)          =>
          Vector(use(documentId(document), m))
        case DrawOp.Pixel(x, y, color) =>
          Vector(
            node(
              "rect",
              "x"      -> x.toString,
              "y"      -> y.toString,
              "width"  -> "1",
              "height" -> "1",
              "fill"   -> flat(color).toString
            )
          )
        case layer: DrawOp.Layer       => drawLayer(layer).toVector
        case DrawOp.HardClip(box, ops) =>
          val g = node("g", "clip-path" -> s"url(#${boxClip(box)})")
          drawAll(ops, DrawPlan.intersection(visible, box)).foreach(g.appendChild)
          Vector(g)
        case DrawOp.Tiles(tile, copies, vector) =>
          if tile.box.width <= 0 || tile.box.height <= 0 || copies.isEmpty then Vector()
          else
            // The copies all lie on one lattice, which starts at the first copy and steps by the
            // tile's size. One pattern filled over the visible pixels draws every one of them, so
            // that no copy's edge is antialiased on its own.
            val pattern = node(
              "pattern",
              "id"               -> nextId("repeat"),
              "patternUnits"     -> "userSpaceOnUse",
              "width"            -> tile.box.width.toString,
              "height"           -> tile.box.height.toString,
              "patternTransform" -> matrix(copies.head)
            )
            val _  = pattern.appendChild(use(tileId(tile, vector), Affine.Identity))
            val id = define(pattern)
            Vector(
              node(
                "rect",
                "x"      -> visible.left.toString,
                "y"      -> visible.top.toString,
                "width"  -> visible.width.toString,
                "height" -> visible.height.toString,
                "fill"   -> s"url(#$id)"
              )
            )
        case DrawOp.Prepared(slot, _) =>
          val elements = prepared(slot)
          prepared -= slot
          elements

    /** A group of what `layer` draws, inside its clip and its masks, or `None` for a layer of no
      * pixels.
      *
      * The group is clipped to the layer's box, as the canvas draws a layer into a canvas of its
      * box, unless a clip shape or a mask, which never reach past the box, already keeps it there.
      * The first mask is the group's own, and each later one that of a group around the group of
      * the masks before it, so that the masks apply in order, as on the canvas.
      */
    private def drawLayer(layer: DrawOp.Layer): Option[dom.Element] =
      val box = layer.box
      if box.width <= 0 || box.height <= 0 then None
      else
        val g = node("g")
        layer.clip match
          case Some(clip) => g.setAttribute("clip-path", s"url(#${shapeClip(clip)})")
          case None       =>
            if layer.masks.isEmpty then g.setAttribute("clip-path", s"url(#${boxClip(box)})")
        val (first, later) = layer.masks.splitAt(1)
        for (ops, inverted) <- first do g.setAttribute("mask", s"url(#${mask(box, ops, inverted)})")
        drawAll(layer.ops, box).foreach(g.appendChild)
        val masked = later.foldLeft(g) { case (inner, (ops, inverted)) =>
          val outer = node("g", "mask" -> s"url(#${mask(box, ops, inverted)})")
          val _     = outer.appendChild(inner)
          outer
        }
        Some(masked)

    /** The id of an alpha mask of `ops` over the device pixels `box`, which keeps what they cover,
      * or for an `inverted` one, what they do not.
      *
      * An alpha mask cannot be inverted as it is, so an inverted one cuts its shape out of an
      * opaque flood with a filter, whose opacity is then what is left where the shape is not. The
      * flood covers the box and a pixel more on every side.
      */
    private def mask(box: DeviceBox, ops: Vector[DrawOp], inverted: Boolean): String =
      val region = Seq(
        "x"      -> box.left.toString,
        "y"      -> box.top.toString,
        "width"  -> box.width.toString,
        "height" -> box.height.toString
      )
      val m = node("mask", region*)
      attributes(
        m,
        "id"        -> nextId("mask"),
        "style"     -> "mask-type:alpha",
        "maskUnits" -> "userSpaceOnUse"
      )
      val drawn = drawAll(ops, box)
      if !inverted then drawn.foreach(m.appendChild)
      else
        val filter = node(
          "filter",
          "id"          -> nextId("cut"),
          "filterUnits" -> "userSpaceOnUse",
          "x"           -> (box.left - 1).toString,
          "y"           -> (box.top - 1).toString,
          "width"       -> (box.width + 2).toString,
          "height"      -> (box.height + 2).toString
        )
        val _ =
          filter.appendChild(node("feFlood", "flood-color" -> "white", "flood-opacity" -> "1"))
        val _ =
          filter.appendChild(node("feComposite", "in2" -> "SourceGraphic", "operator" -> "out"))
        val cut = node("g", "filter" -> s"url(#${define(filter)})")
        drawn.foreach(cut.appendChild)
        val _ = m.appendChild(cut)
      define(m)
    end mask

    /** The id of a clip path of the device pixels `box`. */
    private def boxClip(box: DeviceBox): String =
      val clip = node("clipPath", "id" -> nextId("clip"))
      val _    = clip.appendChild(
        node(
          "rect",
          "x"      -> box.left.toString,
          "y"      -> box.top.toString,
          "width"  -> box.width.toString,
          "height" -> box.height.toString
        )
      )
      define(clip)

    /** The id of a clip path of the rectangle of `clip` under its affine, antialiased. */
    private def shapeClip(clip: ClipShape): String =
      val r    = clip.rect
      val path = node("clipPath", "id" -> nextId("clip"))
      val _    = path.appendChild(
        node(
          "rect",
          "x"         -> number(r.upperLeftCorner.x),
          "y"         -> number(r.upperLeftCorner.y),
          "width"     -> number(r.lowerRightCorner.x - r.upperLeftCorner.x),
          "height"    -> number(r.lowerRightCorner.y - r.upperLeftCorner.y),
          "transform" -> matrix(clip.m)
        )
      )
      define(path)

    /** The id of a clip path of the convex polygon `corners` in device pixels. */
    private def polygonClip(corners: Vector[Pos]): String =
      val path   = node("clipPath", "id" -> nextId("clip"))
      val points = corners.map(p => s"${number(p.x)},${number(p.y)}").mkString(" ")
      val _      = path.appendChild(node("polygon", "points" -> points))
      define(path)

    /** The pixels `image` draws where only the device pixels `visible` show, and the map that takes
      * them to device pixels. They are those of `smile.infrastructure.DrawPlan.imagePixels`, except
      * for a picture source, which is rendered at `density` times the density its map needs.
      */
    private def pixelsOf(
        image: DrawOp.Image,
        visible: DeviceBox
    ): Option[(NativeBuffer, Affine)] =
      val content = image.source.source match
        case BitmapSource.Content(_) => !image.replayed
        case BitmapSource.Pixels(_)  => false
      if !content || density == 1 then DrawPlan.imagePixels(image, visible, settings, timeMillis)
      else
        // The image as the plan would lay it at density, with its pixels mapped back.
        val dense  = Affine.scaling(density, density)
        val scaled = image.copy(
          m = image.m.andThen(dense),
          clip = image.clip.map(_.map(dense(_)))
        )
        val shown = DeviceGrid(density).covering(
          Bounds(
            visible.left,
            visible.top,
            visible.left + visible.width,
            visible.top + visible.height
          )
        )
        DrawPlan
          .imagePixels(scaled, shown, settings, timeMillis)
          .map((pixels, m) => (pixels, m.andThen(Affine.scaling(1 / density, 1 / density))))

    /** What draws `image` where only the device pixels `visible` show, as
      * `smile.infrastructure.CanvasPlanRenderer` draws it. Snapped pixels are drawn as they are
      * inside the clip. Resampled ones are drawn with a border that repeats their edge pixels.
      * Where the clip crops the source, only the source pixels the clip reaches are embedded (see
      * `smile.infrastructure.DrawPlan.sourceWindow`), so that whoever shows the SVG samples nothing
      * outside the crop into it.
      */
    private def drawImage(image: DrawOp.Image, visible: DeviceBox): Option[dom.Element] =
      pixelsOf(image, visible).flatMap: (pixels, m) =>
        val (width, height) = (pixels.width, pixels.height)
        if image.snapped then
          // The plan put the corner on a whole device pixel. The float error of composing the
          // affines is taken off, so that the pixels are shown as they are.
          val placed = Affine(m.a, m.b, m.c, m.d, Math.rint(m.e), Math.rint(m.f))
          val g      = node("g", "style" -> Pixelated, "image-rendering" -> "optimizeSpeed")
          image.clip.foreach(corners =>
            g.setAttribute("clip-path", s"url(#${polygonClip(corners)})")
          )
          val _ = g.appendChild(use(rasterId(pixels, None, padded = false), placed))
          Some(g)
        else
          val window           = image.clip.flatMap(DrawPlan.sourceWindow(_, m, width, height))
          val (w, h, toRaster) = window.fold((width, height, m)): window =>
            (window.width, window.height, Affine.translation(window.left, window.top).andThen(m))
          val extent = Vector((0, 0), (w, 0), (w, h), (0, h))
            .map((x, y) => toRaster(Pos(x.toDouble, y.toDouble)))
          val corners = image.clip.fold(extent): clip =>
            if toRaster.keepsAxes then clip else RasterPlan.intersectConvex(clip, extent)
          if corners.length < 3 then None
          else
            val g = node("g", "clip-path" -> s"url(#${polygonClip(corners)})")
            if DrawPlan.interpolation(image, settings) == RasterInterpolation.Nearest then
              attributes(g, "style" -> Pixelated, "image-rendering" -> "optimizeSpeed")
            val _ = g.appendChild(use(rasterId(pixels, window, padded = true), toRaster))
            Some(g)
    end drawImage

    /** The id of the definition of the pixels of `buffer` in `window`, or all of them, as an
      * `image` of them from the origin, one pixel each, with a border of their edge pixels from
      * `(-1, -1)` where they are `padded`. Each one is defined once.
      */
    private def rasterId(
        buffer: NativeBuffer,
        window: Option[DeviceBox],
        padded: Boolean
    ): String =
      rasters
        .collectFirst:
          case (b, w, p, id) if (b eq buffer) && w == window && p == padded => id
        .getOrElse:
          val box             = window.getOrElse(DeviceBox(0, 0, buffer.width, buffer.height))
          val (width, height) = (box.width, box.height)
          val url             =
            if !padded then buffer.pngDataUrl
            else if window.isEmpty then buffer.paddedPngDataUrl
            else
              val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
              canvas.width = width + 2
              canvas.height = height + 2
              val g = JSBufferAdapter.contextOf(canvas)
              JSBufferAdapter.drawPadded(g, buffer.readOnlyCanvas, box.left, box.top, width, height)
              canvas.toDataURL("image/png")
          val border = if padded then 1 else 0
          val id     = define(
            imageNode(
              nextId("image"),
              -border,
              -border,
              width + 2 * border,
              height + 2 * border,
              url
            )
          )
          rasters :+= ((buffer, window, padded, id))
          id

    /** The id of the definition of `document`, defined once. */
    private def documentId(document: SvgDocument): String =
      documents.getOrElseUpdate(
        document,
        define(
          imageNode(nextId("svg"), 0, 0, document.width, document.height, document.dataUrl)
        )
      )

    /** The id of the definition of `tile`, drawn inside the tile's box. A `vector` tile, which the
      * plan makes of a wrap whose period is on whole pixels, is written as the vectors of its
      * content. Any other tile is its pixels as `smile.infrastructure.CanvasPlanRenderer` renders
      * them, at `density` times their own.
      *
      * A tile that embeds an SVG document is written as vectors too, since an SVG document is drawn
      * onto a canvas only once the browser has decoded it.
      */
    private def tileId(tile: DrawOp.Layer, vector: Boolean): String =
      val (width, height) = (tile.box.width, tile.box.height)
      val g               = node("g", "id" -> nextId("tile"))
      if vector || embedsDocument(tile, slotEmbeds) then drawLayer(tile).foreach(g.appendChild)
      else
        val scaled = scaledBy(tile, density)
        val pixels =
          CanvasPlanRenderer.paint(
            denseSlots(tile),
            Vector(scaled),
            scaled.box,
            settings,
            timeMillis
          )
        val (rasterWidth, rasterHeight) = (scaled.box.width / density, scaled.box.height / density)
        if rasterWidth != width || rasterHeight != height then
          g.setAttribute("clip-path", s"url(#${boxClip(tile.box)})")
        val _ = g.appendChild(
          imageNode(nextId("image"), 0, 0, rasterWidth, rasterHeight, pixels.pngDataUrl)
        )
      define(g)

    /** Draws the glyphs of `layout` under `m`, laid out around the origin of their local frame, the
      * stroke behind the fill unless it is on top, as the canvas draws them.
      */
    private def drawText(
        m: Affine,
        layout: TextLayout,
        fill: Option[FillStyle],
        stroke: Option[StrokeStyle]
    ): dom.Element =
      val drop = layout.baselineDrop
      val text = node(
        "text",
        "x"              -> "0",
        "y"              -> number(drop.getOrElse(0.0)),
        "font-family"    -> FontFamily.css(layout.font),
        "font-size"      -> number(layout.size),
        "font-weight"    -> layout.weight.toString,
        "text-anchor"    -> "middle",
        "text-rendering" -> "geometricPrecision",
        "style"          -> "white-space: pre"
      )
      // Where the font's metrics cannot be had, the viewer centers the line itself.
      if drop.isEmpty then text.setAttribute("dominant-baseline", "central")
      paint(text, DrawPlan.paintOrigin(layout.layoutBox, stroke), fill, stroke)
      text.textContent = layout.content
      under(m, text)

    /** Sets the fill and stroke of `e`, gradients measured from `origin` in its local frame. */
    private def paint(
        e: dom.Element,
        origin: Pos,
        fill: Option[FillStyle],
        stroke: Option[StrokeStyle]
    ): Unit =
      e.setAttribute("fill", fill.fold("none")(style => paintValue(style.paint, origin)))
      stroke.foreach: style =>
        attributes(
          e,
          "stroke"          -> paintValue(style.paint, origin),
          "stroke-width"    -> number(style.width),
          "stroke-linecap"  -> JSPaint.cssLineCap(style.cap),
          "stroke-linejoin" -> JSPaint.cssLineJoin(style.join),
          // SVG's default limit is 4. Canvas and Java2D bevel a miter at 10.
          "stroke-miterlimit" -> number(StrokeStyle.MiterLimit)
        )
        // SVG paints the stroke over the fill, and one beneath it is painted first.
        if !style.onTop then e.setAttribute("paint-order", "stroke")

    /** `paint` as the value of a fill or a stroke, a gradient defined from `origin`. */
    private def paintValue(paint: Paint, origin: Pos): String =
      paint match
        case color: Color             => color.toString
        case gradient: LinearGradient => s"url(#${gradientId(gradient, origin)})"

    /** The id of a definition of `gradient` from `origin`, in the user space that refers to it.
      *
      * A gradient of no length is painted as its average color, as on the canvas, and since SVG
      * would paint its last stop, it is written one unit long with that color at both ends.
      */
    private def gradientId(gradient: LinearGradient, origin: Pos): String =
      val (start, anchoredEnd) = gradient.anchoredAt(origin)
      val isFlat               = LinearGradient.isTooShortToPaint(start, anchoredEnd)
      val end                  = if isFlat then Pos(start.x + 1, start.y) else anchoredEnd
      val g                    = node(
        "linearGradient",
        "id"            -> nextId("gradient"),
        "x1"            -> number(start.x),
        "y1"            -> number(start.y),
        "x2"            -> number(end.x),
        "y2"            -> number(end.y),
        "gradientUnits" -> "userSpaceOnUse"
      )
      // SVG has the cycles built in. Its default, "pad", is NoCycle.
      gradient.cycleMethod match
        case LinearGradient.CycleMethod.NoCycle => ()
        case LinearGradient.CycleMethod.Reflect => g.setAttribute("spreadMethod", "reflect")
        case LinearGradient.CycleMethod.Repeat  => g.setAttribute("spreadMethod", "repeat")
      val stops =
        if isFlat then Seq(0.0 -> gradient.averageColor, 1.0 -> gradient.averageColor)
        else gradient.stops
      for (offset, color) <- stops do
        val _ =
          g.appendChild(node("stop", "offset" -> number(offset), "stop-color" -> color.toString))
      define(g)
    end gradientId
  end Writer

  /** The style that has a raster shown with nearest-neighbor resampling. Chromium, Firefox and
    * WebKit read `pixelated` that way, and renderers that know no CSS read the presentation
    * attribute `image-rendering="optimizeSpeed"` set beside it.
    */
  private val Pixelated = "image-rendering: pixelated"

  /** Whether `op` draws an SVG document anywhere within it, given whether each slot it may draw
    * does, by the number of the slot.
    */
  private def embedsDocument(op: DrawOp, slots: Array[Boolean]): Boolean =
    def embeds(ops: Vector[DrawOp]) = ops.exists(embedsDocument(_, slots))
    op match
      case _: DrawOp.Foreign        => true
      case layer: DrawOp.Layer      => DrawOp.parts(layer).exists(embeds)
      case DrawOp.HardClip(_, ops)  => embeds(ops)
      case DrawOp.Tiles(tile, _, _) => embedsDocument(tile, slots)
      case DrawOp.Prepared(slot, _) => slots(slot)
      case _                        => false

  /** `layer` drawn `k` times as large, onto device pixels `k` times as small. These are the
    * operations of a wrap tile for a raster of it at that density.
    */
  private def scaledBy(layer: DrawOp.Layer, k: Double): DrawOp.Layer =
    if k == 1 then layer
    else
      val dense                                    = Affine.scaling(k, k)
      def box(b: DeviceBox): DeviceBox             = covering(b, k)
      def all(ops: Vector[DrawOp]): Vector[DrawOp] = ops.map(scaled)
      def scaled(op: DrawOp): DrawOp               =
        op match
          case DrawOp.Path(m, shape, fill, stroke) =>
            DrawOp.Path(m.andThen(dense), shape, fill, stroke)
          case DrawOp.Text(m, layout, fill, stroke) =>
            DrawOp.Text(m.andThen(dense), layout, fill, stroke)
          case image: DrawOp.Image =>
            image.copy(
              m = image.m.andThen(dense),
              clip = image.clip.map(_.map(dense(_))),
              // Pixels copied one to one are copied one to one only at a density of one.
              snapped = image.snapped && k == 1
            )
          case DrawOp.Foreign(document, m) => DrawOp.Foreign(document, m.andThen(dense))
          case DrawOp.Pixel(x, y, paint)   =>
            DrawOp.Path(
              Affine(k, 0, 0, k, x * k, y * k),
              UnitSquare,
              Some(FillStyle(flat(paint))),
              None
            )
          case l: DrawOp.Layer                    => layerOf(l)
          case DrawOp.HardClip(b, ops)            => DrawOp.HardClip(box(b), all(ops))
          case DrawOp.Tiles(tile, copies, vector) =>
            DrawOp.Tiles(tile, copies.map(_.andThen(dense)), vector)
          case DrawOp.Prepared(slot, region) => DrawOp.Prepared(slot, box(region))
      def layerOf(l: DrawOp.Layer): DrawOp.Layer =
        new DrawOp.Layer(
          box(l.box),
          l.clip.map(c => c.copy(m = c.m.andThen(dense))),
          l.masks.map((ops, inverted) => (all(ops), inverted)),
          all(l.ops)
        )
      layerOf(layer)
  end scaledBy

  /** The device pixels `k` times as small that cover the device pixels `box`. */
  private def covering(box: DeviceBox, k: Double): DeviceBox =
    DeviceGrid(k).covering(Bounds(box.left, box.top, box.left + box.width, box.top + box.height))

  /** The square of one pixel from the origin, as a path. */
  private val UnitSquare = PathShape(IArray(0.0, 0, 1, 0, 1, 1, 0, 1), true)

  /** The color a paint paints one pixel with, which for a gradient is its average. */
  private def flat(paint: Paint): Color =
    paint match
      case color: Color             => color
      case gradient: LinearGradient => gradient.averageColor

  /** The element of a local shape, in its local frame. */
  private def shapeElement(shape: LocalShape): dom.Element =
    shape match
      case path: PathShape =>
        val xy = path.xy
        val d  = new StringBuilder(s"M${number(xy(0))} ${number(xy(1))}")
        var i  = 2
        while i < xy.length do
          d ++= s"L${number(xy(i))} ${number(xy(i + 1))}"
          i += 2
        if path.closed then d ++= "Z"
        node("path", "d" -> d.toString)
      case arc: EllipseArcShape =>
        arcData(arc) match
          case Some(data) => node("path", "d" -> data)
          case None       =>
            node(
              "ellipse",
              "cx" -> "0",
              "cy" -> "0",
              "rx" -> number(arc.rx),
              "ry" -> number(arc.ry)
            )
      case _: PointShape =>
        node("rect", "x" -> "-0.5", "y" -> "-0.5", "width" -> "1", "height" -> "1")
      case _: TextShape => node("g")

  /** The path data of an arc that is not a whole ellipse, which
    * `smile.infrastructure.CanvasPlanRenderer` draws from too in a browser, so that both backends
    * flatten one curve. A browser flattens a canvas's `ellipse` and SVG's arc command each its own
    * way. `None` for a whole ellipse, which both backends draw as an ellipse. Drawn on a canvas
    * from this kind of data, a whole circle came out a little differently in two renderings in
    * Chromium, so that a mask and its inverse no longer added up to the content.
    */
  private[infrastructure] def arcData(arc: EllipseArcShape): Option[String] =
    Option.when(!arc.isFullCycle)(partialArcData(arc))

  /** The path data of a partial arc, a move to its start and one elliptical arc to its end. */
  private def partialArcData(arc: EllipseArcShape): String =
    // The shape's angles run counterclockwise on screen from three o'clock, measured on the
    // ellipse as if it were a circle stretched onto it, so a positive sweep runs against SVG's
    // positive-angle direction.
    def at(degrees: Double): Pos =
      val radians = degrees.toRadians
      Pos(arc.rx * math.cos(radians), -arc.ry * math.sin(radians))
    val (start, end) = (at(arc.startAngle), at(arc.startAngle + arc.arcAngle))
    val large        = if arc.arcAngle.abs > 180 then 1 else 0
    val sweep        = if arc.arcAngle > 0 then 0 else 1
    s"M${number(start.x)} ${number(start.y)}" +
      s"A${number(arc.rx)} ${number(arc.ry)} 0 $large $sweep ${number(end.x)} ${number(end.y)}"

  /** A group of `e` under `m`. */
  private def under(m: Affine, e: dom.Element): dom.Element =
    val g = node("g", "transform" -> matrix(m))
    val _ = g.appendChild(e)
    g

  /** A `use` of the definition `id` under `m`. */
  private def use(id: String, m: Affine): dom.Element =
    val e = node("use", "href" -> s"#$id", "transform" -> matrix(m))
    e.setAttributeNS(XLink, "xlink:href", s"#$id")
    e

  /** An `image` of `url` with the id `id` from `(x, y)`, `width` by `height` in its user space.
    *
    * The URL, which holds a whole document or raster, is written once, as `xlink:href`, which every
    * SVG 1.1 and SVG 2 viewer reads, where a `use` writes both forms of its short reference.
    */
  private def imageNode(
      id: String,
      x: Double,
      y: Double,
      width: Double,
      height: Double,
      url: String
  ): dom.Element =
    val e = node(
      "image",
      "id"                  -> id,
      "x"                   -> number(x),
      "y"                   -> number(y),
      "width"               -> number(width),
      "height"              -> number(height),
      "preserveAspectRatio" -> "none"
    )
    e.setAttributeNS(XLink, "xlink:href", url)
    e

  private def matrix(m: Affine): String =
    s"matrix(${number(m.a)} ${number(m.b)} ${number(m.c)} ${number(m.d)} ${number(m.e)} ${number(m.f)})"

  /** `v` as SVG writes a number, in JavaScript's shortest form that reads back as `v`. */
  private def number(v: Double): String = v.toString

  private def node(name: String, attrs: (String, String)*): dom.Element =
    val e = dom.document.createElementNS(Namespace, name)
    attributes(e, attrs*)
    e

  private def attributes(e: dom.Element, attrs: (String, String)*): Unit =
    attrs.foreach((name, value) => e.setAttribute(name, value))
end SvgPlanRenderer
