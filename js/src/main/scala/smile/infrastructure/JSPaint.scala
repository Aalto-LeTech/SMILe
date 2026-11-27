package smile.infrastructure

import org.scalajs.dom
import smile.colors.{Color, LinearGradient}
import smile.modeling.Pos
import smile.pictures.{FillStyle, StrokeStyle}
import smile.pictures.StrokeStyle.{Cap, Join}

import scala.scalajs.js

/** Conversions from SMILe's shared style and paint types to their canvas equivalents, with which
  * `smile.infrastructure.CanvasPlanRenderer` fills and strokes.
  *
  * Gradient coordinates are relative to a shape's local box (see [[smile.colors.LinearGradient]]),
  * and canvas gradients resolve in the user space the context's transform establishes, which is the
  * shape's local frame. Every entry point below therefore takes a `gradientOrigin`, the corner the
  * gradient is relative to in that frame (see `smile.infrastructure.DrawPlan.paintOrigin`).
  */
private[smile] object JSPaint:

  /** Runs the two paints of an element in the order its stroke style requests. A stroke with `onTop
    * \= false` paints first, so it ends up beneath the fill, matching the JVM renderer. The paints
    * are by-name because only the order is decided here. Canvas shapes fill and stroke a path, and
    * canvas text fills and strokes glyphs.
    */
  def inPaintOrder(line: Option[StrokeStyle])(fill: => Unit, stroke: => Unit): Unit =
    if line.exists(!_.onTop) then
      stroke
      fill
    else
      fill
      stroke

  /** Paints a path's fill and stroke in the order the stroke style requests. The path is `path`, a
    * `Path2D`, where there is one, and the context's current path otherwise.
    */
  def canvasPaint(
      ctx: dom.CanvasRenderingContext2D,
      gradientOrigin: Pos,
      fill: Option[FillStyle],
      line: Option[StrokeStyle],
      path: Option[js.Any] = None
  ): Unit =
    inPaintOrder(line)(
      canvasFill(ctx, gradientOrigin, fill, path),
      canvasStroke(ctx, gradientOrigin, line, path)
    )

  /** What a canvas paints `gradient` with in the context's current user space, the shape's local
    * frame, where the transform in effect when painting places it. It is a `CanvasGradient`, or the
    * gradient's average color as a CSS color where it is painted flat.
    *
    * @param gradientOrigin
    *   The corner the gradient's points are relative to, in the shape's local frame.
    */
  def canvasGradientStyle(
      gradient: LinearGradient,
      gradientOrigin: Pos,
      ctx: dom.CanvasRenderingContext2D
  ): js.Any =
    val (start, end) = gradient.anchoredAt(gradientOrigin)

    // A canvas paints nothing at all with a gradient of no length. Such a gradient amounts to its
    // average color, which is set as a plain color. WebKit dithers every gradient, so a gradient
    // of two equal stops would come out a step of 255 off here and there.
    def flat: js.Any = gradient.averageColor.toString

    if LinearGradient.isTooShortToPaint(start, end) then flat
    else
      // A canvas gradient only ever continues its end colors, so a repeating or reflecting
      // one gets its cycles written out as stops, across all the canvas can show. A gradient
      // that cycles too often for that is finer than the pixels and is painted flat as well.
      gradient.writtenOutOver(start, end, visibleCorners(ctx)) match
        case None                    => flat
        case Some((from, to, stops)) =>
          val canvasGradient = ctx.createLinearGradient(from.x, from.y, to.x, to.y)
          stops.foreach((offset, color) => canvasGradient.addColorStop(offset, color.toString))
          canvasGradient
  end canvasGradientStyle

  /** The corners of the context's canvas in its current user space, the space gradient points
    * resolve in. Whatever a paint reaches lies between them, however the element is transformed and
    * however far its stroke or its glyphs reach. Empty when the transform is degenerate and nothing
    * can be painted at all.
    */
  private def visibleCorners(ctx: dom.CanvasRenderingContext2D): Seq[Pos] =
    val transform           = ctx.asInstanceOf[js.Dynamic].getTransform()
    def entry(name: String) = transform.selectDynamic(name).asInstanceOf[Double]
    val (a, b, c, d, e, f)  =
      (entry("a"), entry("b"), entry("c"), entry("d"), entry("e"), entry("f"))
    val determinant = a * d - b * c

    if determinant == 0 || determinant.isNaN then Seq.empty
    else
      val (width, height) = (ctx.canvas.width.toDouble, ctx.canvas.height.toDouble)
      for (x, y) <- Seq((0.0, 0.0), (width, 0.0), (0.0, height), (width, height))
      yield Pos(
        (d * (x - e) - c * (y - f)) / determinant,
        (a * (y - f) - b * (x - e)) / determinant
      )

  def canvasFill(
      ctx: dom.CanvasRenderingContext2D,
      gradientOrigin: Pos,
      style: Option[FillStyle],
      path: Option[js.Any] = None
  ): Unit =
    style match
      case None        =>
      case Some(style) =>
        style.paint match
          case color: Color             => ctx.fillStyle = color.toString
          case gradient: LinearGradient =>
            ctx.fillStyle = canvasGradientStyle(gradient, gradientOrigin, ctx)
        path match
          case Some(p) => val _ = ctx.asInstanceOf[js.Dynamic].fill(p)
          case None    => ctx.fill()

  def canvasStroke(
      ctx: dom.CanvasRenderingContext2D,
      gradientOrigin: Pos,
      style: Option[StrokeStyle],
      path: Option[js.Any] = None
  ): Unit =
    style match
      case None        =>
      case Some(style) =>
        style.paint match
          case color: Color             => ctx.strokeStyle = color.toString
          case gradient: LinearGradient =>
            ctx.strokeStyle = canvasGradientStyle(gradient, gradientOrigin, ctx)
        applyCanvasStrokeGeometry(ctx, style)
        path match
          case Some(p) => val _ = ctx.asInstanceOf[js.Dynamic].stroke(p)
          case None    => ctx.stroke()

  /** Applies a stroke's width, cap and join to the canvas context. The paint is set separately,
    * because paths and glyphs anchor their gradients in different frames. The context is shared
    * across an entire picture, so every stroked draw has to set all three.
    */
  def applyCanvasStrokeGeometry(ctx: dom.CanvasRenderingContext2D, style: StrokeStyle): Unit =
    ctx.lineWidth = style.width
    ctx.lineCap = cssLineCap(style.cap)
    ctx.lineJoin = cssLineJoin(style.join)
    ctx.miterLimit = StrokeStyle.MiterLimit

  def cssLineCap(cap: Cap): String = cap match
    case Cap.Butt   => "butt"
    case Cap.Round  => "round"
    case Cap.Square => "square"

  def cssLineJoin(join: Join): String = join match
    case Join.Bevel => "bevel"
    case Join.Round => "round"
    case Join.Miter => "miter"
