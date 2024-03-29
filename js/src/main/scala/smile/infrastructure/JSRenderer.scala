package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.{CanvasGradient, html}
import scalatags.JsDom.all.bindNode
import scalatags.JsDom.implicits.{doubleAttr, intAttr, stringAttr, stringFrag}
import scalatags.JsDom.svgAttrs.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.{Modifier, svgAttrs, svgTags}
import smile.colors.{Color, LinearGradient}
import smile.infrastructure
import smile.modeling.{Angle, BoundaryCalculator, Bounds, Pos}
import smile.pictures.*
import smile.pictures.StrokeStyle.{Cap, Join}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Provides functionality for rendering pictures and their elements into bitmaps and SVGs.
  */
@JSExportTopLevel("Renderer")
object JSRenderer extends Renderer:
  private def bitmapToPng(bitmap: Bitmap): String =
    bitmap.buffer.get.asInstanceOf[dom.html.Canvas].toDataURL("image/png")

  private def pictureBounds(picture: Picture) =
    val bounds = picture.viewport match
      case Some(viewport) => viewport.boundary
      case None           => BoundaryCalculator.fromBoundaries(picture.elements)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    val (xOffsetToOrigin, yOffsetToOrigin) =
      val upperLeftCorner = bounds.upperLeftCorner

      val xOffset = -upperLeftCorner.x
      val yOffset = -upperLeftCorner.y

      (xOffset, yOffset)

    (bounds, flooredWidth, flooredHeight, xOffsetToOrigin, yOffsetToOrigin)

  /*
  /** Creates a bitmap representation of a sequence of picture elements.
   *
   * @param elements
   *   A sequence of [[PictureElement]] instances to be rendered into a bitmap.
   * @return
   *   A [[Bitmap]] instance representing the rendered picture elements.
   */
   */
  def createBitmapFrom(elements: PictureElement*): Bitmap = createBitmapFrom(new Picture(elements))

  /** Creates a bitmap from a given picture.
    *
    * @param picture
    *   The [[smile.pictures.Picture]] to be rendered into a bitmap.
    * @return
    *   A [[smile.pictures.Bitmap]] representing the rendered picture.
    */
  def createBitmapFrom(picture: Picture): Bitmap =
    val (bounds, flooredWidth, flooredHeight, xOffsetToOrigin, yOffsetToOrigin) = pictureBounds(
      picture
    )

    if flooredWidth < 1 || flooredHeight < 1 then return Bitmap.Empty

    val buffer = JSBufferAdapter(flooredWidth, flooredHeight)

    for element <- picture.elements.reverse do
      renderElementToCanvas(element, xOffsetToOrigin, yOffsetToOrigin, buffer.ctx)

    new Bitmap(buffer, bounds)
  end createBitmapFrom

  @JSExport
  def createSvgFrom(element: PictureElement): dom.Element = createSvgFrom(new Picture(element))

  /** Creates an SVG representation of a sequence of picture elements.
    *
    * @param picture
    *   The [[smile.pictures.Picture]] to be rendered into an SVG.
    * @return
    *   An SVG element representing the rendered picture elements.
    */
  @JSExport
  def createSvgFrom(picture: Picture): dom.Element =
    val (bounds, flooredWidth, flooredHeight, xOffsetToOrigin, yOffsetToOrigin) = pictureBounds(
      picture
    )

    if flooredWidth < 1 || flooredHeight < 1 then return svg().render

    require(flooredWidth > 0 && flooredHeight > 0, "Bitmap width and height must be positive")

    val masks = picture.elements
      .collect:
        case mask: MaskGroup => mask
      .map: mask =>
        val maskId = s"mask-${mask.mask.hashCode()}"
        svgTags
          .mask(
            id := maskId,
            renderElement(mask.mask, 0, 0)._1
          )
          .render

    val (elements, gradients) =
      picture.elements.reverse
        .map: element =>
          renderElement(element, xOffsetToOrigin, yOffsetToOrigin)
        .unzip

    svg(
      xmlns      := "http://www.w3.org/2000/svg",
      xmlnsXlink := "http://www.w3.org/1999/xlink",
      width      := bounds.width.inPixels,
      height     := bounds.height.inPixels,
      viewBox    := s"0 0 ${bounds.width.inPixels} ${bounds.height.inPixels}",
      style := "user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none;",
      defs(gradients.flatten.distinctBy(_.id), masks),
      elements
    ).render

  /** Creates a string representation of an SVG element from a sequence of picture elements.
    *
    * @param picture
    *   The [[smile.pictures.Picture]] to be rendered into an SVG string.
    *
    * @return
    *   A string representing the rendered picture elements in SVG format.
    */
  @JSExport
  def svgString(picture: Picture): String = createSvgFrom(picture).outerHTML

  /** Renders an individual element as an SVG element.
    *
    * @param contentItem
    *   The [[PictureElement]] to be rendered.
    * @param xOffsetToOrigin
    *   The X offset to the origin point for rendering.
    * @param yOffsetToOrigin
    *   The Y offset to the origin point for rendering.
    *
    * @return
    *   A tuple containing the SVG element and any gradient elements used in the rendering.
    */
  private def renderElement(
      contentItem: PictureElement,
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double
  ): (dom.Element, Seq[dom.Element]) =
    contentItem match
      case animatedPicture: AnimatedPicture =>
        renderElement(
          animatedPicture.frames(animatedPicture.currentFrameIndex),
          xOffsetToOrigin,
          yOffsetToOrigin
        )
      case maskGroup: MaskGroup =>
        val (elements, gradients) = maskGroup.masked.elements.reverse
          .map: el =>
            renderElement(el, xOffsetToOrigin, yOffsetToOrigin)
          .unzip
        (
          g(
            svgAttrs.mask := s"url(#mask-${maskGroup.mask.hashCode()})",
            elements
          ).render,
          gradients.flatten
        )
      case arc: Arc =>
        makeArc(
          xOffsetToOrigin,
          yOffsetToOrigin,
          arc
        )
      case bitmap: Bitmap =>
        makeBitmap(
          xOffsetToOrigin,
          yOffsetToOrigin,
          bitmap
        )
      case polygon: Polygon =>
        makePolygon(
          xOffsetToOrigin,
          yOffsetToOrigin,
          polygon
        )
      case text: Text =>
        makeText(
          xOffsetToOrigin,
          yOffsetToOrigin,
          text
        )

  end renderElement

  private def renderElementToCanvas(
      contentItem: PictureElement,
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      ctx: dom.CanvasRenderingContext2D
  ): Unit =
    contentItem match
      case animatedPicture: AnimatedPicture =>
        renderElementToCanvas(
          animatedPicture.frames(animatedPicture.currentFrameIndex),
          xOffsetToOrigin,
          yOffsetToOrigin,
          ctx
        )
      case maskGroup: MaskGroup =>
        val tempBufferAdapter = JSBufferAdapter(
          maskGroup.mask.boundary.width.inPixels.toInt,
          maskGroup.mask.boundary.height.inPixels.toInt
        )
        val tempCtx = tempBufferAdapter.ctx
        renderElementToCanvas(maskGroup.mask, 0, 0, tempCtx)
        tempCtx.globalCompositeOperation = "source-in"
        maskGroup.masked.elements.reverse
          .foreach: el =>
            renderElementToCanvas(el, xOffsetToOrigin, yOffsetToOrigin, tempCtx)
        ctx.drawImage(tempBufferAdapter.get, 0, 0)
      case arc: Arc =>
        drawArc(
          xOffsetToOrigin,
          yOffsetToOrigin,
          arc,
          ctx
        )
      case bitmap: Bitmap =>
        drawBitmap(
          xOffsetToOrigin,
          yOffsetToOrigin,
          bitmap,
          ctx
        )
      case polygon: Polygon =>
        drawPolygon(
          xOffsetToOrigin,
          yOffsetToOrigin,
          polygon,
          ctx
        )
      case text: Text =>
        drawText(
          xOffsetToOrigin,
          yOffsetToOrigin,
          text,
          ctx
        )
  end renderElementToCanvas

  private def bitmapData(bitmap: Bitmap, xOffsetToOrigin: Double, yOffsetToOrigin: Double) =
    val centerX = bitmap.position.x + xOffsetToOrigin - bitmap.buffer.width / 2
    val centerY = bitmap.position.y + yOffsetToOrigin - bitmap.buffer.height / 2

    (centerX, centerY)

  private def drawBitmap(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      bitmap: Bitmap,
      ctx: dom.CanvasRenderingContext2D
  ): Unit =
    val (centerX, centerY) = bitmapData(bitmap, xOffsetToOrigin, yOffsetToOrigin)

    ctx.drawImage(
      bitmap.buffer.asInstanceOf[infrastructure.JSBufferAdapter].get,
      centerX,
      centerY,
      bitmap.buffer.width,
      bitmap.buffer.height
    )

  private def makeBitmap(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      bitmap: Bitmap
  ) =
    val (centerX, centerY) = bitmapData(bitmap, xOffsetToOrigin, yOffsetToOrigin)
    (
      svgTags
        .image(
          xLinkHref := bitmapToPng(bitmap),
          x         := centerX,
          y         := centerY,
          width     := bitmap.buffer.width,
          height    := bitmap.buffer.height
        )
        .render,
      Seq()
    )

  private def arcData(element: Arc, xOffsetToOrigin: Double, yOffsetToOrigin: Double) =
    val centerX = element.position.x + xOffsetToOrigin
    val centerY = element.position.y + yOffsetToOrigin
    val radiusX = element.width / 2
    val radiusY = element.height / 2

    (centerX, centerY, radiusX, radiusY)
  end arcData

  private def drawArc(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Arc,
      ctx: dom.CanvasRenderingContext2D
  ): Unit =
    val (centerX, centerY, radiusX, radiusY) = arcData(element, xOffsetToOrigin, yOffsetToOrigin)

    ctx.beginPath()
    if element.arcAngle == Angle.FullAngleInDegrees then
      ctx.ellipse(centerX, centerY, radiusX, radiusY, 0, 0, 2 * Math.PI)
    else
      val startAngle = -(element.startAngle + element.arcAngle) % 360
      val endAngle   = startAngle + element.arcAngle - 0.0001

      val largeArc = endAngle - startAngle <= 180

      ctx.ellipse(
        centerX,
        centerY,
        radiusX,
        radiusY,
        0,
        startAngle.toRadians,
        endAngle.toRadians,
        !largeArc
      )
    canvasFill(ctx, element, element.fillStyle)
    canvasStroke(ctx, element, element.strokeStyle)
  end drawArc

  private def makeArc(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Arc
  ) =
    val (centerX, centerY, radiusX, radiusY) = arcData(element, xOffsetToOrigin, yOffsetToOrigin)

    val (fill, stroke, gradients) = makeStyle(element)

    if element.arcAngle == Angle.FullAngleInDegrees then
      (
        ellipse(
          cx := centerX,
          cy := centerY,
          rx := radiusX,
          ry := radiusY,
          fill,
          stroke
        ).render,
        gradients
      )
    else
      def polarToCartesian(angleInDegrees: Double): Pos =
        val angleInRadians = angleInDegrees.toRadians

        Pos(
          centerX + radiusX * MathUtils.cosRads(angleInRadians),
          centerY - radiusY * MathUtils.sinRads(angleInRadians)
        )
      end polarToCartesian

      val startAngle = element.startAngle
      val endAngle   = startAngle + element.arcAngle - 0.0001

      val start = polarToCartesian(startAngle)
      val end   = polarToCartesian(endAngle)

      val largeArcFlag = if endAngle - startAngle <= 180 then "0" else "1"

      val pathData =
        s"M ${start.x} ${start.y} A $radiusX $radiusY 0 $largeArcFlag 0 ${end.x} ${end.y}"
      (
        path(
          d := pathData,
          transform := s"rotate(${element.rotationAngle}, ${centerX - radiusX}, ${centerY - radiusY})",
          fill,
          stroke
        ).render,
        gradients
      )
  end makeArc

  private def polygonData(element: Polygon) =
    val (xs, ys) = element.points.unzip(p => (p.x, p.y))

    val centerX = element.position.x
    val centerY = element.position.y

    (xs, ys, centerX, centerY)
  end polygonData

  private def drawPolygon(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Polygon,
      ctx: dom.CanvasRenderingContext2D
  ): Unit =
    val (xs, ys, centerX, centerY) = polygonData(element)

    ctx.beginPath()
    ctx.moveTo(xs.head + centerX + xOffsetToOrigin, ys.head + centerY + yOffsetToOrigin)
    xs.tail
      .zip(ys.tail)
      .foreach((x, y) => ctx.lineTo(x + centerX + xOffsetToOrigin, y + centerY + yOffsetToOrigin))
    ctx.closePath()

    canvasFill(ctx, element, element.fillStyle)
    canvasStroke(ctx, element, element.strokeStyle)
  end drawPolygon

  private def makePolygon(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Polygon
  ) =
    val (xs, ys, centerX, centerY) = polygonData(element)

    val (fill, stroke, gradients) = makeStyle(element)
    (
      polygon(
        points    := xs.zip(ys).map((x, y) => s"$x,$y").mkString(" "),
        transform := s"translate(${centerX + xOffsetToOrigin}, ${centerY + yOffsetToOrigin})",
        fill,
        stroke
      ).render,
      gradients
    )
  end makePolygon

  private def drawText(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Text,
      ctx: dom.CanvasRenderingContext2D
  ): Unit =
    val bounds  = element.boundary
    val centerX = element.position.x + xOffsetToOrigin + bounds.width.inPixels / 2
    val centerY = element.position.y + yOffsetToOrigin + bounds.height.inPixels

    ctx.font = s"""${element.weight} ${element.size}px "${element.typeface}""""
    ctx.textAlign = "center"
    ctx.textBaseline = "ideographic"
    element.fillStyle match
      case Some(fillStyle: FillStyle) =>
        ctx.fillStyle = fillStyle.paint.averageColor.toString
        ctx.fillText(element.content, centerX, centerY)
      case _ =>
    element.strokeStyle match
      case Some(strokeStyle: StrokeStyle) =>
        ctx.strokeStyle = strokeStyle.paint.averageColor.toString
        ctx.strokeText(element.content, centerX, centerY)
      case _ =>

  end drawText

  private def makeText(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Text
  ) =
    val centerX = element.position.x + xOffsetToOrigin
    val centerY = element.position.y + yOffsetToOrigin

    val (fill, stroke, gradients) = makeStyle(element)

    (
      text(
        x                := centerX,
        y                := centerY + element.boundary.height.inPixels,
        fontFamily       := s""""${element.typeface}"""",
        fontWeight       := element.weight,
        fontSize         := element.size,
        dominantBaseline := "ideographic",
        fill,
        stroke,
        element.content
      ).render,
      gradients
    )
  end makeText

  private def makeStyle(vectorGraphic: VectorGraphic) =
    val (fill, fillGradient) = svgFill(vectorGraphic.fillStyle)
    val (stroke, strokeGradient) =
      vectorGraphic.strokeStyle
        .map(style => svgStroke(vectorGraphic, style))
        .getOrElse((Seq.empty, None))
    val gradients = fillGradient.toSeq ++ strokeGradient.toSeq

    (fill, stroke, gradients)
  end makeStyle

  private def canvasLinearGradient(
      gradient: LinearGradient,
      element: DrawableElement,
      ctx: dom.CanvasRenderingContext2D
  ): CanvasGradient =
    val canvasGradient = ctx.createLinearGradient(
      element.boundary.upperLeftCorner.x - gradient.start.x,
      element.boundary.upperLeftCorner.y - gradient.start.y,
      element.boundary.lowerRightCorner.x + gradient.end.x,
      element.boundary.lowerRightCorner.y + gradient.end.y
    )

    gradient.fractions
      .zip(gradient.colors)
      .foreach((fraction, color) => canvasGradient.addColorStop(fraction, color.toString))

    canvasGradient

  private def svgLinearGradient(gradient: LinearGradient): (dom.Element, String) =
    val idString = s"gradient-${gradient.hashCode()}"
    val stops = gradient.fractions
      .zip(gradient.colors)
      .map((fraction, color) =>
        stop(
          offset    := s"${(fraction * 100).toInt}%",
          stopColor := color.toString
        )
      )

    (
      linearGradient(
        id            := idString,
        x1            := gradient.start.x,
        y1            := gradient.start.y,
        x2            := gradient.end.x,
        y2            := gradient.end.y,
        gradientUnits := "userSpaceOnUse",
        stops
      ).render,
      idString
    )

  private def svgFill(
      styleOption: Option[FillStyle]
  ): (Seq[Modifier], Option[dom.Element]) =
    styleOption match
      case Some(style) =>
        style.paint match
          case color: Color => (Seq(svgAttrs.fill := color.toString), None)
          case gradient: LinearGradient =>
            val (gradientTag, id) = svgLinearGradient(gradient)
            (Seq(svgAttrs.fill := s"url(#$id)"), Some(gradientTag))
      case None => (Seq(svgAttrs.fill := "none"), None)

  private case class CssStroke(
      color: Option[String],
      svgGradientElement: Option[dom.Element],
      canvasGradient: Option[CanvasGradient],
      width: Double,
      linecap: String,
      linejoin: String
  )

  private def canvasFill(
      ctx: dom.CanvasRenderingContext2D,
      element: DrawableElement,
      style: Option[FillStyle]
  ): Unit =
    style match
      case None =>
      case Some(style) =>
        style.paint match
          case color: Color => ctx.fillStyle = color.toString
          case gradient: LinearGradient =>
            val canvasGradient = canvasLinearGradient(gradient, element, ctx)
            ctx.fillStyle = canvasGradient
        ctx.fill()

  private def canvasStroke(
      ctx: dom.CanvasRenderingContext2D,
      element: DrawableElement,
      style: Option[StrokeStyle]
  ): Unit =
    style match
      case None =>
      case Some(style) =>
        val css = cssStroke(style, element, Some(ctx))
        style.paint match
          case color: Color => ctx.strokeStyle = color.toString
          case _: LinearGradient =>
            css.canvasGradient.foreach(ctx.strokeStyle = _)
        ctx.lineWidth = style.width
        ctx.lineCap = css.linecap
        ctx.lineJoin = css.linejoin
        ctx.stroke()

  private def cssStroke(
      style: StrokeStyle,
      element: DrawableElement,
      ctx: Option[dom.CanvasRenderingContext2D]
  ): CssStroke =
    val linecap = style.cap match
      case Cap.Butt   => "butt"
      case Cap.Round  => "round"
      case Cap.Square => "square"
    val join = style.join match
      case Join.Bevel => "bevel"
      case Join.Round => "round"
      case Join.Miter => "miter"
    val (color, gradientElement, canvasGradient)
        : (Option[String], Option[dom.Element], Option[CanvasGradient]) =
      style.paint match
        case color: Color => (Some(color.toString), None, None)
        case gradient: LinearGradient =>
          ctx match
            case None =>
              val (gradientTag, id) = svgLinearGradient(gradient)
              (Some(s"url(#$id)"), Some(gradientTag), None)
            case Some(ctx) =>
              val canvasGradient = canvasLinearGradient(gradient, element, ctx)
              (None, None, Some(canvasGradient))

    CssStroke(color, gradientElement, canvasGradient, style.width, linecap, join)

  private def svgStroke(
      element: VectorGraphic,
      style: StrokeStyle
  ): (Seq[Modifier], Option[dom.Element]) =
    val css = cssStroke(style, element, None)

    (
      Seq(
        svgAttrs.stroke := css.color.get,
        strokeWidth     := css.width,
        strokeLinecap   := css.linecap,
        strokeLinejoin  := css.linejoin
      ),
      css.svgGradientElement
    )
