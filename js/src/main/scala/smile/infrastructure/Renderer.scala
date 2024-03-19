package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.{Element, window}
import scalatags.JsDom.all.{bindNode, img, src}
import scalatags.JsDom.implicits.{doubleAttr, intAttr, stringAttr}
import scalatags.JsDom.svgAttrs.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.{Modifier, svgTags}
import smile.colors.{Color, LinearGradient}
import smile.modeling.BoundaryCalculator
import smile.pictures.*
import smile.pictures.StrokeStyle.{Cap, Join}

import scala.annotation.tailrec
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Provides functionality for rendering pictures and their elements into bitmaps.
  */
@JSExportTopLevel("Renderer")
object Renderer:
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
    *   The [[Picture]] to be rendered into a bitmap.
    * @return
    *   A [[Bitmap]] representing the rendered picture.
    */
  def createBitmapFrom(picture: Picture): Bitmap =
    if picture.elements.isEmpty then return Bitmap.Empty
    val bounds = picture.viewport match
      case Some(viewport) => viewport.boundary
      case None           => BoundaryCalculator.fromBoundaries(picture.elements)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    val buffer = new BufferAdapter(flooredWidth, flooredHeight)

    val svg = createSvgFrom(picture)

    val image = img(
      src := s"data:image/svg+xml,${svg.innerHTML}"
    ).render
    
    image.onloadedmetadata = _ =>
      window.console.log("Image loaded")
      buffer.ctx.drawImage(image, 0, 0)
    

    new Bitmap(buffer, bounds)
  end

  @JSExport
  def createSvgFrom(element: PictureElement): Element = createSvgFrom(new Picture(element))

  @JSExport
  def createSvgFrom(picture: Picture): Element =
    window.console.log("createSvgFrom") 
    val bounds = picture.viewport match
      case Some(viewport) => viewport.boundary
      case None           => BoundaryCalculator.fromBoundaries(picture.elements)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    if flooredWidth < 1 || flooredHeight < 1 then return svg().render

    require(flooredWidth > 0 && flooredHeight > 0, "Bitmap width and height must be positive")

    val (xOffsetToOrigin, yOffsetToOrigin) =
      val upperLeftCorner = bounds.upperLeftCorner

      val xOffset = -upperLeftCorner.x
      val yOffset = -upperLeftCorner.y

      (xOffset, yOffset)

    val (elements, gradients) =
      picture.elements.reverse.map(renderElement(_, xOffsetToOrigin, yOffsetToOrigin)).unzip

    svg(
      xmlns  := "http://www.w3.org/2000/svg",
      width  := bounds.width.inPixels,
      height := bounds.height.inPixels,
      defs(gradients.flatten),
      elements
    ).render

  @JSExport
  def svgString(picture: Picture) = createSvgFrom(picture).outerHTML
  

  /** Renders an individual element onto a target drawing surface.
    *
    * @param contentItem
    *   The [[PictureElement]] to be rendered.
    * @param xOffsetToOrigin
    *   The X offset to the origin point for rendering.
    * @param yOffsetToOrigin
    *   The Y offset to the origin point for rendering.
    */
  @tailrec
  private def renderElement(
      contentItem: PictureElement,
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double
  ): (Element, Seq[Element]) =
    contentItem match
      case animatedPicture: AnimatedPicture =>
        renderElement(
          animatedPicture.frames(animatedPicture.currentFrameIndex),
          xOffsetToOrigin,
          yOffsetToOrigin
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
//      case point: Point =>
//        makePoint(
//          xOffsetToOrigin,
//          yOffsetToOrigin,
//          point.position.x,
//          point.position.y,
//          point.color
//        )
      case polygon: Polygon =>
        makePolygon(
          xOffsetToOrigin,
          yOffsetToOrigin,
          polygon
        )
//      case text: Text =>
//        makeText(
//          xOffsetToOrigin,
//          yOffsetToOrigin,
//          text.customBounds.isDefined,
//          text.boundary.width.inPixels,
//          text.boundary.height.inPixels,
//          text.position.x,
//          text.position.y,
//          text.content,
//          text.font,
//          text.fillStyle,
//          text.strokeStyle
//        )
      case _ => throw new UnsupportedOperationException("Unsupported picture element")

  end renderElement

  private def makeBitmap(xOffsetToOrigin: Double, yOffsetToOrigin: Double, bitmap: Bitmap) =
    val centerX = bitmap.position.x + xOffsetToOrigin
    val centerY = bitmap.position.y + yOffsetToOrigin

    (
      image(
        src    := bitmap.buffer.toPNG,
        x      := centerX,
        y      := centerY,
        width  := bitmap.buffer.width,
        height := bitmap.buffer.height
      ).render,
      Seq()
    )

  private def makeArc(xOffsetToOrigin: Double, yOffsetToOrigin: Double, shape: Arc) =
    val pos           = shape.position
    val width         = shape.width
    val height        = shape.height
    val startAngle    = shape.startAngle
    val arcAngle      = shape.arcAngle
    val rotationAngle = shape.rotationAngle

    val centerX = pos.x + xOffsetToOrigin
    val centerY = pos.y + yOffsetToOrigin

    // Calculate the radii and center
    val rx = width / 2
    val ry = height / 2
    val cx = 0
    val cy = 0

    // Calculate the start and end points of the arc
    val startRadians = Math.toRadians(startAngle)
    val endRadians   = Math.toRadians(startAngle + arcAngle)

    // SVG path command expects the rotation to be applied around the (0,0) point
    // The transform attribute can then be used to rotate the arc as needed
    val pathData =
      f"M ${cx + rx * Math.cos(startRadians)}%.2f ${cy + ry * Math.sin(startRadians)}%.2f " +
        f"A $rx%.2f $ry%.2f  0 ${if arcAngle > 180 then 1 else 0} ${
            if arcAngle > 0 then 1 else 0
          } " +
        f"${cx + rx * Math.cos(endRadians)}%.2f ${cy + ry * Math.sin(endRadians)}%.2f"

    val (fill, fillGradient)     = svgFill(shape.fillStyle)
    val (stroke, strokeGradient) = shape.strokeStyle.map(svgStroke).getOrElse((Seq.empty, None))

    val gradients = fillGradient.toSeq ++ strokeGradient.toSeq

    (
      path(
        d         := pathData,
        transform := s"translate(${centerX}, ${centerY}) rotate(${rotationAngle + 180}, 0, 0)",
        fill,
        stroke
      ).render,
      gradients
    )

  private def makePolygon(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      shape: Polygon
  ) =
    val (xs, ys) = shape.points.unzip(p => (p.x, p.y))

    val centerX = shape.position.x + xOffsetToOrigin
    val centerY = shape.position.y + yOffsetToOrigin

    val (fill, fillGradient)     = svgFill(shape.fillStyle)
    val (stroke, strokeGradient) = shape.strokeStyle.map(svgStroke).getOrElse((Seq.empty, None))

    val gradients = fillGradient.toSeq ++ strokeGradient.toSeq

    (
      polygon(
        points    := xs.zip(ys).map((x, y) => s"$x,$y").mkString(" "),
        transform := s"translate(${centerX}, ${centerY})",
        fill,
        stroke
      ).render,
      gradients
    )

  private def svgLinearGradient(gradient: LinearGradient): (Element, String) =
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
        id := idString,
        stops
      ).render,
      idString
    )

  private def svgFill(
      styleOption: Option[FillStyle]
  ): (Seq[Modifier], Option[Element]) =
    styleOption match
      case Some(style) =>
        style.paint match
          case color: Color => (Seq(fill := color.toString), None)
          case gradient: LinearGradient =>
            val (gradientTag, id) = svgLinearGradient(gradient)
            (Seq(fill := s"url(#$id)"), Some(gradientTag))
      case None => (Seq(fill := "none"), None)

  private def svgStroke(style: StrokeStyle): (Seq[Modifier], Option[Element]) =
    val linecap = style.cap match
      case Cap.Butt   => "butt"
      case Cap.Round  => "round"
      case Cap.Square => "square"
    val join = style.join match
      case Join.Bevel => "bevel"
      case Join.Round => "round"
      case Join.Miter => "miter"
    val (strokeString, gradient) = style.paint match
      case color: Color => (color.toString, None)
      case gradient: LinearGradient =>
        val (gradientTag, id) = svgLinearGradient(gradient)
        (s"url(#$id)", Some(gradientTag))

    (
      Seq(
        stroke         := strokeString,
        strokeWidth    := style.width,
        strokeLinecap  := linecap,
        strokeLinejoin := join
      ),
      gradient
    )
