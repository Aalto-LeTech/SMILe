package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all.bindNode
import scalatags.JsDom.implicits.{doubleAttr, intAttr, stringAttr, stringFrag}
import scalatags.JsDom.svgAttrs.*
import scalatags.JsDom.svgTags.*
import scalatags.JsDom.{Modifier, svgTags}
import smile.colors.{Color, LinearGradient}
import smile.modeling.{Angle, BoundaryCalculator, Bounds, Pos}
import smile.pictures.*
import smile.pictures.StrokeStyle.{Cap, Join}

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/** Provides functionality for rendering pictures and their elements into bitmaps.
  */
@JSExportTopLevel("Renderer")
object JSRenderer extends Renderer:
  private def bitmapToPng(bitmap: Bitmap): String =
    bitmap.buffer.get.asInstanceOf[dom.html.Canvas].toDataURL("image/png")

  private def pictureBounds(picture: Picture): Bounds =
    picture.viewport match
      case Some(viewport) => viewport.boundary
      case None           => BoundaryCalculator.fromBoundaries(picture.elements)

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
    val viewportPic = picture.withContentBoundaryAsViewport
    val bounds      = pictureBounds(viewportPic)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    val buffer = JSBufferAdapter(flooredWidth, flooredHeight)

    val canvg = typings.canvg.mod.Canvg

    val picturesAndBitmaps: Seq[Picture | Seq[PictureElement]] =
      def areSameType(a: PictureElement, b: PictureElement): Boolean = (a, b) match
        case (_: Bitmap, _: Bitmap)               => true
        case (_: VectorGraphic, _: VectorGraphic) => true
        case _                                    => false
      picture.elements.reverse
        .foldRight(Seq[Seq[PictureElement]]()): (elem, acc) =>
          acc match
            case (head: Seq[PictureElement]) +: tail if areSameType(head.head, elem) =>
              (elem +: head) +: tail
            case _ => Seq(elem) +: acc
        .map(el =>
          el.head match
            case _: Bitmap        => el
            case _: VectorGraphic => Picture(el, viewportPic.viewport)
        )
    end picturesAndBitmaps

    println(picturesAndBitmaps)

    for group <- picturesAndBitmaps do
      group match
        case picture: Picture =>
          val svg = createSvgFrom(picture, reverse = false)
          println(svg.outerHTML)
          val tempBuffer = JSBufferAdapter(flooredWidth, flooredHeight)
          canvg.fromString(tempBuffer.ctx, svg.outerHTML).start()
          buffer.ctx.drawImage(
            tempBuffer.get,
            0,
            0,
            flooredWidth,
            flooredHeight
          )
        case _ =>
          group.asInstanceOf[Seq[Bitmap]].foreach { bitmap =>
            val currentBuffer = bitmap.buffer
            buffer.ctx.drawImage(
              currentBuffer
                .asInstanceOf[JSBufferAdapter]
                .get,
              0,
              bitmap.position.y - bitmap.buffer.height / 2,
              currentBuffer.width,
              currentBuffer.height
            )
          }

    new Bitmap(buffer, bounds)
  end createBitmapFrom

  @JSExport
  def createSvgFrom(element: PictureElement): dom.Element = createSvgFrom(new Picture(element))

  @JSExport
  def createSvgFrom(picture: Picture, reverse: Boolean = true): dom.Element =
    val bounds = pictureBounds(picture)

    println(picture.viewport)
    println(bounds)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    if flooredWidth < 1 || flooredHeight < 1 then return svg().render

    require(flooredWidth > 0 && flooredHeight > 0, "Bitmap width and height must be positive")

    val (xOffsetToOrigin, yOffsetToOrigin) =
      val upperLeftCorner = bounds.upperLeftCorner

      val xOffset = -upperLeftCorner.x
      val yOffset = -upperLeftCorner.y

      (xOffset, yOffset)

    println(s"X: $xOffsetToOrigin, Y: $yOffsetToOrigin")

    val (elements, gradients) =
      (if reverse then picture.elements.reverse else picture.elements)
        .map(renderElement(_, xOffsetToOrigin, yOffsetToOrigin))
        .unzip

    svg(
      xmlns      := "http://www.w3.org/2000/svg",
      xmlnsXlink := "http://www.w3.org/1999/xlink",
      width      := bounds.width.inPixels,
      height     := bounds.height.inPixels,
      viewBox := s"${-xOffsetToOrigin} ${-yOffsetToOrigin} ${bounds.width.inPixels} ${bounds.height.inPixels}",
      style := "user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none;",
      defs(gradients.flatten),
      elements
    ).render

  @JSExport
  def svgString(picture: Picture): String = createSvgFrom(picture).outerHTML

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
  ): (dom.Element, Seq[dom.Element]) =
    val xOffsetToOrigin = 0
    val yOffsetToOrigin = 0
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

  private def makeBitmap(xOffsetToOrigin: Double, yOffsetToOrigin: Double, bitmap: Bitmap) =
    val centerX = bitmap.position.x + xOffsetToOrigin - bitmap.buffer.width / 2
    val centerY = bitmap.position.y + yOffsetToOrigin - bitmap.buffer.height / 2
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

  private def makeArc(xOffsetToOrigin: Double, yOffsetToOrigin: Double, element: Arc) =
    val centerX = element.position.x + xOffsetToOrigin
    val centerY = element.position.y + yOffsetToOrigin
    val radiusX = element.width / 2
    val radiusY = element.height / 2

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
          (centerX) + radiusX * MathUtils.cosRads(angleInRadians),
          (centerY) - radiusY * MathUtils.sinRads(angleInRadians)
        )
      end polarToCartesian

      val startAngle = element.startAngle
      val endAngle   = startAngle + element.arcAngle - 0.0001

      val start = polarToCartesian(startAngle)
      val end   = polarToCartesian(endAngle)

      val largeArcFlag = if endAngle - startAngle <= 180 then "0" else "1"

      val pathData =
        s"M ${start.x} ${start.y} A ${radiusX} ${radiusY} 0 ${largeArcFlag} 0 ${end.x} ${end.y}"
      (
        path(
          d := pathData,
          `class` := s"posX ${element.position.x} posY ${element.position.y} width ${element.width} height ${element.height} radiusX ${element.width / 2} radiusY ${element.height / 2} startAngle ${element.startAngle} arcAngle ${element.arcAngle}",
          transform := s"rotate(${element.rotationAngle}, ${centerX - radiusX}, ${centerY - radiusY})",
          fill,
          stroke
        ).render,
        gradients
      )
  end makeArc

  private def makePolygon(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Polygon
  ) =
    val (xs, ys) = element.points.unzip(p => (p.x, p.y))

    val centerX = element.position.x
    val centerY = element.position.y

    val (fill, stroke, gradients) = makeStyle(element)

    (
      polygon(
        points    := xs.zip(ys).map((x, y) => s"$x,$y").mkString(" "),
        transform := s"translate(${centerX}, ${centerY})",
        fill,
        stroke
      ).render,
      gradients
    )
  end makePolygon

  private def makeText(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      element: Text
  ) =
    val centerX = element.position.x + xOffsetToOrigin
    val centerY = element.position.y + yOffsetToOrigin

    val (fill, stroke, gradients) = makeStyle(element)

    dom.window.console.log(s"Text: ${element.boundary.height.inPixels}")

    (
      text(
        x                := centerX,
        y                := centerY + element.boundary.height.inPixels,
        fontFamily       := element.typeface,
        fontWeight       := "bold",
        fontSize         := element.size,
        dominantBaseline := "ideographic",
//        alignmentBaseline := "mathematical",
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
      vectorGraphic.strokeStyle.map(svgStroke).getOrElse((Seq.empty, None))
    val gradients = fillGradient.toSeq ++ strokeGradient.toSeq

    (fill, stroke, gradients)
  end makeStyle

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
        id := idString,
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
          case color: Color => (Seq(fill := color.toString), None)
          case gradient: LinearGradient =>
            val (gradientTag, id) = svgLinearGradient(gradient)
            (Seq(fill := s"url(#$id)"), Some(gradientTag))
      case None => (Seq(fill := "none"), None)

  private def svgStroke(style: StrokeStyle): (Seq[Modifier], Option[dom.Element]) =
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
