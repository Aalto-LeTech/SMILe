package smile.pictures

import smile.infrastructure.Constants.MaximumOpacity
import smile.infrastructure.{BufferAdapter, DrawingSurface}
import smile.modeling.{BoundaryCalculator, NullBounds}

import java.awt.image.BufferedImage
import scala.annotation.tailrec
import scala.collection.mutable

object Renderer:
  def createBitmapFrom(elements: PictureElement*): Bitmap = createBitmapFrom(new Picture(elements))

  def createBitmapFrom(picture: Picture): Bitmap =
    if picture.elements.isEmpty then return new Bitmap(0, 0)

    val bounds = picture.viewport match
      case Some(viewport) => viewport.boundary
      case None           => BoundaryCalculator.fromBoundaries(picture.elements)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    if flooredWidth < 1 || flooredHeight < 1 then return new Bitmap(0, 0)

    require(flooredWidth > 0 && flooredHeight > 0, "Bitmap width and height must be positive")

    val buffer = new BufferAdapter(flooredWidth, flooredHeight)

    val (xOffsetToOrigoInPixels, yOffsetToOrigoInPixels) =
      val upperLeftCorner = bounds.upperLeftCorner

      val xOffset = -upperLeftCorner.x
      val yOffset = -upperLeftCorner.y

      (xOffset, yOffset)

    renderElements(
      picture,
      new DrawingSurface(buffer),
      xOffsetToOrigoInPixels,
      yOffsetToOrigoInPixels
    )

    new Bitmap(buffer, bounds)
  end createBitmapFrom

  private def renderElements(
      content: Picture,
      targetDrawingSurface: DrawingSurface,
      xOffsetToOrigoInPixels: Double,
      yOffsetToOrigoInPixels: Double
  ): Unit =
    for element <- content.elements.reverse do
      renderElement(
        element,
        targetDrawingSurface,
        xOffsetToOrigoInPixels,
        yOffsetToOrigoInPixels
      )

  private def renderElement(
      contentItem: PictureElement,
      targetDrawingSurface: DrawingSurface,
      xOffsetToOrigoInPixels: Double,
      yOffsetToOrigoInPixels: Double
  ): Unit =
    contentItem match
      case arc: Arc =>
        targetDrawingSurface.drawArc(
          xOffsetToOrigoInPixels,
          yOffsetToOrigoInPixels,
          arc.position.x,
          arc.position.y,
          arc.width,
          arc.height,
          arc.startAngle,
          arc.arcAngle,
          arc.rotationAngle,
          arc.hasBorder,
          arc.hasFilling,
          arc.color,
          arc.fillColor
        )
      case bitmap: Bitmap =>
        val topLeft  = bitmap.boundary.upperLeftCorner
        val topLeftX = xOffsetToOrigoInPixels + topLeft.x
        val topLeftY = yOffsetToOrigoInPixels + topLeft.y

        targetDrawingSurface.drawBitmap(
          bitmap.buffer.get,
          topLeftX,
          topLeftY,
          MaximumOpacity
        )
      case point: Point =>
        targetDrawingSurface.drawPoint(
          xOffsetToOrigoInPixels,
          yOffsetToOrigoInPixels,
          point.position.x,
          point.position.y,
          point.color
        )
      case polygon: Polygon =>
        if polygon.pointsRelativeToCenterAtOrigo.isEmpty then return
        else

          val position = polygon.position
          val points   = polygon.pointsRelativeToCenterAtOrigo

          val (xs, ys) = points.unzip(p => (p.x, p.y))

          val contentUpperLeftCorner = polygon.contentBoundary.upperLeftCorner
          val contentLeftEdge        = contentUpperLeftCorner.x
          val contentTopEdge         = contentUpperLeftCorner.y

          targetDrawingSurface.drawPolygon(
            xOffsetToOrigoInPixels,
            yOffsetToOrigoInPixels,
            position.x,
            position.y,
            xs,
            ys,
            points.length,
            contentLeftEdge,
            contentTopEdge,
            polygon.hasBorder,
            polygon.hasFilling,
            polygon.color,
            polygon.fillColor
          )
      case text: Text => targetDrawingSurface.drawText(text)

  end renderElement
