package smile.infrastructure

import smile.colors.Color
import smile.infrastructure.Constants.MaximumOpacity
import smile.pictures.Text

import java.awt.geom.{AffineTransform, Arc2D, Ellipse2D, Path2D}
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, BasicStroke}

extension (value: Double)
  def truncate: Double =
    if value > 0 then value.floor else value.ceil

class DrawingSurface(val owner: BufferAdapter):
  /** A ``BasicStroke`` instance to represent as thin line as possible (i.e., width = 1 pixel). */
  // TODO: Change to 0 if no anti-aliasing
  private val HairlineStroke = new BasicStroke(1)

  /** A static ``Arc2D.Double`` instance to minimize creation of new objects during rendering. */
  private val StaticArc = new Arc2D.Double()

  /** A static ``Ellipse2D.Double`` instance to minimize creation of new objects during rendering.
    */
  private val StaticEllipse = new Ellipse2D.Double()

  def clearUsing(color: Color, useSourceColorLiterally: Boolean): Unit =
    owner.withGraphics2D: g =>
      if useSourceColorLiterally then g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC))

      g.setColor(color.toAWTColor)
      g.fillRect(0, 0, owner.width, owner.height)

  def drawBitmap(bitmap: BufferedImage): Boolean =
    drawBitmap(bitmap, 0.0, 0.0, MaximumOpacity)

  def drawBitmap(
      bitmap: BufferedImage,
      xInPixels: Double,
      yInPixels: Double,
      opacity: Int
  ): Boolean =
    val x = xInPixels.floor.toInt
    val y = yInPixels.floor.toInt

    val normalizedOpacity: Float = opacity.toFloat / MaximumOpacity

    owner.withGraphics2D: g =>
      g.setTransform(
        AffineTransform.getTranslateInstance(
          xInPixels,
          yInPixels
        )
      )
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, normalizedOpacity))
      g.drawImage(bitmap, 0, 0, null)

  def drawArc(
      xOffsetToOrigoInPixels: Double,
      yOffsetToOrigoInPixels: Double,
      xPositionOfCenterInPixels: Double,
      yPositionOfCenterInPixels: Double,
      widthInPixels: Double,
      heightInPixels: Double,
      startAngleInDegrees: Double,
      arcAngleInDegrees: Double,
      rotationAngleInDegrees: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): Unit =
    val scaledWidth  = (widthInPixels.floor).truncate
    val scaledHeight = (heightInPixels.floor).truncate

    val upperLeftXOffsetFromCenter = (scaledWidth / 2.0).truncate
    val upperLeftYOffsetFromCenter = (scaledHeight / 2.0).truncate

    val representsCompleteCycle = arcAngleInDegrees.abs >= 360

    owner.withGraphics2D: g =>
      g.setTransform(
        AffineTransform.getTranslateInstance(
          xOffsetToOrigoInPixels.truncate + xPositionOfCenterInPixels.truncate,
          yOffsetToOrigoInPixels.truncate + yPositionOfCenterInPixels.truncate
        )
      )

      g.setStroke(HairlineStroke)

      val xDifference =
        xOffsetToOrigoInPixels.truncate + xPositionOfCenterInPixels - upperLeftXOffsetFromCenter
      val yDifference =
        yOffsetToOrigoInPixels.truncate + yPositionOfCenterInPixels - upperLeftYOffsetFromCenter

      // If drawing a complete cycle, check if the cycle represents a small circle
      if representsCompleteCycle && Set(1.0, 2.0).contains(
          scaledWidth
        ) && scaledWidth == scaledHeight
      then

        // -------------------------------------------------------------------------
        // Special case for small circles
        //

        if hasBorder || hasFilling then
          g.setColor(if hasBorder then color.toAWTColor else fillColor.toAWTColor)

          val size = if scaledWidth == 1.0 then 1 else 2
          g.fillRect(
            upperLeftXOffsetFromCenter.toInt - size,
            upperLeftYOffsetFromCenter.toInt - size,
            size,
            size
          )
      else
        // -------------------------------------------------------------------------
        // General case for all arcs
        //

        if rotationAngleInDegrees != 0.0 then g.rotate(rotationAngleInDegrees)

        val shapes: List[(Color, Ellipse2D | Arc2D)] =
          (hasFilling, hasBorder, representsCompleteCycle) match
            case (true, false, true)  => List((fillColor, StaticEllipse))
            case (false, true, true)  => List((color, StaticEllipse))
            case (true, false, false) => List((fillColor, StaticArc))
            case (false, true, false) => List((color, StaticArc))
            case _                    => List((fillColor, StaticEllipse), (color, StaticEllipse))

        shapes.foreach:
          case (color, shape) =>
            g.setColor(color.toAWTColor)

            val ulX =
              if xDifference > 0 then -upperLeftXOffsetFromCenter + 0.5
              else -upperLeftXOffsetFromCenter - 0.5
            val ulY =
              if yDifference > 0 then -upperLeftYOffsetFromCenter + 0.5
              else -upperLeftYOffsetFromCenter - 0.5

            shape match
              case ellipse: Ellipse2D =>
                ellipse.setFrame(ulX, ulY, scaledWidth - 1, scaledHeight - 1)
              case arc: Arc2D =>
                arc.setArc(
                  ulX,
                  ulY,
                  scaledWidth - 1,
                  scaledHeight - 1,
                  startAngleInDegrees,
                  arcAngleInDegrees,
                  Arc2D.OPEN
                )

            if color == fillColor then g.fill(shape) else g.draw(shape)
  end drawArc

  def drawRoundedRectangle(
      upperLeftCornerXInPixels: Double,
      upperLeftCornerYInPixels: Double,
      widthInPixels: Double,
      heightInPixels: Double,
      roundingWidthInPixels: Double,
      roundingHeightInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): Unit =
    val upperLeftX: Int     = upperLeftCornerXInPixels.floor.toInt
    val upperLeftY: Int     = upperLeftCornerYInPixels.floor.toInt
    val width: Int          = widthInPixels.floor.toInt
    val height: Int         = heightInPixels.floor.toInt
    val roundingWidth: Int  = roundingWidthInPixels.floor.toInt
    val roundingHeight: Int = roundingHeightInPixels.floor.toInt

    owner.withGraphics2D: g =>
      if hasFilling then
        g.setColor(fillColor.toAWTColor)
        g.fillRoundRect(upperLeftX, upperLeftY, width, height, roundingWidth, roundingHeight)

      if hasBorder then
        g.setColor(color.toAWTColor)
        g.drawRoundRect(upperLeftX, upperLeftY, width, height, roundingWidth, roundingHeight)

  def drawPolyline(
      xCoordinates: Seq[Double],
      yCoordinates: Seq[Double],
      numberOfCoordinatesToDraw: Int,
      color: Color
  ): Unit =
    val xs = xCoordinates.map(_.floor.toInt).toArray
    val ys = yCoordinates.map(_.floor.toInt).toArray

    owner.withGraphics2D: g =>
      g.setColor(color.toAWTColor)
      g.drawPolyline(xs, ys, numberOfCoordinatesToDraw)

  def drawPolygon(
      xOffsetToOrigoInPixels: Double,
      yOffsetToOrigoInPixels: Double,
      xPositionInPixels: Double,
      yPositionInPixels: Double,
      xCoordinates: Seq[Double],
      yCoordinates: Seq[Double],
      numberOfCoordinatesToDraw: Int,
      leftEdgeInPixels: Double,
      topEdgeInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): Unit =
    if numberOfCoordinatesToDraw == 1 then
      drawPoint(
        xOffsetToOrigoInPixels,
        yOffsetToOrigoInPixels,
        xPositionInPixels + xCoordinates.head,
        yPositionInPixels + yCoordinates.head,
        color
      )
    else
      val xOffset = xOffsetToOrigoInPixels + xPositionInPixels
      val yOffset = yOffsetToOrigoInPixels + yPositionInPixels

      owner.withGraphics2D: g =>
        g.setStroke(HairlineStroke)

        g.setTransform(AffineTransform.getTranslateInstance(xOffset, yOffset))

        val path = Path2D.Double()
        for i <- 0 until numberOfCoordinatesToDraw do
          if i == 0 then path.moveTo(xCoordinates(i), yCoordinates(i))
          else path.lineTo(xCoordinates(i), yCoordinates(i))

        if hasFilling then
          g.setColor(fillColor.toAWTColor)
          g.fill(path)

        if hasBorder then g.setColor(color.toAWTColor)
        g.draw(path)

  def drawLine(
      fromXInPixels: Double,
      fromYInPixels: Double,
      toXInPixels: Double,
      toYInPixels: Double,
      color: Color
  ): Unit =
    val startX = fromXInPixels.floor.toInt
    val startY = fromYInPixels.floor.toInt
    val endX   = toXInPixels.floor.toInt
    val endY   = toYInPixels.floor.toInt

    owner.withGraphics2D: g =>
      g.setColor(color.toAWTColor)
      g.drawLine(startX, startY, endX, endY)

  def drawPoint(
      xOffsetToOrigoInPixels: Double,
      yOffsetToOrigoInPixels: Double,
      xInPixels: Double,
      yInPixels: Double,
      color: Color
  ): Unit =
    val x = xOffsetToOrigoInPixels.floor.toInt + xInPixels.floor.toInt
    val y = yOffsetToOrigoInPixels.floor.toInt + yInPixels.floor.toInt

    owner.withGraphics2D: g =>
      g.setStroke(HairlineStroke)
      g.setColor(color.toAWTColor)
      g.drawLine(x, y, x, y)

  def drawText(text: Text): Unit =
    val x = text.position.x.floor.toInt
    val y = text.position.y.floor.toInt + text.boundary.height.inPixels.toInt

    owner.withGraphics2D: g =>
      g.setColor(text.color.toAWTColor)
      g.setFont(text.font)
      g.drawString(text.content, x, y)
