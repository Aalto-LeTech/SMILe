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
      xOffsetToOrigo: Double,
      yOffsetToOrigo: Double,
      xPositionOfCenter: Double,
      yPositionOfCenter: Double,
      width: Double,
      height: Double,
      startAngle: Double,
      arcAngle: Double,
      rotationAngleInDegrees: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): Unit =
    val representsCompleteCycle = arcAngle.abs >= 360

    val shape =
      if representsCompleteCycle then new Ellipse2D.Double(0, 0, width, height)
      else new Arc2D.Double(0, 0, width, height, startAngle, arcAngle, Arc2D.OPEN)

    val offsetX = xOffsetToOrigo + xPositionOfCenter - width / 2
    val offsetY = yOffsetToOrigo + yPositionOfCenter - height / 2

    owner.withGraphics2D: g =>
      g.translate(offsetX, offsetY)
      g.rotate(rotationAngleInDegrees.toRadians)

      g.setStroke(HairlineStroke)

      if hasFilling then
        g.setColor(fillColor.toAWTColor)
        g.fill(shape)

      if hasBorder then
        g.setColor(color.toAWTColor)
        g.draw(shape)
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
      xOffsetToOrigo: Double,
      yOffsetToOrigo: Double,
      xPosition: Double,
      yPosition: Double,
      xCoordinates: Seq[Double],
      yCoordinates: Seq[Double],
      numberOfCoordinatesToDraw: Int,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): Unit =
    if numberOfCoordinatesToDraw == 1 then
      drawPoint(
        xOffsetToOrigo,
        yOffsetToOrigo,
        xPosition + xCoordinates.head,
        yPosition + yCoordinates.head,
        color
      )
    else
      val xOffset = xOffsetToOrigo + xPosition
      val yOffset = yOffsetToOrigo + yPosition

      owner.withGraphics2D: g =>
        g.setStroke(HairlineStroke)
        g.translate(xOffset, yOffset)

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
      xOffsetToOrigo: Double,
      yOffsetToOrigo: Double,
      xPosition: Double,
      yPosition: Double,
      fromX: Double,
      fromY: Double,
      toX: Double,
      toY: Double,
      color: Color
  ): Unit =
    val xOffset = xOffsetToOrigo + xPosition
    val yOffset = yOffsetToOrigo + yPosition

    val path = Path2D.Double()
    owner.withGraphics2D: g =>
      g.translate(xOffset, yOffset)
      g.setColor(color.toAWTColor)
      path.moveTo(fromX, fromY)
      path.lineTo(toX, toY)
      g.draw(path)

  def drawPoint(
      xOffsetToOrigo: Double,
      yOffsetToOrigo: Double,
      x: Double,
      y: Double,
      color: Color
  ): Unit =
    val offsetX = xOffsetToOrigo + x
    val offsetY = yOffsetToOrigo + y

    owner.withGraphics2D: g =>
      g.translate(offsetX, offsetY)

      g.setStroke(HairlineStroke)
      g.setColor(color.toAWTColor)
      g.drawLine(0, 0, 0, 0)

  def drawText(text: Text): Unit =
    val x = text.position.x
    val y = text.position.y + text.boundary.height.inPixels

    owner.withGraphics2D: g =>
      g.translate(x, y)
      g.setColor(text.color.toAWTColor)
      g.setFont(text.font)
      g.drawString(text.content, 0, 0)
