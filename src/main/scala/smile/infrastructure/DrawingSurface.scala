package smile.infrastructure

import smile.colors.Color
import smile.infrastructure.Constants.MaximumOpacity
import smile.pictures.Text

import java.awt.geom.{AffineTransform, Arc2D, Ellipse2D, Path2D}
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, BasicStroke}

/** Extension method for `Double` to truncate its value towards zero.
  */
extension (value: Double)
  /** Truncates the decimal part of a double, moving towards zero.
    *
    * @return
    *   The truncated `Double` value.
    */
  def truncate: Double =
    if value > 0 then value.floor else value.ceil

/** Represents a drawing surface for rendering shapes, images, and text. It encapsulates a
  * `BufferAdapter` to provide a high-level drawing API.
  *
  * @param owner
  *   The `BufferAdapter` instance that this `DrawingSurface` operates on.
  */
class DrawingSurface(val owner: BufferAdapter):
  /** A ``BasicStroke`` instance to represent as thin line as possible (i.e., width = 1 pixel). */
  private val HairlineStroke = new BasicStroke(1)

  /** A static ``Arc2D.Double`` instance to minimize creation of new objects during rendering. */
  private val StaticArc = new Arc2D.Double()

  /** A static ``Ellipse2D.Double`` instance to minimize creation of new objects during rendering.
    */
  private val StaticEllipse = new Ellipse2D.Double()

  /** A static ``Path2D.Double`` instance to minimize creation of new objects during rendering.
    */
  private val StaticPath = new Path2D.Double()

  /** Clears the drawing surface using the specified color.
    *
    * @param color
    *   The `Color` to clear with.
    * @param useSourceColorLiterally
    *   If true, uses the source color without blending.
    */
  def clearUsing(color: Color, useSourceColorLiterally: Boolean): Unit =
    owner.withGraphics2D: g =>
      if useSourceColorLiterally then g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC))

      g.setColor(color.toAWTColor)
      g.fillRect(0, 0, owner.width, owner.height)

  /** Draws a bitmap image on the surface.
    *
    * @param bitmap
    *   The `BufferedImage` to draw.
    * @return
    *   A boolean indicating success.
    */
  def drawBitmap(bitmap: BufferedImage): Boolean =
    drawBitmap(bitmap, 0.0, 0.0, MaximumOpacity)

  /** Draws a bitmap image with specified position and opacity.
    *
    * @param bitmap
    *   The `BufferedImage` to draw.
    * @param x
    *   The x position in pixels.
    * @param y
    *   The y position in pixels.
    * @param opacity
    *   The opacity for the image.
    * @return
    *   A boolean indicating success.
    */

  def drawBitmap(
      bitmap: BufferedImage,
      x: Double,
      y: Double,
      opacity: Int
  ): Boolean =
    val normalizedOpacity: Float = opacity.toFloat / MaximumOpacity

    owner.withGraphics2D: g =>
      g.setTransform(
        AffineTransform.getTranslateInstance(
          x,
          y
        )
      )
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, normalizedOpacity))
      g.drawImage(bitmap, 0, 0, null)

  /** Draws an arc on the surface.
    *
    * @param xOffsetToOrigin
    *   Offset to the origin.
    * @param yOffsetToOrigin
    *   Offset to the origin.
    * @param xPositionOfCenter
    *   X position of the center.
    * @param yPositionOfCenter
    *   Y position of the center.
    * @param width
    *   Width of the arc.
    * @param height
    *   Height of the arc.
    * @param startAngle
    *   Starting angle.
    * @param arcAngle
    *   Arc angle.
    * @param rotationAngleInDegrees
    *   Rotation angle in degrees.
    * @param hasBorder
    *   Indicates if the arc has a border.
    * @param hasFilling
    *   Indicates if the arc is filled.
    * @param color
    *   Color of the border.
    * @param fillColor
    *   Color of the filling.
    */
  def drawArc(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
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
      if representsCompleteCycle then
        StaticEllipse.setFrame(0, 0, width, height)
        StaticEllipse
      else
        StaticArc.setArc(0, 0, width, height, startAngle, arcAngle, Arc2D.OPEN)
        StaticArc

    val offsetX = xOffsetToOrigin + xPositionOfCenter - width / 2
    val offsetY = yOffsetToOrigin + yPositionOfCenter - height / 2

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

  /** Draws a polygon on the surface.
    *
    * @param xOffsetToOrigin
    *   Offset to the origin.
    * @param yOffsetToOrigin
    *   Offset to the origin.
    * @param xPosition
    *   X position.
    * @param yPosition
    *   Y position.
    * @param xCoordinates
    *   X coordinates of the polygon points.
    * @param yCoordinates
    *   Y coordinates of the polygon points.
    * @param numberOfCoordinatesToDraw
    *   Number of coordinates to draw.
    * @param hasBorder
    *   Indicates if the polygon has a border.
    * @param hasFilling
    *   Indicates if the polygon is filled.
    * @param color
    *   Color of the border.
    * @param fillColor
    *   Color of the filling.
    */
  def drawPolygon(
                   xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
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
        xOffsetToOrigin,
        yOffsetToOrigin,
        xPosition + xCoordinates.head,
        yPosition + yCoordinates.head,
        color
      )
    else
      val xOffset = xOffsetToOrigin + xPosition
      val yOffset = yOffsetToOrigin + yPosition

      owner.withGraphics2D: g =>
        g.setStroke(HairlineStroke)
        g.translate(xOffset, yOffset)

        val path = StaticPath
        path.reset()
        for i <- 0 until numberOfCoordinatesToDraw do
          if i == 0 then path.moveTo(xCoordinates(i), yCoordinates(i))
          else path.lineTo(xCoordinates(i), yCoordinates(i))

        if hasFilling then
          g.setColor(fillColor.toAWTColor)
          g.fill(path)

        if hasBorder then
          g.setColor(color.toAWTColor)
          g.draw(path)

  /** Draws a line on the surface.
    *
    * @param xOffsetToOrigin
    *   Offset to the origin.
    * @param yOffsetToOrigin
    *   Offset to the origin.
    * @param xPosition
    *   X position.
    * @param yPosition
    *   Y position.
    * @param fromX
    *   Starting X position of the line.
    * @param fromY
    *   Starting Y position of the line.
    * @param toX
    *   Ending X position of the line.
    * @param toY
    *   Ending Y position of the line.
    * @param color
    *   Color of the line.
    */
  def drawLine(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      xPosition: Double,
      yPosition: Double,
      fromX: Double,
      fromY: Double,
      toX: Double,
      toY: Double,
      color: Color
  ): Unit =
    val xOffset = xOffsetToOrigin + xPosition
    val yOffset = yOffsetToOrigin + yPosition

    val path = StaticPath
    path.reset()
    owner.withGraphics2D: g =>
      g.translate(xOffset, yOffset)
      g.setColor(color.toAWTColor)
      path.moveTo(fromX, fromY)
      path.lineTo(toX, toY)
      g.draw(path)

  /** Draws a point on the surface.
    *
    * @param xOffsetToOrigin
    *   Offset to the origin.
    * @param yOffsetToOrigin
    *   Offset to the origin.
    * @param x
    *   X position of the point.
    * @param y
    *   Y position of the point.
    * @param color
    *   Color of the point.
    */
  def drawPoint(
                 xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      x: Double,
      y: Double,
      color: Color
  ): Unit =
    val offsetX = xOffsetToOrigin + x
    val offsetY = yOffsetToOrigin + y

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
