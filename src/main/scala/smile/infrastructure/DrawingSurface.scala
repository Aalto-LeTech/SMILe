package smile.infrastructure

import smile.colors.{Color, PresetColor}
import smile.infrastructure.Constants.MaximumOpacity
import smile.pictures.{FillStyle, StrokeStyle}

import java.awt.*
import java.awt.geom.{AffineTransform, Arc2D, Ellipse2D, Path2D}
import java.awt.image.BufferedImage

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
    * @param fillStyle
    *   Optional fill style for the arc.
    * @param strokeStyle
    *   Optional stroke style for the arc.
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
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
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

      draw(g, fillStyle, strokeStyle, shape)
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
    * @param fillStyle
    *   Optional fill style for the polygon.
    * @param strokeStyle
    *   Optional stroke style for the polygon.
    */
  def drawPolygon(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      xPosition: Double,
      yPosition: Double,
      xCoordinates: Seq[Double],
      yCoordinates: Seq[Double],
      numberOfCoordinatesToDraw: Int,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): Unit =
    if numberOfCoordinatesToDraw == 1 then
      drawPoint(
        xOffsetToOrigin,
        yOffsetToOrigin,
        xPosition + xCoordinates.head,
        yPosition + yCoordinates.head,
        fillStyle.map(_.color).getOrElse(PresetColor.Transparent)
      )
    else
      val xOffset = xOffsetToOrigin + xPosition
      val yOffset = yOffsetToOrigin + yPosition

      owner.withGraphics2D: g =>
        g.translate(xOffset, yOffset)

        val path = StaticPath
        path.reset()
        for i <- 0 until numberOfCoordinatesToDraw do
          if i == 0 then path.moveTo(xCoordinates(i), yCoordinates(i))
          else path.lineTo(xCoordinates(i), yCoordinates(i))
        path.lineTo(xCoordinates.head, yCoordinates.head)

        draw(g, fillStyle, strokeStyle, path)

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

  /** Draws text on the drawing surface, optionally scaling to fit within specified dimensions.
    *
    * @param xOffsetToOrigin
    *   The horizontal offset from the origin to the point where the text drawing begins.
    * @param yOffsetToOrigin
    *   The vertical offset from the origin to the point where the text drawing begins.
    * @param isScaled
    *   Indicates whether the text should be scaled to fit within the specified width and height.
    * @param width
    *   The width within which the text should fit. Used only if `isScaled` is true.
    * @param height
    *   The height within which the text should fit. Used only if `isScaled` is true.
    * @param x
    *   The x-coordinate of the center point for the text's bounding box.
    * @param y
    *   The y-coordinate of the center point for the text's bounding box.
    * @param text
    *   The text content to be drawn.
    * @param font
    *   The font used to draw the text.
    * @param fillStyle
    *   An optional fill style for the text.
    * @param strokeStyle
    *   An optional stroke style for the text.
    */
  def drawText(
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double,
      isScaled: Boolean,
      width: Double,
      height: Double,
      x: Double,
      y: Double,
      text: String,
      font: Font,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): Unit =
    owner.withGraphics2D: g =>
      val glyphVector = if isScaled then
        val visualBounds =
          font
            .createGlyphVector(g.getFontRenderContext, text)
            .getVisualBounds

        val scaleX = width / visualBounds.getWidth
        val scaleY = height / visualBounds.getHeight
        val scaleTransform = AffineTransform.getScaleInstance(
          scaleX,
          scaleY
        )

        font
          .deriveFont(scaleTransform)
          .createGlyphVector(g.getFontRenderContext, text)
      else font.createGlyphVector(g.getFontRenderContext, text)

      val visualBounds = glyphVector.getVisualBounds
      val translateX =
        xOffsetToOrigin + x - visualBounds.getWidth / 2
      val translateY =
        yOffsetToOrigin + y - visualBounds.getHeight / 2 - visualBounds.getY

      g.translate(translateX, translateY)
      draw(g, fillStyle, strokeStyle, glyphVector.getOutline)

  private def draw(
      g: Graphics2D,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle],
      shape: Shape
  ): Unit =
    fillStyle match
      case Some(style) =>
        g.setColor(style.color.toAWTColor)
        g.fill(shape)
      case None =>

    strokeStyle match
      case Some(style) =>
        g.setStroke(style.toAWTStroke)
        g.setColor(style.color.toAWTColor)
        g.draw(shape)
      case None =>
