package smile.infrastructure

import smile.Settings
import smile.Settings.{CanvasesAreResizedBasedOnTransformations, DefaultBackgroundColor}
import smile.colors.Color
import smile.modeling.AffineTransformation

import java.awt.*
import java.awt.geom.{AffineTransform, Rectangle2D}
import java.awt.image.{AffineTransformOp, BufferedImage}
import javax.swing.Icon

/** Adapter for managing and manipulating a `BufferedImage`. This class provides methods for common
  * image processing tasks such as copying, scaling, and transforming.
  *
  * @param buffer
  *   The underlying `BufferedImage` instance.
  */
class BufferAdapter(private val buffer: BufferedImage):

  /** Creates a `BufferAdapter` instance with specified width and height, initializing a new
    * `BufferedImage`.
    *
    * @param width
    *   The width of the image.
    * @param height
    *   The height of the image.
    */
  def this(width: Int, height: Int) =
    this(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))

  lazy val width: Int  = buffer.getWidth
  lazy val height: Int = buffer.getHeight

  /** Scaling method used for image scaling operations. */
  val ScalingMethod: Int = Image.SCALE_AREA_AVERAGING

  /** Transformation method used for image transformation operations. */
  val TransformMethod: Int = AffineTransformOp.TYPE_BICUBIC

  /** Creates a deep copy of the current `BufferAdapter` instance, drawing the current buffer onto a
    * new one.
    *
    * @return
    *   A new `BufferAdapter` instance that is a copy of the current one.
    */

  def deepCopy: BufferAdapter =
    val newBuffer = new BufferAdapter(width, height)
    newBuffer.withGraphics2D(g => g.drawImage(buffer, 0, 0, null))
    newBuffer

  /** Provides access to the underlying `BufferedImage`.
    *
    * @return
    *   The underlying `BufferedImage`.
    */
  def get: BufferedImage = buffer

  /** Provides access to the graphics context of the underlying `BufferedImage`.
    *
    * @return
    *   The `Graphics` instance for the underlying `BufferedImage`.
    */

  def graphics: Graphics = buffer.getGraphics

  /** Converts the buffer into a Swing `Icon`.
    *
    * @return
    *   An `Icon` representing the buffer.
    */
  def toSwingIcon: Icon = new Icon:
    def paintIcon(target: Component, graphics: Graphics, x: Int, y: Int): Unit =
      graphics.drawImage(buffer, x, y, target)
    def getIconWidth: Int  = width
    def getIconHeight: Int = height
  end toSwingIcon

  /** Retrieves the color at a specified pixel location in the buffer.
    *
    * @param x
    *   The x-coordinate of the pixel.
    * @param y
    *   The y-coordinate of the pixel.
    * @return
    *   The `Color` of the pixel at the specified coordinates.
    */
  def pixelColor(x: Int, y: Int): Color =
    val rgba = buffer.getRGB(x, y)
    new Color(
      red = rgba >> 16 & 0xff,
      green = rgba >> 8 & 0xff,
      blue = rgba & 0xff,
      opacity = rgba >> 24 & 0xff
    )

  /** Scales the image to a target width and height.
    *
    * @param targetWidth
    *   The target width for the scaled image.
    * @param targetHeight
    *   The target height for the scaled image.
    * @return
    *   A new `BufferAdapter` instance containing the scaled image.
    */
  def scaleTo(targetWidth: Double, targetHeight: Double): BufferAdapter =
    val newWidth  = targetWidth.toInt.abs
    val newHeight = targetHeight.toInt.abs

    val image = buffer.getScaledInstance(newWidth, newHeight, ScalingMethod)

    val newBuffer = new BufferAdapter(newWidth, newHeight)

    val g = newBuffer.buffer.createGraphics()

    // Flip the image if necessary.
    if targetWidth < 0 || targetHeight < 0 then
      val tx = AffineTransform.getScaleInstance(
        if targetWidth < 0 then -1 else 1,
        if targetHeight < 0 then -1 else 1
      )
      if targetWidth < 0 then tx.translate(-newWidth, 0)
      if targetHeight < 0 then tx.translate(0, -newHeight)
      g.setTransform(tx)

    g.drawImage(image, 0, 0, null)
    g.dispose()

    newBuffer
  end scaleTo

  /** Sets the colors of the image from a sequence of `Color` objects.
    *
    * @param colors
    *   A sequence of `Color` objects to be applied to the image.
    */
  def setColorsFromSeq(
      colors: Seq[Color]
  ): Unit =
    def colorArray = new Array[Int](colors.length)
    val reds       = colorArray
    val greens     = colorArray
    val blues      = colorArray
    val opacities  = colorArray

    for (c, i) <- colors.zipWithIndex do
      if c != null then
        reds(i) = c.red
        greens(i) = c.green
        blues(i) = c.blue
        opacities(i) = c.opacity

    val raster     = buffer.getRaster
    val setSamples = raster.setSamples(0, 0, width, height, _: Int, _: Array[Int])

    setSamples(0, reds)
    setSamples(1, greens)
    setSamples(2, blues)
    setSamples(3, opacities)

    buffer.setData(raster)
  end setColorsFromSeq

  /** Copies a portion of the image defined by two corners: top-left and bottom-right.
    *
    * @param topLeftX
    *   X-coordinate of the top-left corner.
    * @param topLeftY
    *   Y-coordinate of the top-left corner.
    * @param bottomRightX
    *   X-coordinate of the bottom-right corner.
    * @param bottomRightY
    *   Y-coordinate of the bottom-right corner.
    * @return
    *   A new `BufferAdapter` instance containing the copied portion of the image.
    */
  def copyPortionXYXY(
      topLeftX: Double,
      topLeftY: Double,
      bottomRightX: Double,
      bottomRightY: Double
  ): BufferAdapter =
    val (x0, x1) =
      if topLeftX > bottomRightX then (bottomRightX, topLeftX)
      else (topLeftX, bottomRightX)

    val (y0, y1) =
      if topLeftY > bottomRightY then (bottomRightY, topLeftY)
      else (topLeftY, bottomRightY)

    val width  = x1 - x0
    val height = y1 - y0

    copyPortionXYWH(topLeftX, topLeftY, width, height)

  /** Copies a portion of the image defined by a top-left corner and dimensions.
    *
    * @param topLeftX
    *   X-coordinate of the top-left corner.
    * @param topLeftY
    *   Y-coordinate of the top-left corner.
    * @param width
    *   Width of the portion to copy.
    * @param height
    *   Height of the portion to copy.
    * @return
    *   A new `BufferAdapter` instance containing the copied portion of the image.
    */
  def copyPortionXYWH(
      topLeftX: Double,
      topLeftY: Double,
      width: Double,
      height: Double
  ): BufferAdapter =

    val flooredWidth: Int  = width.floor.toInt
    val flooredHeight: Int = height.floor.toInt

    val sourceBufferArea =
      buffer.getSubimage(
        topLeftX.floor.toInt,
        topLeftY.floor.toInt,
        flooredWidth,
        flooredHeight
      )

    val initializer = (g: Graphics2D) => g.drawImage(sourceBufferArea, null, 0, 0)

    val newBuffer = createNormalizedLowLevelBitmapBufferOf(
      flooredWidth,
      flooredHeight,
      Some(initializer)
    )

    newBuffer

  private def createNormalizedLowLevelBitmapBufferOf(
      width: Int,
      height: Int,
      initializer: Option[Graphics2D => Unit]
  ): BufferAdapter =

    val newBuffer = new BufferAdapter(
      width,
      height
    )

    newBuffer.withGraphics2D(g =>
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC))
      g.setColor(DefaultBackgroundColor.toAWTColor)
      g.fillRect(0, 0, width, height)

      initializer.foreach(i => i(g))
    )

    newBuffer
  end createNormalizedLowLevelBitmapBufferOf

  def withGraphics2D[ResultType](
      workUnit: Graphics2D => ResultType
  ): ResultType =

    var gr2D: Graphics2D              = null
    var memorizedThrowable: Throwable = null

    try
      gr2D = buffer.createGraphics()

      setDefaultGraphics2DProperties(gr2D)
      workUnit(gr2D)
    catch
      case caughtThrowable: Throwable =>
        memorizedThrowable = caughtThrowable
        throw caughtThrowable
    finally
      if gr2D != null then
        if memorizedThrowable != null then
          try gr2D.dispose()
          catch
            case caughtThrowable: Throwable =>
              memorizedThrowable.addSuppressed(caughtThrowable)
        else gr2D.dispose()

  private inline final def setDefaultGraphics2DProperties(g: Graphics2D): Unit =
    val antialiasingState =
      if Settings.DrawingIsAntiAliased then RenderingHints.VALUE_ANTIALIAS_ON
      else RenderingHints.VALUE_ANTIALIAS_OFF

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, antialiasingState)

    val textAntialiasingState =
      if Settings.DrawingIsAntiAliased then RenderingHints.VALUE_TEXT_ANTIALIAS_ON
      else RenderingHints.VALUE_TEXT_ANTIALIAS_OFF

    g.setRenderingHint(
      RenderingHints.KEY_TEXT_ANTIALIASING,
      textAntialiasingState
    )

    g.setRenderingHint(
      RenderingHints.KEY_ALPHA_INTERPOLATION,
      RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY
    )
    g.setRenderingHint(
      RenderingHints.KEY_RENDERING,
      RenderingHints.VALUE_RENDER_QUALITY
    )
  end setDefaultGraphics2DProperties

  /** Iterates over all pixel locations in the buffer, invoking a callback function for each pixel
    * coordinate.
    *
    * @param callback
    *   A function that takes two integers (x and y coordinates) and returns Unit. It is called for
    *   each pixel.
    */
  def iterateLocations(callback: (Int, Int) => Unit): Unit =
    for
      x <- 0 until width
      y <- 0 until height
    do callback(x, y)

  /** Creates a new `BufferAdapter` instance that is a transformed version of the current buffer.
    * The transformation is applied using an `AffineTransformation`. The canvas can optionally be
    * resized based on the transformation.
    *
    * @param transformation
    *   The `AffineTransformation` to apply to the image.
    * @param resizeCanvasBasedOnTransformation
    *   A boolean indicating whether the canvas should be resized based on the transformation.
    *   Defaults to the value of `CanvasesAreResizedBasedOnTransformations` from `Settings`.
    * @param backgroundColor
    *   The background color to use when clearing the canvas if resizing is necessary. Defaults to
    *   `DefaultBackgroundColor`.
    * @return
    *   A new `BufferAdapter` instance containing the transformed image.
    */
  def createTransformedVersionWith(
      transformation: AffineTransformation,
      resizeCanvasBasedOnTransformation: Boolean = CanvasesAreResizedBasedOnTransformations,
      backgroundColor: Color = DefaultBackgroundColor
  ): BufferAdapter =

    val globalInterpolationMethod = TransformMethod

    val lowLevelTransformation = transformation.toAWTAffineTransform
    val transformedContentBoundaries: Rectangle2D =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)
        .getBounds2D(buffer)

    val (offsetLeft, offsetTop, offsetRight, offsetBottom) =
      if !resizeCanvasBasedOnTransformation then (0.0, 0.0, 0.0, 0.0)
      else
        (
          -transformedContentBoundaries.getMinX,
          -transformedContentBoundaries.getMinY,
          transformedContentBoundaries.getMaxX - width,
          transformedContentBoundaries.getMaxY - height
        )

    val (resultingImageWidth, resultingImageHeight) =
      if !resizeCanvasBasedOnTransformation then (width, height)
      else
        (
          Math.floor(width.toDouble + offsetLeft + offsetRight).toInt,
          Math.floor(height.toDouble + offsetTop + offsetBottom).toInt
        )

    if resizeCanvasBasedOnTransformation then
      if offsetTop > 0 || offsetLeft > 0 then
        val translationToBringTheRotatedBitmapFullyVisible =
          AffineTransform.getTranslateInstance(offsetLeft, offsetTop)

        lowLevelTransformation.preConcatenate(translationToBringTheRotatedBitmapFullyVisible)

    val finalTransformOperation =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)
    val resultingBuffer = BufferAdapter(resultingImageWidth, resultingImageHeight)

    new DrawingSurface(resultingBuffer).clearUsing(backgroundColor, true)

    finalTransformOperation.filter(buffer, resultingBuffer.get)

    resultingBuffer
  end createTransformedVersionWith
