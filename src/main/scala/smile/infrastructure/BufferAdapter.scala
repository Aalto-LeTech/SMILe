package smile.infrastructure

import smile.Settings
import smile.Settings.DefaultBackgroundColor
import smile.colors.Color
import smile.modeling.AffineTransformation

import java.awt.*
import java.awt.geom.{AffineTransform, Rectangle2D}
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.Icon

object BufferAdapter:
  val Empty: BufferAdapter = BufferAdapter(1, 1)

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
  private def ScalingMethod: Int = Settings.BufferScalingMethod.value

  /** Transformation method used for image transformation operations. */
  private def TransformMethod: Int = Settings.BufferTransformMethod.value

  /** Creates a deep copy of the current `BufferAdapter` instance, drawing the current buffer onto a
    * new one.
    *
    * @return
    *   A new `BufferAdapter` instance that is a copy of the current one.
    */

  def deepCopy: BufferAdapter =
    val newBuffer = BufferAdapter(width, height)
    newBuffer.withGraphics2D(g => g.drawImage(buffer, 0, 0, null))
    newBuffer

  /** Provides access to the underlying `BufferedImage`.
    *
    * @return
    *   The underlying `BufferedImage`.
    */
  def get: BufferedImage = buffer

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
    val argb = buffer.getRGB(x, y)
    new Color(argb)

  /** Scales the image to a target width and height.
    *
    * @param targetWidth
    *   The target width for the scaled image.
    * @param targetHeight
    *   The target height for the scaled image.
    * @return
    *   A new `BufferAdapter` instance containing the scaled image.
    */
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double
  ): BufferAdapter =
    val isNearestNeighbor = Settings.BufferScalingMethod == Settings.ScalingMethod.NearestNeighbor

    val newWidth  = targetWidth.toInt.abs
    val newHeight = targetHeight.toInt.abs

    val newBuffer = BufferAdapter(newWidth, newHeight)

    val g = newBuffer.buffer.createGraphics()

    // Flip the image if necessary.
    if targetWidth < 0 || targetHeight < 0 then
      val tx = AffineTransform.getScaleInstance(
        if targetWidth < 0 then -1 else 1,
        if targetHeight < 0 then -1 else 1
      )
      if targetWidth < 0 then tx.translate(-newWidth, 0)
      if targetHeight < 0 then tx.translate(0, -newHeight)
      g.transform(tx)

    if isNearestNeighbor then
      g.setRenderingHint(
        RenderingHints.KEY_INTERPOLATION,
        RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
      )
      g.drawImage(this.buffer, 0, 0, newWidth, newHeight, null)
    else
      val image = this.buffer.getScaledInstance(newWidth, newHeight, ScalingMethod)
      g.drawImage(image, 0, 0, null)
    g.dispose()

    newBuffer
  end scaleTo

  /** Sets the colors of the image from a sequence of `Color` objects.
    *
    * @param colors
    *   A sequence of `Color` objects to be applied to the image. The sequence must have exactly
    *   width * height items.
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
    * @param left
    *   X-coordinate of the top-left corner.
    * @param top
    *   Y-coordinate of the top-left corner.
    * @param right
    *   X-coordinate of the bottom-right corner.
    * @param bottom
    *   Y-coordinate of the bottom-right corner.
    * @return
    *   A new `BufferAdapter` instance containing the copied portion of the image.
    */
  def copyPortionXYXY(
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  ): BufferAdapter =
    val (x0, x1) = if left > right then (right, left) else (left, right)
    val (y0, y1) = if top > bottom then (bottom, top) else (top, bottom)

    val width  = x1 - x0
    val height = y1 - y0

    copyPortionXYWH(left, top, width, height)

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
    if width <= 0 || height <= 0 then return BufferAdapter.Empty

    val flooredWidth: Int  = width.floor.toInt
    val flooredHeight: Int = height.floor.toInt

    val sourceBufferArea =
      buffer.getSubimage(
        topLeftX.floor.toInt,
        topLeftY.floor.toInt,
        flooredWidth,
        flooredHeight
      )

    new BufferAdapter(sourceBufferArea)

  private[infrastructure] def withGraphics2D[ResultType](
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
  end setDefaultGraphics2DProperties

  /** Creates a new `BufferAdapter` instance that is a transformed version of the current buffer.
    * The transformation is applied using an `AffineTransformation`. The canvas can optionally be
    * resized based on the transformation.
    *
    * @param transformation
    *   The `AffineTransformation` to apply to the image.
    * @param backgroundColor
    *   The background color to use when clearing the canvas if resizing is necessary. Defaults to
    *   `DefaultBackgroundColor`.
    * @return
    *   A new `BufferAdapter` instance containing the transformed image.
    */
  def createTransformedVersionWith(
      transformation: AffineTransformation,
      backgroundColor: Color = DefaultBackgroundColor
  ): BufferAdapter =

    val globalInterpolationMethod = TransformMethod

    val lowLevelTransformation = transformation.toAWTAffineTransform
    val transformedContentBoundaries: Rectangle2D =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)
        .getBounds2D(buffer)

    val (offsetLeft, offsetTop, offsetRight, offsetBottom) =
      (
        -transformedContentBoundaries.getMinX,
        -transformedContentBoundaries.getMinY,
        transformedContentBoundaries.getMaxX - width,
        transformedContentBoundaries.getMaxY - height
      )

    val (resultingImageWidth, resultingImageHeight) =
      (
        Math.floor(width.toDouble + offsetLeft + offsetRight).toInt,
        Math.floor(height.toDouble + offsetTop + offsetBottom).toInt
      )

    if offsetTop > 0 || offsetLeft > 0 then
      val translationToBringTheRotatedBitmapFullyVisible =
        AffineTransform.getTranslateInstance(offsetLeft, offsetTop)
      lowLevelTransformation.preConcatenate(translationToBringTheRotatedBitmapFullyVisible)

    val finalTransformOperation =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)

    if resultingImageWidth == 0 || resultingImageHeight == 0 then BufferAdapter.Empty
    else
      val resultingBuffer = BufferAdapter(resultingImageWidth, resultingImageHeight)
      new DrawingSurface(resultingBuffer).clearUsing(backgroundColor, true)
      finalTransformOperation.filter(buffer, resultingBuffer.get)
      resultingBuffer
  end createTransformedVersionWith

  /** Saves a `BufferedImage` to a specified path. The image is saved in PNG format.
    *
    * @param path
    *   The filesystem path where the image should be saved.
    * @return
    *   `true` if the image was saved successfully, `false` otherwise.
    */
  def saveToPath(path: String): Boolean =
    ImageIO.write(buffer, "png", new File(path))
end BufferAdapter
