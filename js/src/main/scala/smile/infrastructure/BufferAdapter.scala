package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all.canvas
import smile.Settings
import smile.Settings.DefaultBackgroundColor
import smile.colors.Color
import smile.modeling.AffineTransformation

object BufferAdapter:
  val Empty: BufferAdapter = BufferAdapter(1, 1)

/** Adapter for managing and manipulating a `BufferedImage`. This class provides methods for common
  * image processing tasks such as copying, scaling, and transforming.
  *
  * @param buffer
  *   The underlying `BufferedImage` instance.
  */
class BufferAdapter(val width: Int, val height: Int) extends BufferAdapterAbstract:
  private val buffer: html.Canvas = canvas().render

  buffer.width = width
  buffer.height = height

  private[infrastructure] val ctx =
    buffer.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

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
    val imageData = ctx.getImageData(0, 0, width, height)
    newBuffer.ctx.putImageData(imageData, 0, 0)
    newBuffer

  /** Provides access to the underlying `Canvas`.
    *
    * @return
    *   The underlying `Canvas`.
    */
  def get: html.Canvas = buffer

  def imageData: Seq[Color] =
    ctx.getImageData(0, 0, width, height).data.toSeq.map(Color.fromRgbaInt)

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
    val argb = ctx.getImageData(x, y, 1, 1).data
    new Color(argb(0), argb(1), argb(2), argb(3))

  def setRGBA(x: Int, y: Int, color: Color): Unit =
    val imageData = ctx.createImageData(1, 1)
    imageData.data(0) = color.red
    imageData.data(1) = color.green
    imageData.data(2) = color.blue
    imageData.data(3) = color.opacity
    ctx.putImageData(imageData, x, y)

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
  ): BufferAdapter = ??? /*
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
  end scaleTo*/

  def setColorsFromSeq(colors: Seq[Color]): Unit =
    val imageData = ctx.createImageData(width, height)
    colors.zipWithIndex.foreach:
      case (color, index) =>
        val i = index * 4
        imageData.data(i) = color.red
        imageData.data(i + 1) = color.green
        imageData.data(i + 2) = color.blue
        imageData.data(i + 3) = color.opacity

    ctx.putImageData(imageData, 0, 0)
  end setColorsFromSeq

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
      ctx.getImageData(
        topLeftX.floor.toInt,
        topLeftY.floor.toInt,
        flooredWidth,
        flooredHeight
      )

    val newBuffer = BufferAdapter(flooredWidth, flooredHeight)
    newBuffer.ctx.putImageData(sourceBufferArea, 0, 0)

    newBuffer
  end copyPortionXYWH

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
  ): BufferAdapter = ??? /*

    val globalInterpolationMethod = TransformMethod

    val lowLevelTransformation = transformationToAWT(transformation)
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
  end createTransformedVersionWith*/

  /** Saves a `BufferedImage` to a specified path. The image is saved in PNG format.
    *
    * @param path
    *   The filesystem path where the image should be saved.
    * @return
    *   `true` if the image was saved successfully, `false` otherwise.
    */
  //def saveToPath(path: String): Boolean = ???
//    ImageIO.write(buffer, "png", new File(path))

  def toPNG: String = buffer.toDataURL("image/png")
end BufferAdapter
