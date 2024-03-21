package smile.infrastructure

import smile.Settings.DefaultBackgroundColor
import smile.colors.Color
import smile.modeling.AffineTransformation

object BufferAdapter:
  def apply(width: Int, height: Int): BufferAdapter[?] = 
    PlatformSpecific.bufferAdapter(width, height)

  val Empty: BufferAdapter[?] = apply(1, 1)

abstract class BufferAdapter[Buffer]:

  val width: Int
  val height: Int

  def imageData: Seq[Color]

  def get: Buffer

  /** Creates a deep copy of the current `JVMBufferAdapter` instance, drawing the current buffer
    * onto a new one.
    *
    * @return
    *   A new `JVMBufferAdapter` instance that is a copy of the current one.
    */
  def deepCopy: BufferAdapter[?]

  /** Sets the colors of the image from a sequence of `Color` objects.
    *
    * @param colors
    *   A sequence of `Color` objects to be applied to the image. The sequence must have exactly
    *   width * height items.
    */
  def setColorsFromSeq(
      colors: Seq[Color]
  ): Unit

  /** Retrieves the color at a specified pixel location in the buffer.
    *
    * @param x
    *   The x-coordinate of the pixel.
    * @param y
    *   The y-coordinate of the pixel.
    * @return
    *   The `Color` of the pixel at the specified coordinates.
    */
  def pixelColor(x: Int, y: Int): Color

  /** Scales the image to a target width and height.
    *
    * @param targetWidth
    *   The target width for the scaled image.
    * @param targetHeight
    *   The target height for the scaled image.
    * @return
    *   A new `JVMBufferAdapter` instance containing the scaled image.
    */
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double
  ): BufferAdapter[?]

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
  ): BufferAdapter[Buffer] =
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
  ): BufferAdapter[Buffer]

  def setColorsByLocation(generator: (Int, Int) => Color): Unit =
    val data = (0 until width * height).map: i =>
      val x = i % width
      val y = i / width
      generator(x, y)

    setColorsFromSeq(data)
  end setColorsByLocation

  def transformColorToColor(transformer: Color => Color): BufferAdapter[?] =
    val result      = BufferAdapter(width, height)
    val currentData = this.imageData
    val resultColors = (0 until width * height).map: i =>
      transformer(currentData(i))

    result.setColorsFromSeq(resultColors)
    result
  end transformColorToColor

  def mergeWith(another: BufferAdapter[?], pixelMerger: (Color, Color) => Color): BufferAdapter[?] =
    val resultWidth  = width.min(another.width)
    val resultHeight = height.min(another.height)

    val thisData    = this.imageData
    val anotherData = another.imageData

    val resultColors = (0 until resultWidth * resultHeight).map: i =>
      pixelMerger(thisData(i), anotherData(i))

    val result = BufferAdapter(resultWidth, resultHeight)
    result.setColorsFromSeq(resultColors)
    result
  end mergeWith

  /** Creates a new `JVMBufferAdapter` instance that is a transformed version of the current buffer.
    * The transformation is applied using an `AffineTransformation`. The canvas can optionally be
    * resized based on the transformation.
    *
    * @param transformation
    *   The `AffineTransformation` to apply to the image.
    * @param backgroundColor
    *   The background color to use when clearing the canvas if resizing is necessary. Defaults to
    *   `DefaultBackgroundColor`.
    * @return
    *   A new `JVMBufferAdapter` instance containing the transformed image.
    */
  def createTransformedVersionWith(
      transformation: AffineTransformation,
      backgroundColor: Color = DefaultBackgroundColor
  ): BufferAdapter[?]

end BufferAdapter
