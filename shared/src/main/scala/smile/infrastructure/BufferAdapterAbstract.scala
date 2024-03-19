package smile.infrastructure

import smile.colors.Color

abstract class BufferAdapterAbstract:

  val width: Int
  val height: Int

  def imageData: Seq[Color]

  /** Sets the colors of the image from a sequence of `Color` objects.
    *
    * @param colors
    *   A sequence of `Color` objects to be applied to the image. The sequence must have exactly
    *   width * height items.
    */
  def setColorsFromSeq(
      colors: Seq[Color]
  ): Unit

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
  ): BufferAdapter

  def setColorsByLocation(generator: (Int, Int) => Color): Unit =
    val data = (0 until width * height).map: i =>
      val x = i % width
      val y = i / width
      generator(x, y)

    setColorsFromSeq(data)
  end setColorsByLocation

  def transformColorToColor(transformer: Color => Color): BufferAdapter =
    val result      = new BufferAdapter(width, height)
    val currentData = this.imageData
    val resultColors = (0 until width * height).map: i =>
      transformer(currentData(i))

    result.setColorsFromSeq(resultColors)
    result
  end transformColorToColor

  def mergeWith(another: BufferAdapter, pixelMerger: (Color, Color) => Color): BufferAdapter =
    val resultWidth  = width.min(another.width)
    val resultHeight = height.min(another.height)

    val thisData    = this.imageData
    val anotherData = another.imageData

    val resultColors = (0 until resultWidth * resultHeight).map: i =>
      pixelMerger(thisData(i), anotherData(i))

    val result = new BufferAdapter(resultWidth, resultHeight)
    result.setColorsFromSeq(resultColors)
    result
  end mergeWith

end BufferAdapterAbstract
