package smile.infrastructure

import smile.RenderSettings
import smile.colors.Color
import smile.modeling.Affine

private[smile] object ImageOperations:

  /** The size of the buffer that `smile.infrastructure.ImageOperations.transform` makes when it
    * transforms a buffer of the given size, computed without transforming anything.
    */
  private[smile] def transformedSize(
      width: Int,
      height: Int,
      transformation: Affine
  ): (Int, Int) =
    val (newWidth, newHeight, _) = transformedPlacement(width, height, transformation)
    (newWidth, newHeight)

  /** The buffer that `smile.infrastructure.ImageOperations.transform` makes when it transforms a
    * buffer of the given size. The result is its size, the extent of the transformed corners
    * rounded down to whole pixels and at least one, and `transformation` followed by the move that
    * puts the middle of that extent in the middle of the buffer. Both platforms transform with it,
    * so they agree on the size and on where the pixels land.
    */
  private[smile] def transformedPlacement(
      width: Int,
      height: Int,
      transformation: Affine
  ): (Int, Int, Affine) =
    val corners =
      Seq(
        (0.0, 0.0),
        (width.toDouble, 0.0),
        (0.0, height.toDouble),
        (width.toDouble, height.toDouble)
      )
    val xs = corners.map(transformation.mapX)
    val ys = corners.map(transformation.mapY)
    // The size along an axis, and the move along it from the middle of the extent to the middle
    // of the buffer.
    def fitted(values: Seq[Double]): (Int, Double) =
      val size = Math.floor(values.max - values.min).toInt.max(1)
      (size, (size - values.min - values.max) / 2)
    val (newWidth, dx)  = fitted(xs)
    val (newHeight, dy) = fitted(ys)
    (newWidth, newHeight, transformation.translatedBy(dx, dy))
  end transformedPlacement

/** The image operations of a platform buffer, such as scaling, cropping and transforming, kept
  * apart from the storage in `smile.infrastructure.BufferAdapter` so that a buffer that only stores
  * pixels need not implement them. Every operation returns a new buffer.
  */
private[smile] trait ImageOperations extends BufferAdapter:

  /** A new buffer of these pixels scaled to the target size.
    *
    * @param settings
    *   Supplies the interpolation method (see [[smile.RenderSettings]]).
    */
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double,
      settings: RenderSettings = RenderSettings.Default
  ): NativeBuffer

  /** Copies the portion between two corners, which may be given in either order. */
  def copyPortionXYXY(
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  ): NativeBuffer =
    val (x0, x1) = if left > right then (right, left) else (left, right)
    val (y0, y1) = if top > bottom then (bottom, top) else (top, bottom)

    val width  = x1 - x0
    val height = y1 - y0

    copyPortionXYWH(x0, y0, width, height)

  def copyPortionXYWH(
      topLeftX: Double,
      topLeftY: Double,
      width: Double,
      height: Double
  ): NativeBuffer

  /** A new buffer of these pixels moved right by `dx` and down by `dy`, with the ones that leave it
    * on one side coming back on the other. Each offset is at least zero and less than the size
    * along its axis. The pixels are copied as they are.
    */
  private[smile] def wrapShifted(dx: Int, dy: Int): NativeBuffer =
    val source  = argbData
    val shifted = new Array[Int](source.length)
    var y       = 0
    while y < height do
      val from = y * width
      val to   = ((y + dy) % height) * width
      // The row's part that stays inside the buffer, then the part that wraps around to its start.
      Array.copy(source, from, shifted, to + dx, width - dx)
      Array.copy(source, from + width - dx, shifted, to, dx)
      y += 1
    val result = BufferAdapter(width, height)
    result.setARGBData(shifted)
    result

  /** Transforms every pixel with a function on packed ARGB integers. It backs
    * `smile.infrastructure.ImageOperations.transformColorToColor` and boxes no
    * [[smile.colors.Color]] per pixel.
    */
  def transformARGB(transformer: Int => Int): NativeBuffer =
    val data = argbData
    var i    = 0
    while i < data.length do
      data(i) = transformer(data(i))
      i += 1

    val result = BufferAdapter(width, height)
    result.setARGBData(data)
    result
  end transformARGB

  /** Transforms every pixel with a function on [[smile.colors.Color]] instances. A thin wrapper
    * over `smile.infrastructure.ImageOperations.transformARGB`, which avoids boxing a `Color` per
    * pixel.
    */
  def transformColorToColor(transformer: Color => Color): NativeBuffer =
    transformARGB(argb => transformer(new Color(argb)).toARGBInt)

  /** Merges this Buffer with another Buffer using a merging function on packed ARGB integers. It
    * backs `smile.infrastructure.ImageOperations.mergeWith` and boxes no [[smile.colors.Color]] per
    * pixel.
    *
    * @return
    *   A new buffer whose dimensions are the element-wise minimum of the two sources'.
    */
  def mergeARGBWith(another: BufferAdapter, merger: (Int, Int) => Int): NativeBuffer =
    val resultWidth  = width.min(another.width)
    val resultHeight = height.min(another.height)

    val thisData    = this.argbData(resultWidth, resultHeight)
    val anotherData = another.argbData(resultWidth, resultHeight)

    // Both reads have a compact row stride, even when the source widths differ.
    val merged = new Array[Int](resultWidth * resultHeight)
    var i      = 0
    while i < merged.length do
      merged(i) = merger(thisData(i), anotherData(i))
      i += 1

    val result = BufferAdapter(resultWidth, resultHeight)
    result.setARGBData(merged)
    result
  end mergeARGBWith

  /** Merges this Buffer with another Buffer using a specified pixel merging function. A thin
    * wrapper over `smile.infrastructure.ImageOperations.mergeARGBWith`, which avoids boxing two
    * `Color`s per pixel.
    */
  def mergeWith(another: BufferAdapter, pixelMerger: (Color, Color) => Color): NativeBuffer =
    mergeARGBWith(
      another,
      (thisARGB, anotherARGB) => pixelMerger(new Color(thisARGB), new Color(anotherARGB)).toARGBInt
    )

  /** Creates a new `BufferAdapter` instance that is a transformed version of the current buffer,
    * sized and placed by `smile.infrastructure.ImageOperations.transformedPlacement`.
    *
    * @param settings
    *   Supplies the interpolation method (see [[smile.RenderSettings]]). The area the
    *   transformation leaves uncovered is transparent.
    */
  def transform(
      transformation: Affine,
      settings: RenderSettings = RenderSettings.Default
  ): NativeBuffer

end ImageOperations
