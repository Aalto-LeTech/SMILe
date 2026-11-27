package smile.infrastructure

import smile.colors.Color

private[smile] object BufferAdapter:
  def apply(width: Int, height: Int): NativeBuffer =
    PlatformSpecific.bufferAdapter(width, height)

  /** A fresh 1x1 buffer representing "no content". A new instance is returned on every access
    * because the platform image a buffer hands out through `get` can still be drawn on, so a shared
    * singleton could be corrupted.
    */
  def Empty: NativeBuffer = apply(1, 1)

/** A bitmap's size, reads of its pixels and the setters that fill it. The image operations live in
  * `smile.infrastructure.ImageOperations`, so that a buffer that only stores pixels need not
  * implement them.
  *
  * A buffer is not changed after it has been made. Every operation returns a new buffer. Only SMILe
  * itself fills a buffer it has just created, through the setters it keeps to itself.
  */
private[smile] trait BufferAdapter:

  val width: Int
  val height: Int

  def imageData: Seq[Color]

  /** Saves the buffer as a PNG file, on platforms that support direct file access.
    *
    * @return
    *   `true` if the image was saved. `false` if saving failed or the platform does not support it.
    */
  def saveToPath(path: String): Boolean = false

  /** Overwrites all pixels.
    *
    * @param colors
    *   Exactly width * height values.
    */
  private[smile] def setColorsFromSeq(
      colors: Seq[Color]
  ): Unit

  def pixelColor(x: Int, y: Int): Color

  private[smile] def setRGBA(x: Int, y: Int, color: Color): Unit

  /** Returns all pixels as packed ARGB integers in row-major order. The bulk counterpart of
    * `smile.infrastructure.BufferAdapter.imageData` for pixel operations that would otherwise box a
    * [[smile.colors.Color]] per pixel.
    */
  def argbData: Array[Int]

  /** Returns the top-left rectangle as packed ARGB integers with a compact row-major layout.
    * Dimensions must be nonnegative and within this buffer. A zero-area rectangle is empty. The
    * returned array is independent of the source's pixel storage.
    *
    * Platforms can override this compatibility implementation to read only the requested pixels.
    */
  def argbData(regionWidth: Int, regionHeight: Int): Array[Int] =
    requireARGBRegion(regionWidth, regionHeight)
    if regionWidth == 0 || regionHeight == 0 then return Array.emptyIntArray
    val source = argbData
    val result = new Array[Int](regionWidth * regionHeight)
    var y      = 0
    while y < regionHeight do
      Array.copy(source, y * width, result, y * regionWidth, regionWidth)
      y += 1
    result

  protected final def requireARGBRegion(regionWidth: Int, regionHeight: Int): Unit =
    require(
      regionWidth >= 0 && regionHeight >= 0 && regionWidth <= width && regionHeight <= height,
      s"Requested ARGB region ${regionWidth}x$regionHeight is outside the ${width}x$height buffer"
    )

  /** Overwrites all pixels from packed ARGB integers in row-major order.
    *
    * @param data
    *   Exactly width * height values.
    */
  private[smile] def setARGBData(data: Array[Int]): Unit

  /** Populates the buffer from packed ARGB values in row-major callback order. The destination is
    * updated only after all callbacks succeed. Platforms may fuse generation with native storage.
    */
  private[smile] def setARGBByLocation(generator: (Int, Int) => Int): Unit =
    val data = new Array[Int](width * height)
    var y    = 0
    var i    = 0
    while y < height do
      var x = 0
      while x < width do
        data(i) = generator(x, y)
        i += 1
        x += 1
      y += 1
    setARGBData(data)

  private[smile] def setColorsByLocation(generator: (Int, Int) => Color): Unit =
    setARGBByLocation((x, y) => generator(x, y).toARGBInt)
  end setColorsByLocation

end BufferAdapter
