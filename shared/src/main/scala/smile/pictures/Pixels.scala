package smile.pictures

import smile.colors.Color
import smile.infrastructure.NativeBuffer

/** The pixels of a [[smile.pictures.Bitmap]] at one buffer pixel per logical pixel, as
  * [[smile.pictures.Bitmap.pixels]] computed them. The view never changes. Nothing it gives can be
  * written back into the bitmap or into the bitmaps that share its pixels.
  *
  * [[smile.pictures.Pixels.argbData]] is the fast path for reading many pixels. The other readers
  * make a [[smile.colors.Color]] per pixel.
  */
final class Pixels private[smile] (private val buffer: NativeBuffer):

  /** The number of pixels in a row. */
  val width: Int = buffer.width

  /** The number of rows. */
  val height: Int = buffer.height

  /** All pixels as packed ARGB integers in row-major order, in a new array on every call, so the
    * caller may change it.
    */
  def argbData: Array[Int] = buffer.argbData

  /** All pixels' colors in row-major order. */
  def colors: IndexedSeq[Color] = buffer.imageData.toIndexedSeq

  /** The color of the pixel at column `x` and row `y`, or `None` outside the pixels. */
  def colorAt(x: Int, y: Int): Option[Color] =
    if x < 0 || x >= width || y < 0 || y >= height then None
    else Some(buffer.pixelColor(x, y))

  override def toString: String = s"Pixels($width x $height)"
end Pixels
