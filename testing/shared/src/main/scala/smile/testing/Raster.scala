package smile.testing

import smile.RenderSettings
import smile.pictures.Element

/** A rendered picture as non-premultiplied ARGB pixels in rows, which is what both platforms'
  * buffers hand out.
  */
final case class Raster(width: Int, height: Int, argb: Array[Int]):
  require(argb.length == width * height, s"${argb.length} pixels for a ${width}x$height raster")

  def apply(x: Int, y: Int): Int = argb(y * width + x)

  /** The pixel at a position, or transparent beyond the edges. */
  def at(x: Int, y: Int): Int =
    if x < 0 || y < 0 || x >= width || y >= height then 0 else argb(y * width + x)

object Raster:
  /** Renders a picture with this platform's bitmap renderer, which is AWT on the JVM and canvas on
    * Scala.js.
    *
    * @param scale
    *   Device pixels per logical pixel.
    */
  def of(
      picture: Element,
      settings: RenderSettings = RenderSettings.Default,
      scale: Double = 1.0
  ): Raster =
    val bitmap = smile.infrastructure.Renderer().createBitmapFrom(picture, scale, settings)
    val pixels = bitmap.pixels(settings)
    Raster(pixels.width, pixels.height, pixels.argbData)
