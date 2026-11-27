package smile

import java.awt.Image

/** How bitmaps are resampled when a bitmap's buffer is scaled (`Bitmap.scaleBy`, `scaleTo`) and
  * when a renderer draws a bitmap at another size than its buffer's, as under a display scale
  * factor or in an SVG that is shown enlarged. AWT, canvas and SVG's `image-rendering` all follow
  * the method, and each method means the same on both platforms as far as each renderer can tell
  * them apart:
  *
  *   - `Smooth`, the default, interpolates bilinearly between the pixels, so an enlarged bitmap
  *     blends from each pixel into the next.
  *   - `NearestNeighbor` and `Fast` copy the nearest pixel, so an enlarged bitmap is made of
  *     blocks.
  *   - `AreaAveraging` averages the area each new pixel covers, which is the better way to make a
  *     bitmap smaller and enlarges into blocks. Only the JVM's buffer scaling has it (see
  *     [[java.awt.Image]]). On Scala.js, and where a renderer resamples as it draws, it is smooth.
  *
  * The method in effect is the `bufferScalingMethod` of the [[smile.RenderSettings]] a renderer is
  * given, and the default's for a bitmap's own scaling.
  */
enum ScalingMethod(private[smile] val value: Int) derives CanEqual:
  case Smooth          extends ScalingMethod(Image.SCALE_SMOOTH)
  case Fast            extends ScalingMethod(Image.SCALE_FAST)
  case AreaAveraging   extends ScalingMethod(Image.SCALE_AREA_AVERAGING)
  case NearestNeighbor extends ScalingMethod(-1000)

  private[smile] def copiesNearestPixel: Boolean = this == NearestNeighbor || this == Fast
