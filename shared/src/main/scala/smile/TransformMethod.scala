package smile

import java.awt.image.AffineTransformOp

/** How bitmaps are resampled when they are affinely transformed, as `Bitmap.rotateAround` does. See
  * [[java.awt.image.AffineTransformOp]] for details. The method in effect is the
  * `bufferTransformMethod` of the default [[smile.RenderSettings]].
  */
enum TransformMethod(private[smile] val value: Int) derives CanEqual:
  case Bilinear extends TransformMethod(AffineTransformOp.TYPE_BILINEAR)
  case Nearest  extends TransformMethod(AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
  case Bicubic  extends TransformMethod(AffineTransformOp.TYPE_BICUBIC)
