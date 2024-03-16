package smile

import smile.colors.Color
import smile.colors.PresetColor.Transparent
import smile.modeling.*

import java.awt.Image
import java.awt.image.AffineTransformOp

/** Settings Configuration for the SMILe Graphics Library.
  *
  * This object contains global settings affecting the behavior and appearance of graphical elements
  * created using the SMILe library. These settings can be modified to change default behaviors
  * across various operations and elements.
  */
object Settings:
  /** Enumeration of the different methods used for scaling of bitmaps. See [[java.awt.Image]] for
    * details.
    */
  enum ScalingMethod(val value: Int):
    case Smooth          extends ScalingMethod(Image.SCALE_SMOOTH)
    case Fast            extends ScalingMethod(Image.SCALE_FAST)
    case AreaAveraging   extends ScalingMethod(Image.SCALE_AREA_AVERAGING)
    case NearestNeighbor extends ScalingMethod(-1000)

  /** Enumeration of the different methods used for transforming bitmaps. See
    * [[java.awt.image.AffineTransformOp]] for details.
    */
  enum TransformMethod(val value: Int):
    case Bilinear extends TransformMethod(AffineTransformOp.TYPE_BILINEAR)
    case Nearest  extends TransformMethod(AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
    case Bicubic  extends TransformMethod(AffineTransformOp.TYPE_BICUBIC)

  var DrawingIsAntiAliased: Boolean                     = true
  var BufferScalingMethod: ScalingMethod                = ScalingMethod.Smooth
  var BufferTransformMethod: TransformMethod            = TransformMethod.Bilinear
  var DefaultPaddingInPixels: Double                    = 0
  var DefaultBackgroundColor: Color                     = Transparent
  var DefaultPositionType: PositionType                 = PositionType.Center
  var DefaultHorizontalAlignment: HorizontalAlignment   = HorizontalAlignment.Left
  var DefaultVerticalAlignment: VerticalAlignment       = VerticalAlignment.Middle
  var DefaultPosition: Pos                              = Pos.Origin
  var DefaultRotationAngle: Angle                       = Angle.Zero
  var DefaultRotationAngleInDegrees: Double             = DefaultRotationAngle.inDegrees
