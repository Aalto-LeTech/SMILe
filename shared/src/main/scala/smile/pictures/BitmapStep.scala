package smile.pictures

import smile.RenderSettings
import smile.infrastructure.{ImageOperations, NativeBuffer}
import smile.modeling.{Affine, Angles}

/** One operation a `smile.pictures.PixelSource` has recorded, in the pixels of the buffer it
  * applies to. A bitmap's pixels are its source's pixels with its steps replayed in order by the
  * buffer algorithms. Scaling and turning resample with the methods of the settings the pixels are
  * asked for, flips are drawn with the default settings, and crops and wraparound shifts copy
  * pixels.
  */
private[smile] enum BitmapStep:

  /** Resampling to a size with `bufferScalingMethod`. The size is never negative, because a
    * mirroring scale records a flip before it.
    */
  case Scale(targetWidth: Double, targetHeight: Double)

  /** Turning by an angle in degrees around the buffer's center with `bufferTransformMethod`, onto a
    * buffer the size of the turned extent.
    */
  case Rotate(angle: Double)

  /** Mirroring across the vertical axis through the buffer's center. */
  case FlipH

  /** Mirroring across the horizontal axis through the buffer's center. */
  case FlipV

  /** Turning by 180 degrees around the buffer's center. */
  case FlipD

  /** The whole pixels from column `left` and row `top` up to, but not including, column `right` and
    * row `bottom`.
    */
  case Crop(left: Int, top: Int, right: Int, bottom: Int)

  /** Moving the pixels right by `dx` and down by `dy`, with the ones that leave the buffer on one
    * side coming back on the other. Both offsets are at least zero and less than the buffer's size
    * along their axis, so that consecutive shifts fold into one.
    */
  case WrapShift(dx: Int, dy: Int)

  /** The size of the buffer this step makes from a buffer of the given size, known without making
    * it.
    */
  def sizeAfter(width: Int, height: Int): (Int, Int) = this match
    case Scale(targetWidth, targetHeight) => (targetWidth.toInt.abs, targetHeight.toInt.abs)
    case Rotate(angle)                    =>
      ImageOperations.transformedSize(width, height, BitmapStep.rotation(angle, width, height))
    case FlipH | FlipV | FlipD =>
      ImageOperations.transformedSize(width, height, BitmapStep.flip(this, width, height))
    case Crop(left, top, right, bottom) => (right - left, bottom - top)
    case WrapShift(_, _)                => (width, height)

  /** The buffer this step makes from `buffer`, resampling with the methods of `settings`, except
    * that a flip always uses the default settings.
    */
  def replayOn(buffer: NativeBuffer, settings: RenderSettings): NativeBuffer = this match
    case Scale(targetWidth, targetHeight) => buffer.scaleTo(targetWidth, targetHeight, settings)
    case Rotate(angle)                    =>
      buffer.transform(BitmapStep.rotation(angle, buffer.width, buffer.height), settings)
    // A flip is drawn by the platform with the default settings, whatever the output's. A canvas
    // that copied the pixels through their unpremultiplied values could store translucent ones
    // differently, and a smoothing quality other than the default's is not guaranteed to land each
    // pixel exactly on another.
    case FlipH | FlipV | FlipD =>
      buffer.transform(BitmapStep.flip(this, buffer.width, buffer.height), RenderSettings.Default)
    case Crop(left, top, right, bottom) => buffer.copyPortionXYXY(left, top, right, bottom)
    case WrapShift(dx, dy)              => buffer.wrapShifted(dx, dy)
end BitmapStep

private[smile] object BitmapStep:

  /** The turn of a buffer of `width` by `height` pixels by `angle` degrees around its middle,
    * counterclockwise on screen for a positive angle. The buffer's transform fits its result to the
    * turned pixels, so only the linear part matters.
    */
  private def rotation(angle: Double, width: Int, height: Int): Affine =
    val (sin, cos) = Angles.exactSinCos(angle)
    val (px, py)   = (width / 2.0, height / 2.0)
    Affine.unchecked(cos, -sin, sin, cos, px - sin * py - cos * px, py + sin * px - cos * py)

  /** The flip of a buffer of `width` by `height` pixels that `step` records, onto the pixels left
    * and above it, which the buffer's transform then fits its result to.
    */
  private def flip(step: BitmapStep, width: Int, height: Int): Affine = step match
    case FlipH => Affine.unchecked(-1, 0, 0, 1, -width.toDouble, 0)
    case FlipV => Affine.unchecked(1, 0, 0, -1, 0, -height.toDouble)
    case _     => Affine.unchecked(-1, 0, 0, -1, -width.toDouble, -height.toDouble)

end BitmapStep
