package smile.infrastructure

import smile.modeling.Angles

/** Constants for the colors the library works with.
  */
private[smile] object Constants:
  val MinimumRed: Int = 0

  val MaximumRed: Int = 255

  val MinimumGreen: Int = 0

  val MaximumGreen: Int = 255

  val MinimumBlue: Int = 0

  val MaximumBlue: Int = 255

  val MinimumOpacity: Int = 0

  val MaximumOpacity: Int = 255

  val MinimumHSIHue: Double = 0.0

  val MaximumHSIHue: Double = Angles.FullTurn

  val MinimumHSISaturation: Double = 0.0

  val MaximumHSISaturation: Double = 1.0

  val MinimumHSIIntensity: Double = 0.0

  val MaximumHSIIntensity: Double = 255.0

  /** The factor by which `Color.darker` shades colors. */
  val DefaultShadingFactor = 0.1

  /** The factor by which `Color.lighter` tints colors. */
  val DefaultTintingFactor = 0.1
