package smile.infrastructure

import scala.collection.immutable.Range.Inclusive

object Constants:
  // DEGREES

  /** Number of degrees representing a full circle. */
  val FullCircleInDegrees: Int = 360

  /** Number of degrees representing a full circle. */
  val OneThirdOfCircleInDegrees: Int = FullCircleInDegrees / 3

  /** Number of degrees representing a half of a circle. */
  val OneHalfOfCircleInDegrees: Int = 180

  /** Number of degrees representing a quarter of a circle in the clockwise direction. */
  val OneQuarterOfCircleInDegreesClockwise: Int = -90

  /** Number of degrees representing a quarter of a circle in the counter-clockwise direction. */
  val OneQuarterOfCircleInDegreesCounterClockwise: Int = 90

  /** 60 degrees in radians. */
  val Deg60InRad: Double = Math.PI / 3.0

  // COLORS

  private val ColorValueRange: Inclusive = 0 to 255

  /** An RGB color component value representing minimal amount of red. */
  val MinimumRed: Int = ColorValueRange.start

  /** An RGB color component value representing maximal amount of red. */
  val MaximumRed: Int = ColorValueRange.end

  /** Range for valid values of the red color component of an RGB color. */
  val RedRange: Range.Inclusive = ColorValueRange

  /** An RGB color component value representing minimal amount of green. */
  val MinimumGreen: Int = ColorValueRange.start

  /** An RGB color component value representing maximal amount of green. */
  val MaximumGreen: Int = ColorValueRange.end

  /** Range for valid values of the green color component of an RGB color. */
  val GreenRange: Range.Inclusive = ColorValueRange

  /** An RGB color component value representing minimal amount of blue. */
  val MinimumBlue: Int = ColorValueRange.start

  /** An RGB color component value representing maximal amount of blue. */
  val MaximumBlue: Int = ColorValueRange.end

  /** Range for valid values of the blue color component of an RGB color. */
  val BlueRange: Range.Inclusive = ColorValueRange

  /** A color component value representing minimal amount of gray. */
  val MinimumGray: Int = ColorValueRange.start

  /** A color component value representing maximal amount of gray. */
  val MaximumGray: Int = ColorValueRange.end

  /** Range for valid values of gray color component. */
  val GrayRange: Range.Inclusive = ColorValueRange

  /** An RGBA color component value representing minimal opacity. */
  val MinimumOpacity: Int = ColorValueRange.start

  /** Color component value representing minimal opacity. */
  val FullyTransparent: Int = MinimumOpacity

  /** An RGBA color component value representing maximal opacity. */
  val MaximumOpacity: Int = ColorValueRange.end

  /** Color component value representing maximal opacity. */
  val FullyOpaque: Int = MaximumOpacity

  /** Range for valid values of the opacity color component of an RGBA color. */
  val OpacityRange: Range.Inclusive = ColorValueRange

  /** Color component value representing normalized minimal amount of red. */
  val MinimumNormalizedRed: Double = 0.0

  /** Color component value representing normalized maximal amount of red. */
  val MaximumNormalizedRed: Double = 1.0

  /** Color component value representing normalized minimal amount of green. */
  val MinimumNormalizedGreen: Double = 0.0

  /** Color component value representing normalized maximal amount of green. */
  val MaximumNormalizedGreen: Double = 1.0

  /** Color component value representing normalized minimal amount of blue. */
  val MinimumNormalizedBlue: Double = 0.0

  /** Color component value representing normalized maximal amount of blue. */
  val MaximumNormalizedBlue: Double = 1.0

  /** Color component value representing normalized minimal amount of gray. */
  val MinimumNormalizedGray: Double = 0.0

  /** Color component value representing normalized maximal amount of gray. */
  val MaximumNormalizedGray: Double = 1.0

  /** Color component value representing normalized minimal opacity. */
  val MinimumNormalizedOpacity: Double = 0.0

  /** Color component value representing normalized maximal opacity. */
  val MaximumNormalizedOpacity: Double = 1.0

  /** Color component value representing start of the hue cycle. */
  val MinimumHSIHue: Double = 0.0

  /** Color component value representing end of the hue cycle. */
  val MaximumHSIHue: Double = FullCircleInDegrees.toDouble

  /** Color component value representing an undefined hue (at the center of the hue cycle). */
  val UndefinedHSIHue: Double = Double.NaN

  /** Color component value representing minimal amount of saturation. */
  val MinimumHSISaturation: Double = 0.0

  /** Color component value representing maximal amount of saturation. */
  val MaximumHSISaturation: Double = 1.0

  /** Color component value representing minimal amount of intensity. */
  val MinimumHSIIntensity: Double = ColorValueRange.start.toDouble

  /** Color component value representing maximal amount of intensity. */
  val MaximumHSIIntensity: Double = ColorValueRange.end.toDouble

  /** Color component value representing start of the hue cycle. */
  val MinimumHSVHue: Double = 0.0

  /** Color component value representing end of the hue cycle. */
  val MaximumHSVHue: Double = FullCircleInDegrees.toDouble

  /** Color component value representing an undefined hue (at the center of the hue cycle). */
  val UndefinedHSVHue: Double = Double.NaN

  /** Color component value representing minimal amount of saturation. */
  val MinimumHSVSaturation: Double = 0.0

  /** Color component value representing maximal amount of saturation. */
  val MaximumHSVSaturation: Double = 1.0

  /** Color component value representing minimal amount of intensity. */
  val MinimumHSVValue: Double = ColorValueRange.start.toDouble

  /** Color component value representing maximal amount of intensity. */
  val MaximumHSVValue: Double = ColorValueRange.end.toDouble

  val DefaultShadingFactor = 0.1

  /** The factor the amount of which colors are tinted by the lighter() method. */
  val DefaultTintingFactor = 0.1
