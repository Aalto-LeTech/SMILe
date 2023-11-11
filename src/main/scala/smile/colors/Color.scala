package smile.colors

import smile.infrastructure.Constants.*

import java.awt.Color as LowLevelColor

object Color:
  def fromHSI(
      hue: Double,
      saturation: Double,
      intensity: Double,
      opacity: Int = MaximumOpacity
  ): Color =
    validateHSI(hue, saturation, intensity, opacity)
    val (r, g, b) = hsiToRGB(hue, saturation, intensity)
    new Color(r, g, b)

  def rgbToHSI(red: Int, green: Int, blue: Int): (Double, Double, Double) =
    validateRGBA(red, green, blue)

    val rgbSum: Double = red + green + blue

    val hue: Double =
      if isGray(red, green, blue) then // Not defined for grays
        UndefinedHSIHue
      else
        val RedMinusGreen = red - green
        val RedMinusBlue  = red - blue

        val root = Math.sqrt(RedMinusGreen * RedMinusGreen + RedMinusBlue * (green - blue))

        val angleCandidate =
          Math.toDegrees(Math.acos((RedMinusGreen + RedMinusBlue) / (2.0 * root)))

        if green >= blue then angleCandidate else FullCircleInDegrees - angleCandidate

    val saturation: Double =
      if isBlack(red, green, blue) then MinimumHSISaturation
      else 1.0 - 3.0 * (min(red, green, blue) / rgbSum)

    val intensity = rgbSum / 3.0

    (hue, saturation, intensity)
  end rgbToHSI

  def hsiToRGB(hueInDegrees: Double, saturation: Double, intensity: Double): (Int, Int, Int) =
    validateHSI(hueInDegrees, saturation, intensity)

    // Special case
    if saturation == MinimumHSVSaturation then
      val i: Int = Math.round(intensity).toInt
      return (i, i, i)

    val nHueInDeg = normalizedHSIHueInDegreesFrom(hueInDegrees)

    val (aThirdOfCircleHueInDegrees: Double, finalOrder: ((Int, Int, Int) => (Int, Int, Int))) =
      if nHueInDeg <= 120.0 then (nHueInDeg, (x: Int, y: Int, z: Int) => (x, z, y))
      else if nHueInDeg <= 240.0 then (nHueInDeg - 120.0, (x: Int, y: Int, z: Int) => (y, x, z))
      else (nHueInDeg - 240.0, (x: Int, y: Int, z: Int) => (z, y, x))

    val quotient = (saturation *
      Math.toDegrees(
        Math.cos(Math.toRadians(aThirdOfCircleHueInDegrees))
      )) / Math.toDegrees(
      Math.cos(Math.toRadians(60.0 - aThirdOfCircleHueInDegrees))
    )
    val X = Math.round(intensity * (1 + quotient)).toInt
    val Y = Math.round(intensity - intensity * saturation).toInt
    val Z = Math.round(3.0 * intensity - X - Y).toInt

    val (red, green, blue) = finalOrder(X, Y, Z)

    (red, green, blue)
  end hsiToRGB

  def argbIntFrom(red: Int, green: Int, blue: Int, opacity: Int = MaximumOpacity) =
    validateRGBA(red, green, blue, opacity)
    (opacity << 24) | (red << 16) | (green << 8) | blue

  private def normalizedHSIHueInDegreesFrom(hueCandidateInDegrees: Double): Double =
    val modCircle = hueCandidateInDegrees % FullCircleInDegrees
    if modCircle < 0 then FullCircleInDegrees + modCircle else modCircle
  end normalizedHSIHueInDegreesFrom

  def isGray(red: Int, green: Int, blue: Int): Boolean =
    red == blue && green == blue

  def isBlack(red: Int, green: Int, blue: Int): Boolean =
    red == MinimumRed &&
      green == MinimumGreen &&
      blue == MinimumBlue

  private def min(red: Int, green: Int, blue: Int): Int =
    red.min(green).min(blue)

  private def validateRGBA(red: Int, green: Int, blue: Int, opacity: Int = 255): Unit =
    require(red >= MinimumRed && red <= MaximumRed, "Red must be between 0 and 255")
    require(green >= MinimumGreen && green <= MaximumGreen, "Green must be between 0 and 255")
    require(blue >= MinimumBlue && blue <= MaximumBlue, "Blue must be between 0 and 255")
    require(
      opacity >= MinimumOpacity && opacity <= MaximumOpacity,
      "Opacity must be between 0 and 255"
    )

  private def validateHSI(
      hue: Double,
      saturation: Double,
      intensity: Double,
      opacity: Int = 255
  ): Unit =
    require(hue >= MinimumHSIHue && hue <= MaximumHSIHue, "Hue must be between 0 and 360")
    require(
      saturation >= MinimumHSISaturation && saturation <= MaximumHSISaturation,
      "Saturation must be between 0 and 1"
    )
    require(
      intensity >= MinimumHSIIntensity && intensity <= MaximumHSIIntensity,
      "Intensity must be between 0 and 255"
    )
    require(
      opacity >= MinimumOpacity && opacity <= MaximumOpacity,
      "Opacity must be between 0 and 255"
    )

end Color

class Color(
    val red: Int,
    val green: Int,
    val blue: Int,
    val opacity: Int = MaximumOpacity,
    val canonicalName: Option[String] = None
):
  Color.validateRGBA(red, green, blue, opacity)

  def this(red: Int, green: Int, blue: Int, opacity: Int, name: String) =
    this(red, green, blue, opacity, Some(name))

  def this(argb: Int, canonicalName: Option[String]) =
    this((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff, (argb >> 24) & 0xff, canonicalName)

  def this(argb: Int) =
    this(argb, None)

  def isOpaque: Boolean = opacity == MaximumOpacity

  def toHSI(argbInt: Int): (Double, Double, Double) =
    Color.rgbToHSI(red, green, blue)

  def toAWTColor: LowLevelColor = new LowLevelColor(red, green, blue, opacity)

  def toARGBInt: Int = Color.argbIntFrom(red, green, blue, opacity)

  def lighter: Color = tintByFactor(DefaultTintingFactor)

  def darker: Color = shadeByFactor(DefaultShadingFactor)

  def tintByFactor(tintingFactorFromZeroToOne: Double): Color =
    val newRed = (red + tintingFactorFromZeroToOne * (MaximumRed - red)).ceil.toInt
    val newGreen =
      (green + tintingFactorFromZeroToOne * (MaximumGreen - green)).ceil.toInt
    val newBlue =
      (blue + tintingFactorFromZeroToOne * (MaximumBlue - blue)).ceil.toInt

    new Color(newRed, newGreen, newBlue, opacity)
  end tintByFactor

  def shadeByFactor(shadingFactorFromZeroToOne: Double): Color =
    val invertedFactor: Double = 1.0 - shadingFactorFromZeroToOne
    val newRed                 = (invertedFactor * red).toInt
    val newGreen               = (invertedFactor * green).toInt
    val newBlue                = (invertedFactor * blue).toInt

    new Color(newRed, newGreen, newBlue, opacity)
  end shadeByFactor

  def tintByPercentage(tintingFactorInPercents: Double): Color =
    require(
      tintingFactorInPercents >= 0.0 && tintingFactorInPercents <= 100.0,
      "Tinting factor must be between 0 and 100"
    )

    tintByFactor(tintingFactorInPercents / 100.0)
  end tintByPercentage

  def shadeByPercentage(shadingFactorInPercents: Double): Color =
    require(
      shadingFactorInPercents >= 0.0 && shadingFactorInPercents <= 100.0,
      "Shading factor must be between 0 and 100"
    )

    shadeByFactor(shadingFactorInPercents / 100.0)
  end shadeByPercentage
