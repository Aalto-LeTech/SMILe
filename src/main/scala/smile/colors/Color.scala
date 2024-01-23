package smile.colors

import smile.infrastructure.Constants.*

import java.awt.Color as LowLevelColor

/** Provides utility methods for creating and converting colors in various color spaces and formats,
  * including HSI (Hue, Saturation, Intensity) and RGBA (Red, Green, Blue, Alpha). It also supports
  * operations to lighten or darken colors.
  *
  * @see
  *   [[java.awt.Color]]
  */
object Color:

  /** Creates a `Color` instance from HSI values with optional opacity. Validates the input values
    * for HSI and opacity before conversion.
    *
    * @param hue
    *   The hue component of the color, in degrees [0, 360).
    * @param saturation
    *   The saturation component of the color, in the range [0, 1].
    * @param intensity
    *   The intensity component of the color, in the range [0, 255].
    * @param opacity
    *   The opacity of the color, in the range [0, 255]. Default is [[smile.infrastructure.Constants.MaximumOpacity]].
    * @return
    *   A new `Color` instance based on the specified HSI values and opacity.
    */
  def fromHSI(
      hue: Double,
      saturation: Double,
      intensity: Double,
      opacity: Int = MaximumOpacity
  ): Color =
    validateHSI(hue, saturation, intensity, opacity)
    val (r, g, b) = hsiToRGB(hue, saturation, intensity)
    new Color(r, g, b)

  /** Converts RGB values to HSI (Hue, Saturation, Intensity). Validates the input RGB values before
    * conversion.
    *
    * @param red
    *   The red component, in the range [0, 255].
    * @param green
    *   The green component, in the range [0, 255].
    * @param blue
    *   The blue component, in the range [0, 255].
    * @return
    *   A tuple containing the HSI components: (hue, saturation, intensity).
    */
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

        val angleCandidate = Math
          .toDegrees(Math.acos((RedMinusGreen + RedMinusBlue) / (2.0 * root)))

        if green >= blue then angleCandidate
        else FullCircleInDegrees - angleCandidate

    val saturation: Double =
      if isBlack(red, green, blue) then MinimumHSISaturation
      else 1.0 - 3.0 * (min(red, green, blue) / rgbSum)

    val intensity = rgbSum / 3.0

    (hue, saturation, intensity)
  end rgbToHSI

  /** Converts HSI values to RGB. Validates the input HSI values before conversion.
    *
    * @param hueInDegrees
    *   The hue component of the color, in degrees [0, 360).
    * @param saturation
    *   The saturation component of the color, in the range [0, 1].
    * @param intensity
    *   The intensity component of the color, in the range [0, 255].
    * @return
    *   A tuple containing the RGB components: (red, green, blue).
    */
  def hsiToRGB(hueInDegrees: Double, saturation: Double, intensity: Double): (Int, Int, Int) =
    validateHSI(hueInDegrees, saturation, intensity)

    // Special case
    if saturation == MinimumHSVSaturation then
      val i: Int = Math.round(intensity).toInt
      return (i, i, i)

    val nHueInDeg = normalizedHSIHueInDegreesFrom(hueInDegrees)

    type OrderingFunction = (Int, Int, Int) => (Int, Int, Int)
    
    val (aThirdOfCircleHueInDegrees: Double, finalOrder: OrderingFunction) =
      if nHueInDeg <= 120.0 then (nHueInDeg, (x: Int, y: Int, z: Int) => (x, z, y))
      else if nHueInDeg <= 240.0 then (nHueInDeg - 120.0, (x: Int, y: Int, z: Int) => (y, x, z))
      else (nHueInDeg - 240.0, (x: Int, y: Int, z: Int) => (z, y, x))

    val quotient =
      (saturation * Math.toDegrees(Math.cos(Math.toRadians(aThirdOfCircleHueInDegrees)))) /
        Math.toDegrees(Math.cos(Math.toRadians(60.0 - aThirdOfCircleHueInDegrees)))
    val X = Math.round(intensity * (1 + quotient)).toInt
    val Y = Math.round(intensity - intensity * saturation).toInt
    val Z = Math.round(3.0 * intensity - X - Y).toInt

    val (red, green, blue) = finalOrder(X, Y, Z)

    (red, green, blue)
  end hsiToRGB

  /** Creates an ARGB integer value from RGBA components. Validates the RGBA components before
    * conversion.
    *
    * @param red
    *   The red component, in the range [0, 255].
    * @param green
    *   The green component, in the range [0, 255].
    * @param blue
    *   The blue component, in the range [0, 255].
    * @param opacity
    *   The opacity component, in the range [0, 255]. Default is [[smile.infrastructure.Constants.MaximumOpacity]].
    * @return
    *   An integer representing the ARGB value.
    */
  def argbIntFrom(red: Int, green: Int, blue: Int, opacity: Int = MaximumOpacity): Int =
    validateRGBA(red, green, blue, opacity)
    (opacity << 24) | (red << 16) | (green << 8) | blue

  private def normalizedHSIHueInDegreesFrom(hueCandidateInDegrees: Double): Double =
    val modCircle = hueCandidateInDegrees % FullCircleInDegrees
    if modCircle < 0 then FullCircleInDegrees + modCircle
    else modCircle
  end normalizedHSIHueInDegreesFrom

  /** Determines if the specified RGB color is a shade of gray. A color is considered gray if its
    * red, green, and blue components are equal.
    *
    * @param red
    *   The red component of the color, in the range [0, 255].
    * @param green
    *   The green component of the color, in the range [0, 255].
    * @param blue
    *   The blue component of the color, in the range [0, 255].
    * @return
    *   `true` if the color is a shade of gray, otherwise `false`.
    */
  def isGray(red: Int, green: Int, blue: Int): Boolean = red == blue && green == blue

  /** Determines if the specified RGB color is black. A color is considered black if its red, green,
    * and blue components are all at their minimum value.
    *
    * @param red
    *   The red component of the color, expected to be [[smile.infrastructure.Constants.MinimumRed]] for black.
    * @param green
    *   The green component of the color, expected to be [[smile.infrastructure.Constants.MinimumGreen]] for black.
    * @param blue
    *   The blue component of the color, expected to be [[smile.infrastructure.Constants.MinimumBlue]] for black.
    * @return
    *   `true` if the color is black, otherwise `false`.
    */
  def isBlack(red: Int, green: Int, blue: Int): Boolean =
    red == MinimumRed && green == MinimumGreen && blue == MinimumBlue

  private def min(red: Int, green: Int, blue: Int): Int = red.min(green).min(blue)

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

/** Represents a color with RGB and opacity components. Optionally, a canonical name can be
  * associated.
  *
  * @param red
  *   The red component of the color, in the range [0, 255].
  * @param green
  *   The green component of the color, in the range [0, 255].
  * @param blue
  *   The blue component of the color, in the range [0, 255].
  * @param opacity
  *   The opacity of the color, in the range [0, 255]. Defaults to [[smile.infrastructure.Constants.MaximumOpacity]].
  * @param canonicalName
  *   An optional canonical name for the color.
  */
class Color(
    val red: Int,
    val green: Int,
    val blue: Int,
    val opacity: Int = MaximumOpacity,
    val canonicalName: Option[String] = None
):
  Color.validateRGBA(red, green, blue, opacity)

  def this(red: Int, green: Int, blue: Int, opacity: Int, name: String) = this(
    red,
    green,
    blue,
    opacity,
    Some(name)
  )

  def this(argb: Int, canonicalName: Option[String]) = this(
    (argb >> 16) & 0xff,
    (argb >> 8) & 0xff,
    argb & 0xff,
    (argb >> 24) & 0xff,
    canonicalName
  )

  def this(argb: Int) = this(argb, None)

  /** Checks if the color is fully opaque.
    *
    * @return
    *   `true` if the opacity is equal to [[smile.infrastructure.Constants.MaximumOpacity]], otherwise `false`.
    */
  def isOpaque: Boolean = opacity == MaximumOpacity

  /** Converts the color to HSI (Hue, Saturation, Intensity) format.
    *
    * @return
    *   A tuple containing the HSI components of the color.
    */
  def toHSI: (Double, Double, Double) = Color.rgbToHSI(red, green, blue)

  /** Converts the color to a `java.awt.Color` instance.
    *
    * @return
    *   A new `java.awt.Color` instance matching the RGB and opacity of this color.
    */
  def toAWTColor: LowLevelColor = new LowLevelColor(red, green, blue, opacity)

  /** Converts the color to an ARGB integer.
    *
    * @return
    *   An integer representing the ARGB value of the color.
    */
  def toARGBInt: Int = Color.argbIntFrom(red, green, blue, opacity)

  /** Lightens the color by a default factor.
    *
    * @return
    *   A new `Color` instance that is a lighter version of this color.
    * @see
    *   [[tintByFactor]]
    */
  def lighter: Color = tintByFactor(DefaultTintingFactor)

  /** Darkens the color by a default factor.
    *
    * @return
    *   A new `Color` instance that is a darker version of this color.
    * @see
    *   [[shadeByFactor]]
    */
  def darker: Color = shadeByFactor(DefaultShadingFactor)

  /** Lightens the color by a specified factor.
    *
    * @param tintingFactorFromZeroToOne
    *   The factor to lighten the color by, in the range [0, 1].
    * @return
    *   A new `Color` instance that is a lighter version of this color by the specified factor.
    */
  def tintByFactor(tintingFactorFromZeroToOne: Double): Color =
    val newRed   = (red + tintingFactorFromZeroToOne * (MaximumRed - red)).ceil.toInt
    val newGreen = (green + tintingFactorFromZeroToOne * (MaximumGreen - green)).ceil.toInt
    val newBlue  = (blue + tintingFactorFromZeroToOne * (MaximumBlue - blue)).ceil.toInt

    new Color(newRed, newGreen, newBlue, opacity)
  end tintByFactor

  /** Darkens the color by a specified factor.
    *
    * @param shadingFactorFromZeroToOne
    *   The factor to darken the color by, in the range [0, 1].
    * @return
    *   A new `Color` instance that is a darker version of this color by the specified factor.
    */
  def shadeByFactor(shadingFactorFromZeroToOne: Double): Color =
    val invertedFactor: Double = 1.0 - shadingFactorFromZeroToOne
    val newRed                 = (invertedFactor * red).toInt
    val newGreen               = (invertedFactor * green).toInt
    val newBlue                = (invertedFactor * blue).toInt

    new Color(newRed, newGreen, newBlue, opacity)
  end shadeByFactor

  /** Lightens the color by a specified percentage.
    *
    * @param tintingFactorInPercents
    *   The percentage to lighten the color by, in the range [0, 100].
    * @return
    *   A new `Color` instance that is a lighter version of this color by the specified percentage.
    */
  def tintByPercentage(tintingFactorInPercents: Double): Color =
    require(
      tintingFactorInPercents >= 0.0 && tintingFactorInPercents <= 100.0,
      "Tinting factor must be between 0 and 100"
    )

    tintByFactor(tintingFactorInPercents / 100.0)
  end tintByPercentage

  /** Darkens the color by a specified percentage.
    *
    * @param shadingFactorInPercents
    *   The percentage to darken the color by, in the range [0, 100].
    * @return
    *   A new `Color` instance that is a darker version of this color by the specified percentage.
    */
  def shadeByPercentage(shadingFactorInPercents: Double): Color =
    require(
      shadingFactorInPercents >= 0.0 && shadingFactorInPercents <= 100.0,
      "Shading factor must be between 0 and 100"
    )

    shadeByFactor(shadingFactorInPercents / 100.0)
  end shadeByPercentage
