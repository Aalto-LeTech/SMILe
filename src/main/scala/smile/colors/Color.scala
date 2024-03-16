package smile.colors

import smile.infrastructure.Constants.*

import java.awt

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
    *   The opacity of the color, in the range [0, 255]. Default is
    *   [[smile.infrastructure.Constants.MaximumOpacity]].
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
        val redMinusGreen = red - green
        val redMinusBlue  = red - blue

        val root = Math.sqrt(redMinusGreen * redMinusGreen + redMinusBlue * (green - blue))

        val angleCandidate = Math
          .toDegrees(Math.acos((redMinusGreen + redMinusBlue) / (2.0 * root)))

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

    // Normalize the hue to [0, 360)
    val nHueInDeg = hueInDegrees % 360.0

    // Convert intensity to a scale of 0 to 1 for calculations
    val normalizedIntensity = intensity / 255.0

    // Calculate the RGB values in the range [0, 1]
    val C: Double = (1 - Math.abs(2 * normalizedIntensity - 1)) * saturation
    val X: Double = C * (1 - Math.abs((nHueInDeg / 60.0) % 2 - 1))
    val m: Double = normalizedIntensity - C / 2

    val (r1, g1, b1): (Double, Double, Double) =
      if nHueInDeg < 60 then (C, X, 0.0)
      else if nHueInDeg < 120 then (X, C, 0.0)
      else if nHueInDeg < 180 then (0.0, C, X)
      else if nHueInDeg < 240 then (0.0, X, C)
      else if nHueInDeg < 300 then (X, 0.0, C)
      else (C, 0.0, X)

    // Adjust the RGB values to the range [0, 255]
    val red   = Math.round((r1 + m) * 255).toInt
    val green = Math.round((g1 + m) * 255).toInt
    val blue  = Math.round((b1 + m) * 255).toInt

    (red, green, blue)

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
    *   The opacity component, in the range [0, 255]. Default is
    *   [[smile.infrastructure.Constants.MaximumOpacity]].
    * @return
    *   An integer representing the ARGB value.
    */
  def argbIntFrom(red: Int, green: Int, blue: Int, opacity: Int = MaximumOpacity): Int =
    validateRGBA(red, green, blue, opacity)
    (opacity << 24) | (red << 16) | (green << 8) | blue

  def rgbaIntFrom(red: Int, green: Int, blue: Int, opacity: Int = MaximumOpacity): Int =
    validateRGBA(red, green, blue, opacity)
    (red << 24) | (green << 16) | (blue << 8) | opacity

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
    *   The red component of the color, expected to be [[smile.infrastructure.Constants.MinimumRed]]
    *   for black.
    * @param green
    *   The green component of the color, expected to be
    *   [[smile.infrastructure.Constants.MinimumGreen]] for black.
    * @param blue
    *   The blue component of the color, expected to be
    *   [[smile.infrastructure.Constants.MinimumBlue]] for black.
    * @return
    *   `true` if the color is black, otherwise `false`.
    */
  def isBlack(red: Int, green: Int, blue: Int): Boolean =
    red == MinimumRed && green == MinimumGreen && blue == MinimumBlue

  private def min(red: Int, green: Int, blue: Int): Int = red.min(green).min(blue)

  private def validateRGBA(red: Int, green: Int, blue: Int, opacity: Int = 255): Unit =
    require(
      red >= MinimumRed && red <= MaximumRed,
      s"Red must be between $MinimumRed and $MaximumRed"
    )
    require(
      green >= MinimumGreen && green <= MaximumGreen,
      s"Green must be between $MinimumGreen and $MaximumGreen"
    )
    require(
      blue >= MinimumBlue && blue <= MaximumBlue,
      s"Blue must be between $MinimumBlue and $MaximumBlue"
    )
    require(
      opacity >= MinimumOpacity && opacity <= MaximumOpacity,
      s"Opacity must be between $MinimumOpacity and $MaximumOpacity"
    )

  private def validateHSI(
      hue: Double,
      saturation: Double,
      intensity: Double,
      opacity: Int = 255
  ): Unit =
    require(
      hue >= MinimumHSIHue && hue <= MaximumHSIHue,
      s"Hue must be between $MinimumHSIHue and $MaximumHSIHue"
    )
    require(
      saturation >= MinimumHSISaturation && saturation <= MaximumHSISaturation,
      s"Saturation must be between $MinimumHSISaturation and $MaximumHSISaturation"
    )
    require(
      intensity >= MinimumHSIIntensity && intensity <= MaximumHSIIntensity,
      s"Intensity must be between $MinimumHSIIntensity and $MaximumHSIIntensity"
    )
    require(
      opacity >= MinimumOpacity && opacity <= MaximumOpacity,
      s"Opacity must be between $MinimumOpacity and $MaximumOpacity"
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
  *   The opacity of the color, in the range [0, 255]. Defaults to
  *   [[smile.infrastructure.Constants.MaximumOpacity]].
  */
case class Color(
    red: Int,
    green: Int,
    blue: Int,
    opacity: Int = MaximumOpacity
) extends Paint:
  Color.validateRGBA(red, green, blue, opacity)

  def this(argb: Int) = this(
    (argb >> 16) & 0xff,
    (argb >> 8) & 0xff,
    argb & 0xff,
    (argb >> 24) & 0xff
  )

  /** Checks if the color is fully opaque.
    *
    * @return
    *   `true` if the opacity is equal to [[smile.infrastructure.Constants.MaximumOpacity]],
    *   otherwise `false`.
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
  def toAWTColor: awt.Color = new awt.Color(red, green, blue, opacity)

  override lazy val toAWTPaint: awt.Paint = toAWTColor

  override lazy val averageColor: Color = this

  /** Converts the color to an ARGB integer.
    *
    * @return
    *   An integer representing the ARGB value of the color.
    */
  def toARGBInt: Int = Color.argbIntFrom(red, green, blue, opacity)

  /** Converts the color to an RGBA integer.
    *
    * @return
    *   An integer representing the RGBA value of the color.
    */
  def toRGBAInt: Int = Color.rgbaIntFrom(red, green, blue, opacity)

  private def toRGBInt: Int = (red << 16) | (green << 8) | blue

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

  override def toString: String =
    if opacity == MaximumOpacity then f"#$toRGBInt%06X"
    else f"#$toRGBAInt%08X"
