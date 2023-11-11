package smile.modeling

import smile.infrastructure.MathUtils

import java.awt.geom.AffineTransform as AWTAffineTransform

object AffineTransformation:
  private val Zero = 0.0

  private val One = 1.0

  /** Identity transformation. */
  lazy val Identity: AffineTransformation =
    AffineTransformation(One, Zero, Zero, Zero, One, Zero)

  /** @param horizontalSize
    *   in pixels
    * @return
    *   an affine transformation that flips the image horizontally
    */
  def forYAxisRelativeHorizontalFlipOf(horizontalSize: Double): AffineTransformation =
    AffineTransformation(-One, Zero, -horizontalSize, Zero, One, Zero)

  /** @param verticalSize
    *   in pixels
    * @return
    *   an affine transformation that flips the image vertically
    */
  def forXAxisRelativeVerticalFlipOf(verticalSize: Double): AffineTransformation =
    AffineTransformation(One, Zero, Zero, Zero, -One, -verticalSize)

  /** @param horizontalSize
    *   in pixels
    * @param verticalSize
    *   in pixels
    * @return
    *   an affine transformation that flips the image diagonally
    */
  def forOrigoRelativeDiagonalFlipOf(
      horizontalSize: Double,
      verticalSize: Double
  ): AffineTransformation =
    AffineTransformation(-One, Zero, -horizontalSize, Zero, -One, -verticalSize)

  def forPointCentredRotation(angle: Double, pX: Double, pY: Double): AffineTransformation =
    val cos = MathUtils.cos(angle)
    val sin = MathUtils.sin(angle)

    forPointCentredRotation(cos, sin, pX, pY)

  def forPointCentredRotation(
      cos: Double,
      sin: Double,
      pX: Double,
      pY: Double
  ): AffineTransformation =
    AffineTransformation(cos, sin, pX + sin * pY - cos * pX, -sin, cos, pY - sin * pX - cos * pY)

case class AffineTransformation(
    alpha: Double,
    gamma: Double,
    tauX: Double,
    delta: Double,
    beta: Double,
    tauY: Double
):
  def toAWTAffineTransform: AWTAffineTransform =
    val awtTransform = new AWTAffineTransform()

    awtTransform.setTransform(
      alpha,
      delta,
      gamma,
      beta,
      tauX,
      tauY
    )

    awtTransform
