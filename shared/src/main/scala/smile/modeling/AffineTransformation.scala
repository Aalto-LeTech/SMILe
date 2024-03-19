package smile.modeling

import smile.infrastructure.MathUtils

/** Provides factory methods for creating common affine transformations, including flips and
  * rotations, relative to various axes and points.
  */
object AffineTransformation:
  private val Zero = 0.0

  private val One = 1.0

  /** Represents the identity transformation, which makes no changes to the coordinates.
    */
  lazy val Identity: AffineTransformation =
    AffineTransformation(One, Zero, Zero, Zero, One, Zero)

  /** Creates an affine transformation for a horizontal flip about the Y-axis.
    *
    * @param horizontalSize
    *   The width of the image in pixels.
    * @return
    *   An affine transformation that flips the image horizontally.
    */

  def forYAxisRelativeHorizontalFlipOf(horizontalSize: Double): AffineTransformation =
    AffineTransformation(-One, Zero, -horizontalSize, Zero, One, Zero)

  /** Creates an affine transformation for a vertical flip about the X-axis.
    *
    * @param verticalSize
    *   The height of the image in pixels.
    * @return
    *   An affine transformation that flips the image vertically.
    */

  def forXAxisRelativeVerticalFlipOf(verticalSize: Double): AffineTransformation =
    AffineTransformation(One, Zero, Zero, Zero, -One, -verticalSize)

  /** Creates an affine transformation for a diagonal flip about the origin, flipping both
    * horizontally and vertically.
    *
    * @param horizontalSize
    *   The width of the image in pixels.
    * @param verticalSize
    *   The height of the image in pixels.
    * @return
    *   An affine transformation that flips the image diagonally.
    */
  def forOriginRelativeDiagonalFlipOf(
      horizontalSize: Double,
      verticalSize: Double
  ): AffineTransformation =
    AffineTransformation(-One, Zero, -horizontalSize, Zero, -One, -verticalSize)

  /** Creates an affine transformation for rotation about a specified point.
    *
    * @param angle
    *   The angle of rotation in degrees.
    * @param pX
    *   The X-coordinate of the point around which to rotate.
    * @param pY
    *   The Y-coordinate of the point around which to rotate.
    * @return
    *   An affine transformation representing the rotation.
    */
  def forPointCentredRotation(angle: Double, pX: Double, pY: Double): AffineTransformation =
    val cos = MathUtils.cos(angle)
    val sin = MathUtils.sin(angle)

    forPointCentredRotation(cos, sin, pX, pY)

  /** Creates an affine transformation for rotation about a specified point using precomputed cosine
    * and sine values.
    *
    * @param cos
    *   The cosine of the rotation angle.
    * @param sin
    *   The sine of the rotation angle.
    * @param pX
    *   The X-coordinate of the point around which to rotate.
    * @param pY
    *   The Y-coordinate of the point around which to rotate.
    * @return
    *   An affine transformation representing the rotation.
    */
  def forPointCentredRotation(
      cos: Double,
      sin: Double,
      pX: Double,
      pY: Double
  ): AffineTransformation =
    AffineTransformation(cos, sin, pX + sin * pY - cos * pX, -sin, cos, pY - sin * pX - cos * pY)

/** Represents an affine transformation defined by matrix coefficients for scaling, rotating, and
  * translating points in 2D space.
  *
  * @param alpha
  *   The X scaling component.
  * @param gamma
  *   The X to Y shearing component.
  * @param tauX
  *   The X translation component.
  * @param delta
  *   The Y to X shearing component.
  * @param beta
  *   The Y scaling component.
  * @param tauY
  *   The Y translation component.
  */
case class AffineTransformation(
    alpha: Double,
    gamma: Double,
    tauX: Double,
    delta: Double,
    beta: Double,
    tauY: Double
)
