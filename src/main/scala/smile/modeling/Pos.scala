package smile.modeling

import scala.annotation.targetName

/** Companion object for the `Pos` case class, providing a constant for the origin position.
  */
object Pos:
  /** The origin position (0.0, 0.0).
    */
  val Origin: Pos = Pos(0.0, 0.0)

/** Represents a position in a 2D coordinate system.
  *
  * @param x
  *   The x-coordinate of the position.
  * @param y
  *   The y-coordinate of the position.
  */
case class Pos(x: Double, y: Double):
  @targetName("plusPos")
  final inline def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

  @targetName("plusTuple")
  final inline def +(other: (Double, Double)): Pos = Pos(x + other._1, y + other._2)

  @targetName("minusPos")
  final inline def -(other: Pos): Pos = Pos(x - other.x, y - other.y)

  @targetName("minusTuple")
  final inline def -(other: (Double, Double)): Pos = Pos(x - other._1, y - other._2)

  @targetName("timesPos")
  final inline def *(other: Pos): Pos = Pos(x * other.x, y * other.y)

  @targetName("timesTuple")
  final inline def *(other: (Double, Double)): Pos = Pos(x * other._1, y * other._2)

  @targetName("timesDouble")
  final inline def *(other: Double): Pos = Pos(x * other, y * other)

  @targetName("divPos")
  final inline def /(other: Pos): Pos = Pos(x / other.x, y / other.y)

  @targetName("divTuple")
  final inline def /(other: (Double, Double)): Pos = Pos(x / other._1, y / other._2)

  @targetName("divDouble")
  final inline def /(other: Double): Pos = Pos(x / other, y / other)

  /** Finds the center position between this position and another.
    *
    * @param destination
    *   The other position to find the center between.
    * @return
    *   The center position between this and the destination position.
    */
  final inline def centerBetween(destination: Pos): Pos =
    if this == destination then this else destination + (this - destination) / 2.0

  /** Moves this position by specified offsets.
    *
    * @param xOffset
    *   The offset in the x-direction.
    * @param yOffset
    *   The offset in the y-direction.
    * @return
    *   A new position after the move.
    */
  inline def moveBy(xOffset: Double, yOffset: Double): Pos = Pos(x + xOffset, y + yOffset)

  /** Scales this position relative to a specific point.
    *
    * @param horizontalFactor
    *   The factor by which to scale the x-coordinate.
    * @param verticalFactor
    *   The factor by which to scale the y-coordinate.
    * @param relativityPoint
    *   The point relative to which scaling is performed.
    * @return
    *   A new position after scaling.
    */
  final inline def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Pos =
    val xDistance = x - relativityPoint.x
    val yDistance = y - relativityPoint.y

    Pos(
      relativityPoint.x + horizontalFactor * xDistance,
      relativityPoint.y + verticalFactor * yDistance
    )

  /** Scales the position relative to the origin (0,0) by specified horizontal and vertical factors.
    *
    * @param horizontalFactor
    *   The factor by which the x-coordinate is scaled.
    * @param verticalFactor
    *   The factor by which the y-coordinate is scaled.
    * @return
    *   A new `Pos` instance after scaling.
    */
  final inline def scaleByRelativeToOrigin(horizontalFactor: Double, verticalFactor: Double): Pos =
    Transformer.scale(this, horizontalFactor, verticalFactor)

  /** Rotates the position around the origin (0,0) by a specified angle in degrees.
    *
    * @param angle
    *   The angle in degrees to rotate the position by.
    * @return
    *   A new `Pos` instance after rotation.
    */
  def rotateByAroundOrigin(angle: Double): Pos = Transformer.rotate(this, angle)

  /** Rotates the position around a specified center of rotation by a given angle in degrees.
    * Supports direct rotation by 0, 90, 180, 270, 360, -90, -180, and -270 degrees. For angles not
    * directly supported, a general rotation method is used.
    *
    * @param angle
    *   The angle in degrees to rotate the position by.
    * @param centerOfRotation
    *   The center point around which the position is rotated.
    * @return
    *   A new `Pos` instance after rotation.
    */
  def rotateBy(angle: Double, centerOfRotation: Pos): Pos =
    angle match
      case 0.0    => this
      case 90.0   => Transformer.rotateBy90DegsCW(this, centerOfRotation)
      case 180.0  => Transformer.rotateBy180Degs(this, centerOfRotation)
      case 270.0  => Transformer.rotateBy90DegsCCW(this, centerOfRotation)
      case 360.0  => this
      case -90.0  => Transformer.rotateBy90DegsCCW(this, centerOfRotation)
      case -180.0 => Transformer.rotateBy180Degs(this, centerOfRotation)
      case -270.0 => Transformer.rotateBy90DegsCW(this, centerOfRotation)
      case _      => Transformer.rotate(this, angle, centerOfRotation)
