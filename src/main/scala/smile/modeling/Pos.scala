package smile.modeling

import scala.annotation.targetName

object Pos:
  val Origo: Pos = Pos(0.0, 0.0)

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

  final inline def centerBetween(destination: Pos): Pos =
    if this == destination then this else destination + (this - destination) / 2.0

  inline def moveBy(xOffset: Double, yOffset: Double): Pos = Pos(x + xOffset, y + yOffset)

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

  final inline def scaleByRelativeToOrigo(horizontalFactor: Double, verticalFactor: Double): Pos =
    Transformer.scale(this, horizontalFactor, verticalFactor)

  def rotateByAroundOrigo(angle: Double): Pos = Transformer.rotate(this, angle)

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
