package smile.modeling

import java.awt.geom.AffineTransform

object TransformationMatrix:
  val identity = new TransformationMatrix()
class TransformationMatrix(private val matrix: Array[Array[Double]]):
  def this() = this(Array(Array(1.0, 0.0, 0.0), Array(0.0, 1.0, 0.0), Array(0.0, 0.0, 1.0)))

  def translate(dx: Double, dy: Double): TransformationMatrix =
    val translationMatrix = Array(
      Array(1.0, 0.0, dx),
      Array(0.0, 1.0, dy),
      Array(0.0, 0.0, 1.0)
    )
    multiply(new TransformationMatrix(translationMatrix))

  def rotate(angle: Double): TransformationMatrix =
    val rad = Math.toRadians(angle)
    val cos = Math.cos(rad)
    val sin = Math.sin(rad)
    val rotationMatrix = Array(
      Array(cos, -sin, 0.0),
      Array(sin, cos, 0.0),
      Array(0.0, 0.0, 1.0)
    )
    multiply(new TransformationMatrix(rotationMatrix))

  def rotate(angle: Double, pivot: Pos): TransformationMatrix =
    translate(-pivot.x, -pivot.y)
      .rotate(angle)
      .translate(pivot.x, pivot.y)

  def scale(sx: Double, sy: Double): TransformationMatrix =
    val scaleMatrix = Array(
      Array(sx, 0.0, 0.0),
      Array(0.0, sy, 0.0),
      Array(0.0, 0.0, 1.0)
    )
    multiply(new TransformationMatrix(scaleMatrix))

  def scale(sx: Double, sy: Double, center: Pos): TransformationMatrix =
    translate(-center.x, -center.y)
      .scale(sx, sy)
      .translate(center.x, center.y)

  def multiply(other: TransformationMatrix): TransformationMatrix =
    val result = Array.ofDim[Double](3, 3)
    for
      i <- 0 until 3
      j <- 0 until 3
    do result(i)(j) = (0 until 3).map(k => this.matrix(i)(k) * other.matrix(k)(j)).sum
    new TransformationMatrix(result)

  def applyToPoint(point: Pos): Pos =
    val x = point.x * matrix(0)(0) + point.y * matrix(0)(1) + matrix(0)(2)
    val y = point.x * matrix(1)(0) + point.y * matrix(1)(1) + matrix(1)(2)
    Pos(x, y)

  def applyToWidth(width: Double): Double =
    width * matrix(0)(0)

  def applyToHeight(height: Double): Double =
    height * matrix(1)(1)

  def toAffineTransform: AffineTransform =
    new AffineTransform(
      matrix(0)(0),
      matrix(1)(0),
      matrix(0)(1),
      matrix(1)(1),
      matrix(0)(2),
      matrix(1)(2)
    )
end TransformationMatrix
