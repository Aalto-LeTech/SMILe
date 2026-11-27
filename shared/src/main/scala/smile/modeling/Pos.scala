package smile.modeling

import smile.modeling.NumberSupport.*

import scala.annotation.targetName
import scala.math.hypot

/** Each instance of this class represents a location on a two-dimensional plane. A `Pos` object is
  * a pair of two coordinates, `x` and `y`. `Pos` objects are immutable.
  */
final case class Pos(x: Double, y: Double) derives CanEqual:

  /** Adds the two given values to the `x` and `y` of this `Pos`, respectively. Returns the
    * resulting `Pos`. The expressions `myPos.add(dx, dy)` and `myPos.offset(dx, dy)` are
    * equivalent.
    */
  def add(dx: Double, dy: Double) = Pos(this.x + dx, this.y + dy)

  /** Returns the `Pos` that is `dx` and `dy` removed from this `Pos`. The expressions
    * `myPos.offset(dx, dy)` and `myPos.add(dx, dy)` are equivalent.
    */
  def offset(dx: Double, dy: Double): Pos = this.add(dx, dy)

  /** Adds the `x` and `y` of the given `Pos` to the `x` and `y` of this `Pos`, respectively.
    * Returns the resulting `Pos`. The expressions `pos1.add(pos2)`, `pos1.offset(pos2)`, and
    * `pos1 + pos2` are equivalent.
    */
  def add(pos: Pos): Pos = this.add(pos.x, pos.y)

  /** Returns the `Pos` that is removed from this `Pos` by the amount indicated by the given `Pos`.
    * The expressions `pos1.offset(pos2)`, `pos1.add(pos2)`, and `pos1 + pos2` are equivalent.
    */
  def offset(another: Pos): Pos = this.add(another)

  /** Adds the `x` and `y` of the given `Pos` to the `x` and `y` of this `Pos`, respectively.
    * Returns the resulting `Pos`. The expressions `pos1 + pos2`, `pos1.add(pos2)`, and
    * `pos1.offset(pos2)` are equivalent.
    */
  @targetName("plus")
  infix def +(another: Pos): Pos = this.add(another)

  /** Subtracts the `x` and `y` of the given `Pos` from the `x` and `y` of this `Pos`, respectively.
    * Returns the resulting `Pos`. The expressions `pos1.subtract(pos2)` and `pos1 - pos2` are
    * equivalent.
    */
  def subtract(another: Pos): Pos = this + -another

  /** Subtracts the two given values from the `x` and `y` of this `Pos`, respectively. Returns the
    * resulting `Pos`.
    */
  def subtract(dx: Double, dy: Double) = Pos(this.x - dx, this.y - dy)

  /** Subtracts the `x` and `y` of the given `Pos` from the `x` and `y` of this `Pos`, respectively.
    * Returns the resulting `Pos`. The expressions `pos1 - pos2` and `pos1.subtract(pos2)` are
    * equivalent.
    */
  @targetName("minus")
  infix def -(another: Pos): Pos = this + -another

  /** Returns the `Pos` that is the opposite of this `Pos`. For instance, the opposite of (2,-3) is
    * (-2,3).
    */
  @targetName("negation")
  def unary_- = Pos(-this.x, -this.y)

  /** Returns a `Pos` whose `x` coordinate is less than this `Pos`’s by the given amount. */
  def subtractX(dx: Double): Pos = this.setX(this.x - dx)

  /** Returns a `Pos` whose `y` coordinate is less than this `Pos`’s by the given amount. */
  def subtractY(dy: Double): Pos = this.setY(this.y - dy)

  /** Returns a `Pos` whose `x` coordinate is greater than this `Pos`’s by the given amount. */
  def addX(dx: Double): Pos = this.setX(this.x + dx)

  /** Returns a `Pos` whose `y` coordinate is greater than this `Pos`’s by the given amount. */
  def addY(dy: Double): Pos = this.setY(this.y + dy)

  /** Returns a `Pos` whose two coordinates are obtained by multiplying this one’s `x` and `y` by
    * the given factor. For instance, (2,-3) multiplied by 10 is (20,-30). The expressions
    * `myPos.multiply(n)` and `myPos * n` are equivalent.
    */
  def multiply(factor: Double): Pos = Pos(this.x * factor, this.y * factor)

  /** Returns a `Pos` whose two coordinates are obtained by multiplying this one’s `x` and `y` by
    * the given factor. For instance, (2,-3) multiplied by 10 is (20,-30). The expressions
    * `myPos * n` and `myPos.multiply(n)` are equivalent.
    */
  @targetName("times")
  infix def *(factor: Double): Pos = this.multiply(factor)

  /** Returns a `Pos` whose two coordinates are obtained by dividing this one’s `x` and `y` by the
    * given number. For instance, (20,-30) divided by 10 is (2,-3). The expressions
    * `myPos.divide(n)` and `myPos / n` are equivalent.
    */
  def divide(divisor: Double): Pos = Pos(this.x / divisor, this.y / divisor)

  /** Returns a `Pos` whose two coordinates are obtained by dividing this one’s `x` and `y` by the
    * given number. For instance, (20,-30) divided by 10 is (2,-3). The expressions `myPos / n` and
    * `myPos.divide(n)` are equivalent.
    */
  @targetName("dividedBy")
  infix def /(divisor: Double): Pos = this.divide(divisor)

  /** Returns a `Pos` whose `x` equals the given value and whose `y` equals this `Pos`’s `y`. */
  def setX(newX: Double): Pos = this.copy(x = newX)

  /** Returns a `Pos` whose `x` equals this `Pos`’s `x` and whose `y` equals the given value. */
  def setY(newY: Double): Pos = this.copy(y = newY)

  /** Returns a `Pos` whose `x` and `y` are determined by applying the given function to the `x` and
    * `y` of this `Pos`.
    */
  def withXY(computeNew: (Double, Double) => (Double, Double)): Pos =
    Pos(computeNew(this.x, this.y))

  /** Returns a `Pos` whose `x` is determined by applying the given function to the `x` of this
    * `Pos` and whose `y` equals that of this `Pos`.
    */
  def withX(computeNew: Double => Double): Pos = this.setX(computeNew(this.x))

  /** Returns a `Pos` whose `x` equals that of this `Pos` and whose `y` is determined by applying
    * the given function to the `y` of this `Pos`.
    */
  def withY(computeNew: Double => Double): Pos = this.setY(computeNew(this.y))

  /** Returns the difference between this `Pos`’s `x` coordinate and the given one’s.
    * @return
    *   the difference, which is positive if the given `Pos`’s `x` is greater than this one’s and
    *   negative if the reverse is true
    */
  def xDiff(another: Pos): Double = another.x - this.x

  /** Returns the difference between this `Pos`’s `y` coordinate and the given one’s.
    * @return
    *   the difference, which is positive if the given `Pos`’s `y` is greater than this one’s and
    *   negative if the reverse is true
    */
  def yDiff(another: Pos): Double = another.y - this.y

  /** Returns a `Pos` that is as close to this `Pos` as possible but has a `y` coordinate less than
    * or equal to the given limit.
    * @see
    *   [[smile.modeling.Pos.noHigherThan]], [[smile.modeling.Pos.noFurtherRightThan]],
    *   [[smile.modeling.Pos.clampY]], [[smile.modeling.Pos.clamp]]
    */
  def noLowerThan(yMax: Double): Pos = Pos(this.x, this.y atMost yMax)

  /** Returns a `Pos` that is as close to this `Pos` as possible but has a `y` coordinate equal to
    * or greater than the given limit.
    * @see
    *   [[smile.modeling.Pos.noLowerThan]], [[smile.modeling.Pos.noFurtherLeftThan]],
    *   [[smile.modeling.Pos.clampY]], [[smile.modeling.Pos.clamp]]
    */
  def noHigherThan(yMin: Double): Pos = Pos(this.x, this.y atLeast yMin)

  /** Returns a `Pos` that is as close to this `Pos` as possible but has an `x` coordinate equal to
    * or greater than the given limit.
    * @see
    *   [[smile.modeling.Pos.noHigherThan]], [[smile.modeling.Pos.noFurtherRightThan]],
    *   [[smile.modeling.Pos.clampX]], [[smile.modeling.Pos.clamp]]
    */
  def noFurtherLeftThan(xMin: Double): Pos = Pos(this.x atLeast xMin, this.y)

  /** Returns a `Pos` that is as close to this `Pos` as possible but has an `x` coordinate less than
    * or equal to the given limit.
    * @see
    *   [[smile.modeling.Pos.noLowerThan]], [[smile.modeling.Pos.noFurtherLeftThan]],
    *   [[smile.modeling.Pos.clampX]], [[smile.modeling.Pos.clamp]]
    */
  def noFurtherRightThan(xMax: Double): Pos = Pos(this.x atMost xMax, this.y)

  /** Returns a `Pos` that is as close to this `Pos` as possible but whose `x` is between the given
    * lower and upper bounds (inclusive).
    * @param min
    *   the least possible value of `x` in the resulting `Pos`. Must not exceed `max`
    * @param max
    *   the greatest possible value of `x` in the resulting `Pos`. Must not be below `min`
    * @see
    *   [[smile.modeling.Pos.noFurtherLeftThan]], [[smile.modeling.Pos.noFurtherRightThan]],
    *   [[smile.modeling.Pos.clampY]], [[smile.modeling.Pos.clamp]]
    */
  def clampX(min: Double, max: Double): Pos = Pos(this.x atLeast min atMost max, this.y)

  /** Returns a `Pos` that is as close to this `Pos` as possible but whose `y` is between the given
    * lower and upper bounds (inclusive).
    * @param min
    *   the least possible value of `y` in the resulting `Pos`. Must not exceed `max`
    * @param max
    *   the greatest possible value of `y` in the resulting `Pos`. Must not be below `min`
    * @see
    *   [[smile.modeling.Pos.noLowerThan]], [[smile.modeling.Pos.noHigherThan]],
    *   [[smile.modeling.Pos.clampX]], [[smile.modeling.Pos.clamp]]
    */
  def clampY(min: Double, max: Double): Pos = Pos(this.x, this.y atLeast min atMost max)

  /** Returns a `Pos` that is as close to this `Pos` as possible but whose `x` and `y` are between
    * the given lower and upper bounds (inclusive).
    * @param xMin
    *   the least possible value of `x` in the resulting `Pos`. Must not exceed `xMax`
    * @param xMax
    *   the greatest possible value of `x` in the resulting `Pos`. Must not be below `xMin`
    * @param yMin
    *   the least possible value of `y` in the resulting `Pos`. Must not exceed `yMax`
    * @param yMax
    *   the greatest possible value of `y` in the resulting `Pos`. Must not be below `yMin`
    * @see
    *   [[smile.modeling.Pos.noLowerThan]], [[smile.modeling.Pos.noHigherThan]],
    *   [[smile.modeling.Pos.noFurtherLeftThan]], [[smile.modeling.Pos.noFurtherRightThan]],
    *   [[smile.modeling.Pos.clampX]], [[smile.modeling.Pos.clampY]]
    */
  def clamp(xMin: Double, xMax: Double, yMin: Double, yMax: Double): Pos =
    Pos(this.x atLeast xMin atMost xMax, this.y atLeast yMin atMost yMax)

  /** Returns the [[smile.modeling.Direction]] from this `Pos` towards the given other `Pos`. */
  def directionOf(destination: Pos): Direction =
    Direction.fromDeltas(this.xDiff(destination), this.yDiff(destination))

  /** Returns a `Pos` whose `x` and `y` capture the relative distance from this `Pos` towards the
    * given `Pos`. That is, subtracts the `x` and `y` of `this` from the `x` and `y` of the given
    * `Pos`, respectively. The expressions `pos1.vectorTo(pos2)` and `pos2 - pos1` are equivalent.
    */
  def vectorTo(destination: Pos): Pos = destination - this

  /** Returns the `Pos` that an object moving at the given velocity reaches in one unit of time.
    * That is, adds the `dx` and `dy` components of the given velocity to this `Pos` and returns the
    * result.
    */
  def nextPos(velocity: Velocity): Pos = this + velocity.toPos

  /** Returns the distance between this `Pos` and the given `Pos` “as the crow flies”. */
  def distance(another: Pos): Double = hypot(this.xDiff(another), this.yDiff(another))

  /** Returns a `String` description of the `Pos` that is identical to `toString` except it has two
    * decimals for each coordinate.
    */
  def roughly: String = f"$x%1.2f,$y%1.2f"

  /** Returns a `String` description of the `Pos`. It has the form `"(x,y)"`.
    * @see
    *   [[smile.modeling.Pos.roughly]]
    */
  def description: String = s"(${jvmText(x)},${jvmText(y)})"

  /** Returns a `String` description of the `Pos`. It has the form `"(x,y)"`. This is equivalent to
    * [[smile.modeling.Pos.description]].
    * @see
    *   [[smile.modeling.Pos.roughly]]
    */
  override def toString: String = this.description

  /** Returns the `x` and `y` coordinates of the `Pos` as a tuple. */
  def toPair: (Double, Double) = (this.x, this.y)

  /** Applies the given function to the two coordinates of the `Pos` and returns the result. */
  def turnInto[Result](f: (Double, Double) => Result): Result =
    f(this.x, this.y)

  inline private def copy(x: Double = this.x, y: Double = this.y) = Pos(x, y)

  /** Adds the given pair's two numbers to this position's `x` and `y`, respectively. */
  @targetName("plusTuple")
  def +(other: (Double, Double)): Pos = Pos(x + other._1, y + other._2)

  /** Subtracts the given pair's two numbers from this position's `x` and `y`, respectively. */
  @targetName("minusTuple")
  def -(other: (Double, Double)): Pos = Pos(x - other._1, y - other._2)

  /** Multiplies this position's `x` and `y` by the given position's `x` and `y`, respectively. */
  @targetName("timesPos")
  def *(other: Pos): Pos = Pos(x * other.x, y * other.y)

  /** Multiplies this position's `x` and `y` by the given pair's two numbers, respectively. */
  @targetName("timesTuple")
  def *(other: (Double, Double)): Pos = Pos(x * other._1, y * other._2)

  /** Divides this position's `x` and `y` by the given position's `x` and `y`, respectively. */
  @targetName("divPos")
  def /(other: Pos): Pos = Pos(x / other.x, y / other.y)

  /** Divides this position's `x` and `y` by the given pair's two numbers, respectively. */
  @targetName("divTuple")
  def /(other: (Double, Double)): Pos = Pos(x / other._1, y / other._2)

  /** Finds the center position between this position and another. */
  def centerBetween(destination: Pos): Pos =
    if this == destination then this else destination + (this - destination) / 2.0

  /** Moves this position by specified offsets. The same as [[smile.modeling.Pos.add]] with two
    * numbers.
    */
  def moveBy(xOffset: Double, yOffset: Double): Pos = Pos(x + xOffset, y + yOffset)

  /** Scales this position's horizontal and vertical distance from `relativityPoint` by the given
    * factors.
    */
  def scaleBy(
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
    */
  def scaleByRelativeToOrigin(horizontalFactor: Double, verticalFactor: Double): Pos =
    Pos(horizontalFactor * x, verticalFactor * y)

  /** Rotates the position around the origin (0,0) by a specified angle in degrees. Right angles
    * (multiples of 90 degrees) are computed exactly.
    *
    * @param angle
    *   The angle in degrees to rotate the position by. Positive angles rotate counterclockwise as
    *   seen on screen, where the y axis points down. `Pos(1, 0)` turns toward `Pos(0, -1)`, as
    *   [[smile.modeling.Direction]]'s angles do.
    */
  def rotateAroundOrigin(angle: Double): Pos =
    angle match
      case 0.0 | 360.0    => this
      case 90.0 | -270.0  => Pos(y, -x)
      case 180.0 | -180.0 => Pos(-x, -y)
      case 270.0 | -90.0  => Pos(-y, x)
      case _              =>
        val (sin, cos) = Angles.exactSinCos(angle)
        Pos(cos * x + sin * y, -sin * x + cos * y)

  /** Rotates the position around a specified center of rotation by a given angle in degrees. Right
    * angles (multiples of 90 degrees) are computed exactly.
    *
    * @param angle
    *   The angle in degrees to rotate the position by. Positive angles rotate counterclockwise as
    *   seen on screen, where the y axis points down. `Pos(1, 0)` turns toward `Pos(0, -1)`, as
    *   [[smile.modeling.Direction]]'s angles do.
    */
  def rotateAround(angle: Double, centerOfRotation: Pos): Pos =
    val center = centerOfRotation
    angle match
      case 0.0 | 360.0    => this
      case 90.0 | -270.0  => Pos(y - center.y + center.x, -x + center.x + center.y)
      case 180.0 | -180.0 => Pos(2 * center.x - x, 2 * center.y - y)
      case 270.0 | -90.0  => Pos(-y + center.x + center.y, x - center.x + center.y)
      case _              =>
        val (sin, cos) = Angles.exactSinCos(angle)
        val dx         = x - center.x
        val dy         = y - center.y
        Pos(cos * dx + sin * dy + center.x, -sin * dx + cos * dy + center.y)

end Pos

/** This companion object of [[smile.modeling.Pos class `Pos`]] provides alternative methods for
  * creating new `Pos` objects.
  *
  * On the JVM, `smile.modeling` adds `Pos(point)` for a `java.awt.Point` as an extension on this
  * object. `import smile.modeling.*` brings it into scope.
  */
object Pos:

  /** Constructs a [[smile.modeling.Pos]] instance from the given pair of x and y coordinates. */
  def apply(xyPair: (Double, Double)): Pos = new Pos(xyPair(0), xyPair(1))

  /** The origin (0,0) of the coordinate system as a `Pos` instance. */
  val Origin: Pos = Pos(0.0, 0.0)

end Pos
