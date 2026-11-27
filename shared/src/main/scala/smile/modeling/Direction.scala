package smile.modeling

import smile.modeling.NumberSupport.jvmText

import scala.math.*
import scala.annotation.targetName

/** Each instance of this class represents a direction on a two-dimensional plane. A direction
  * consists of two components: `dx` and `dy`, each of which is a number between -1.0 and +1.0
  * (inclusive). `dx` indicates the degree to which the x coordinate changes when moving in that
  * direction. `dy` is the same for the y coordinate. The x coordinate increases rightwards and y
  * downwards.
  *
  * For example, moving straight rightwards can be thought of as moving in a `Direction` with a `dx`
  * of +1.0 and a `dy` of 0.0. The upward direction has a `dx` of 0.0 and a `dy` of -1.0. Diagonals
  * have non-zero values for both `dx` and `dy`.
  *
  * The squares of the `dx` and `dy` of any `Direction` always sum to roughly 1.0, except for the
  * special value [[smile.modeling.Direction.NoDirection NoDirection]], which represents the lack of
  * a direction and has zero for both components. A `Direction` differs from a
  * [[smile.modeling.Velocity]], which combines a direction of movement and a speed.
  *
  * You don’t instantiate `Direction` directly. You create `Direction`s with the methods on this
  * class’s [[smile.modeling.Direction$ companion object]]. Among other things, you can:
  *
  *   - construct a direction from two differences between coordinates, as in
  *     `Direction.fromDeltas(-100, 50)`, or
  *   - create a direction that corresponds to a given angle, as in `Direction.fromDegrees(60)` and
  *     `Direction.fromRadians(-scala.math.Pi / 4)`.
  *
  * Some of the methods of a `Direction` object also create and return new `Direction`s, as does the
  * [[smile.modeling.Pos.directionOf directionOf]] method on `Pos` objects. Moreover, this class’s
  * [[smile.modeling.Direction$ companion object]] has a few predefined `Direction` constants:
  * [[smile.modeling.Direction.Up Up]], [[smile.modeling.Direction.Down Down]],
  * [[smile.modeling.Direction.Left Left]], [[smile.modeling.Direction.Right Right]], and
  * [[smile.modeling.Direction.NoDirection NoDirection]].
  *
  * `Direction` objects are immutable.
  */
final case class Direction private (dx: Double, dy: Double)
    derives CanEqual: // dx*dx+dy*dy ~= 1, or both zero

  /** Returns this direction as radians. Zero radians points rightwards, and the angle grows
    * counterclockwise from there, staying below 2π.
    */
  def toRadians: Double =
    val atan = atan2(-this.dy, this.dx)
    if atan < 0 then atan + 2 * Pi else atan

  /** Returns this direction as degrees. Zero degrees points rightwards, and the angle grows
    * counterclockwise from there, staying below 360.
    */
  def toDegrees: Double =
    // Adding zero turns the -0.0 of a rightward direction into 0.0.
    this.toRadians.toDegrees + 0.0

  /** Returns a direction that has the opposite `dx` as this one and the same `dy`. */
  def switchX: Direction = Direction(-dx, dy)

  /** Returns a direction that has the opposite `dy` as this one and the same `dx`. */
  def switchY: Direction = Direction(dx, -dy)

  /** Returns a direction that has the opposite `dx` and the opposite `dy` as this one. */
  def opposite: Direction = Direction(-dx, -dy)

  /** Returns `true` if this direction is a clearly rightward one (with a `dx` over zero) or at
    * least non-leftward (with a `dx` of exactly zero). Only returns `false` if `dx` is less than
    * zero.
    */
  def isRightward: Boolean = this.dx >= 0

  /** Returns `true` if this direction is a clearly leftward one (with a `dx` under zero) or at
    * least non-rightward (with a `dx` of exactly zero). Only returns `false` if `dx` is over zero.
    */
  def isLeftward: Boolean = this.dx <= 0

  /** Returns `true` if this direction is a clearly downward one (with a `dy` over zero) or at least
    * non-upward (with a `dy` of exactly zero). Only returns `false` if `dy` is less than zero.
    */
  def isDownward: Boolean = this.dy >= 0

  /** Returns `true` if this direction is a clearly upward one (with a `dy` under zero) or at least
    * non-downward (with a `dy` of exactly zero). Only returns `false` if `dy` is over zero.
    */
  def isUpward: Boolean = this.dy <= 0

  /** Determines whether this direction and the given one point towards the same quarter of the unit
    * circle. That is, determines whether the two directions’ `dx` have same sign as the other and
    * whether their `dy`s also have the same signs. Zero values count as both positive and negative
    * for this purpose.
    */
  def sharesQuadrant(another: Direction): Boolean =
    def sameHalf(dx1: Double, dx2: Double) = dx1.sign == dx2.sign || dx1.sign == 0 || dx2.sign == 0
    sameHalf(this.dx, another.dx) && sameHalf(this.dy, another.dy)

  /** Returns a direction that is removed from this direction by the given number of degrees
    * counterclockwise. For instance, `counterclockwise(90)` produces a direction at a right angle
    * to the original.
    */
  def counterclockwise(degrees: Double): Direction = Direction.fromDegrees(this.toDegrees + degrees)

  /** Returns a direction that is removed from this direction by the given number of degrees
    * clockwise. For instance, `clockwise(90)` produces a direction at a right angle to the
    * original.
    */
  def clockwise(degrees: Double): Direction = Direction.fromDegrees(this.toDegrees - degrees)

  /** Returns a direction that is removed from this direction counterclockwise by the number of
    * degrees the given direction represents. Calling `dir1 + dir2` is equivalent to calling
    * `dir1.counterclockwise(dir2.toDegrees)`.
    */
  @targetName("plus")
  def +(another: Direction): Direction = Direction.fromDegrees(this.toDegrees + another.toDegrees)

  /** Returns a `String` description of the `Direction`. The exact format varies for different
    * direction objects. The companion's constants print their names, such as `Direction.Up`, and
    * any other direction prints its components, even one equal to a constant.
    * @see
    *   [[smile.modeling.Direction.roughly]]
    */
  override def toString: String =
    if this eq Direction.Up then "Direction.Up"
    else if this eq Direction.Down then "Direction.Down"
    else if this eq Direction.Left then "Direction.Left"
    else if this eq Direction.Right then "Direction.Right"
    else if this eq Direction.NoDirection then "(no direction)"
    else s"Direction(dx=${jvmText(dx)},dy=${jvmText(dy)})"

  /** Returns a `String` description of the `Direction`, such as `"dx=1.00,dy=0.00"`. Uses two
    * decimals for each coordinate.
    */
  def roughly: String = f"dx=$dx%1.2f,dy=$dy%1.2f"

end Direction

/** This companion object of [[smile.modeling.Direction class `Direction`]] provides some constants
  * of type `Direction` and methods for creating new `Direction` instances.
  */
object Direction:

  /** the “straight upwards” direction with a `dx` of zero, and a `dy` of -1 */
  val Up: Direction = Direction(0, -1)

  /** the “straight downwards” direction with a `dx` of zero, and a `dy` of +1 */
  val Down: Direction = Direction(0, 1)

  /** the “straight leftwards” direction with a `dx` of -1, and a `dy` of zero */
  val Left: Direction = Direction(-1, 0)

  /** the “straight rightwards” direction with a `dx` of +1, and a `dy` of zero */
  val Right: Direction = Direction(1, 0)

  /** a “non-direction” whose `dx` and `dy` are both zero */
  val NoDirection: Direction = Direction(0, 0)

  /** Takes in an angle as radians and returns the corresponding [[smile.modeling.Direction]]. Zero
    * radians points rightwards, positive angles are counterclockwise from there, and negative ones
    * clockwise.
    */
  def fromRadians(angle: Double): Direction = Direction(cos(angle), -sin(angle))

  /** Takes in an angle as degrees and returns the corresponding [[smile.modeling.Direction]]. Zero
    * degrees points rightwards, positive angles are counterclockwise from there, and negative ones
    * clockwise.
    */
  def fromDegrees(angle: Double): Direction =
    val (sin, cos) = Angles.exactSinCos(angle)
    Direction(cos, -sin)

  /** Takes in a difference between x coordinates and a difference between y coordinates and
    * constructs a [[smile.modeling.Direction]] from them.
    *
    * @param dx
    *   a difference between two x coordinates. It need *not* lie between -1.0 and +1.0 like the
    *   `dx` and `dy` of the resulting [[smile.modeling.Direction]] object.
    * @param dy
    *   a difference between two y coordinates. It need *not* lie between -1.0 and +1.0 like the
    *   `dx` and `dy` of the resulting [[smile.modeling.Direction]] object.
    * @return
    *   the [[smile.modeling.Direction]] that corresponds to the given deltas. If both are exactly
    *   equal to zero, returns [[smile.modeling.Direction.NoDirection]].
    */
  def fromDeltas(dx: Double, dy: Double): Direction =
    if dx == 0.0 && dy == 0.0 then NoDirection else fromRadians(atan2(-dy, dx))

  /** Returns a randomly chosen direction, a whole number of degrees picked evenly from 0 to 359. */
  def random(): Direction =
    Direction.fromDegrees(scala.util.Random.nextInt(360))

end Direction
