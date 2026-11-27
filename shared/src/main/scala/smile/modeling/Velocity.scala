package smile.modeling

import smile.modeling.NumberSupport.*
import scala.math.*
import scala.annotation.targetName

/** Each instance of this class represents movement in the context of a two-dimensional plane. A
  * velocity is a combination of a [[smile.modeling.Direction]] of movement with a speed (a
  * non-negative `Double`).
  *
  * Another way to think about a `Velocity` is to divide it in two components: `dx` and `dy`. `dx`
  * indicates the amount (and direction) of movement along the x-axis. `dy` is the same for the
  * y-axis. The x coordinate increases rightwards and y downwards. For instance, a velocity with a
  * `dx` of 10 and a `dy` of zero indicates rightward movement, and a velocity with a `dx` of -100
  * and a `dy` of -100 indicates faster movement leftwards and upwards.
  *
  * There are many ways to create a `Velocity` using the methods on the
  * [[smile.modeling.Velocity$ companion object]]. Among other things, you can:
  *
  *   - construct a velocity from a direction and a (positive or zero) speed, as in
  *     `Velocity(Direction.Left, 50)`,
  *   - construct a velocity from `dx` and `dy`, as in `Velocity(-100, 50)`, or
  *   - determine the velocity needed to go from one [[smile.modeling.Pos]] to another in a single
  *     unit of time, as in `Velocity.between(pos1, pos2)`.
  *
  * Many of the methods on `Velocity` objects also create and return new velocities.
  *
  * `Velocity` objects are immutable.
  *
  * @param speed
  *   the speed of movement, which *cannot be negative*
  */
final case class Velocity(direction: Direction, speed: Double) derives CanEqual:
  require(speed >= 0, s"The speed component of a Velocity cannot be negative (was: $speed)")

  /** the amount of change in the x coordinate when moving at this velocity for one unit of time */
  lazy val dx: Double = this.direction.dx * speed

  /** the amount of change in the y coordinate when moving at this velocity for one unit of time */
  lazy val dy: Double = this.direction.dy * speed

  /** Sums this velocity with the given one. The sum’s `dx` equals the sum of the two `dx`s. The
    * same goes for `dy`.
    */
  @targetName("plus")
  def +(another: Velocity): Velocity = Velocity(this.dx + another.dx, this.dy + another.dy)

  /** Subtracts the given velocity from this one. The result’s `dx` equals the difference between
    * the two `dx`s. The same goes for `dy`.
    */
  @targetName("minus")
  def -(another: Velocity): Velocity = this + -another

  /** Returns a velocity that is equal to this one in speed but has the opposite direction. The
    * expressions `-myVelocity` and `myVelocity.opposite` are equivalent.
    */
  @targetName("negation")
  def unary_- : Velocity = this.copy(direction = this.direction.opposite)

  /** Returns a velocity that is equal to this one in speed but has the opposite direction. The
    * expressions `myVelocity.opposite` and `-myVelocity` are equivalent.
    */
  def opposite: Velocity = this.copy(direction = this.direction.opposite)

  /** Returns a velocity whose direction is the given number of degrees counterclockwise from this
    * one’s. The speed is the same.
    */
  def counterclockwise(degrees: Double): Velocity =
    this.changeDirection(this.direction.counterclockwise(degrees))

  /** Returns a velocity whose direction is the given number of degrees clockwise from this one’s.
    * The speed is the same.
    */
  def clockwise(degrees: Double): Velocity = this.changeDirection(this.direction.clockwise(degrees))

  /** Returns a velocity whose `dx` is the opposite of this one’s. The `dy` is the same, as is the
    * speed.
    */
  def switchX: Velocity = this.copy(direction = this.direction.switchX)

  /** Returns a velocity whose `dy` is the opposite of this one’s. The `dx` is the same, as is the
    * speed.
    */
  def switchY: Velocity = this.copy(direction = this.direction.switchY)

  /** Returns a velocity that has the given direction and the same speed as this one. */
  def changeDirection(newDirection: Direction): Velocity = this.copy(direction = newDirection)

  /** Returns a velocity that has the given speed and the same direction as this one. */
  def changeSpeed(newSpeed: Double): Velocity = this.copy(speed = newSpeed)

  /** Returns a velocity whose `speed` equals this velocity’s speed multiplied by the given number,
    * which must be positive or zero. The direction is the same.
    */
  @targetName("times")
  def *(multiplier: Double): Velocity = this.copy(speed = this.speed * multiplier)

  /** Returns a velocity whose `speed` equals this velocity’s speed divided by the given number,
    * which must be positive. The direction is the same.
    */
  @targetName("dividedBy")
  def /(divisor: Double): Velocity = this * (1 / divisor)

  /** Returns a velocity whose `speed` equals this velocity’s speed plus the given number. The
    * direction is the same.
    */
  def faster(addition: Double): Velocity =
    Velocity(this.direction, (this.speed + addition) atLeast 0)

  /** Returns a velocity whose `speed` equals this velocity’s speed minus the given number. The
    * direction is the same.
    */
  def slower(reduction: Double): Velocity =
    Velocity(this.direction, (this.speed - reduction) atLeast 0)

  /** Returns a velocity whose direction equals this one’s but whose `speed` is zero. */
  def stopped: Velocity = this.changeSpeed(0)

  /** Returns a velocity whose `speed` is as close to this one’s as possible without exceeding the
    * given number. The direction is the same.
    */
  infix def noFasterThan(maxSpeed: Double): Velocity =
    this.copy(speed = this.speed atMost maxSpeed atLeast 0)

  /** Returns a velocity whose `speed` is as close to this one’s as possible without being lower
    * than the given number. The direction is the same.
    */
  infix def noSlowerThan(minSpeed: Double): Velocity =
    this.copy(speed = this.speed atLeast minSpeed)

  /** Returns the `Pos` that an object moving at this velocity reaches in one unit of time. That is,
    * adds the `dx` and `dy` components of this velocity to the given `Pos` and returns the result.
    */
  def nextFrom(position: Pos): Pos = position + this.toPos

  /** Returns a `String` description of the `Velocity`. E.g.,
    * `"111.80 with direction dx=0.89,dy=-0.45"`. Uses two decimals for each number.
    */
  def roughly: String = f"$speed%1.2f with direction ${direction.roughly}"

  /** Returns a [[smile.modeling.Pos]] whose `x` and `y` components equal this velocity’s `dx` and
    * `dy`, respectively.
    */
  def toPos: Pos = Pos(this.dx, this.dy)

  /** Determines whether the speed of this `Velocity` is precisely zero. */
  def isStill: Boolean = this.speed == 0

  /** Returns a `String` description of the `Velocity`. The format varies depending on the
    * velocity’s direction.
    * @see
    *   [[smile.modeling.Velocity.roughly]]
    */
  override def toString: String =
    if Velocity.SpecialDirections.contains(this.direction) then
      s"Velocity($direction,${jvmText(speed)})"
    else s"Velocity(dx=${jvmText(dx)},dy=${jvmText(dy)})"

  inline private def copy(direction: Direction = this.direction, speed: Double = this.speed) =
    Velocity(direction, speed)

end Velocity

/** This companion object of [[smile.modeling.Velocity class `Velocity`]] exists primarily to
  * provide convenience methods for creating new instances of `Velocity`.
  */
object Velocity:

  /** a zero-speed [[smile.modeling.Velocity]] with a direction of
    * [[smile.modeling.Direction.NoDirection `NoDirection`]]
    */
  val Still = Velocity(Direction.NoDirection, 0)

  /** Creates a `Velocity` with the given `dx` and `dy`. */
  def apply(dx: Double, dy: Double): Velocity =
    new Velocity(Direction.fromDeltas(dx, dy), hypot(dx, dy))

  /** Creates a `Velocity` whose `dx` and `dy` equal the `x` and `y` of the given
    * [[smile.modeling.Pos]], respectively.
    */
  def apply(vector: Pos): Velocity = this(vector.x, vector.y)

  /** Returns the [[smile.modeling.Velocity]] from one [[smile.modeling.Pos]] to another. Its
    * direction points from `from` towards `to`, and its speed equals the distance between them.
    */
  def between(from: Pos, to: Pos): Velocity = Velocity(to - from)

  /** Returns the sum of all the given velocities. If the given collection is empty, returns
    * [[smile.modeling.Velocity.Still]].
    */
  def sum(velocities: Seq[Velocity]): Velocity =
    velocities.reduceLeftOption(_ + _) getOrElse Velocity.Still

  /** Returns the average of all the given velocities. If the given collection is empty, returns
    * [[smile.modeling.Velocity.Still]].
    */
  def average(velocities: Seq[Velocity]): Velocity =
    if velocities.nonEmpty then sum(velocities) / velocities.size else Velocity.Still

  private val SpecialDirections =
    import Direction.*
    Set(Up, Down, Left, Right, NoDirection)

end Velocity
