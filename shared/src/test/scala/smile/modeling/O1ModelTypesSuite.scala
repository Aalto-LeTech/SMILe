package smile.modeling

import smile.modeling.NumberSupport.*

import scala.collection.mutable.ArrayBuffer

/** `Pos`, `Direction` and `Velocity` are shared with the course library, so they must behave and
  * print exactly as its own did. The probes repeat those of the course library's behaviour-baseline
  * harness, and [[smile.modeling.O1GoldenValues]] holds what the course library printed for them.
  */
class O1ModelTypesSuite extends munit.FunSuite:

  /** A probe result as the harness's `String.valueOf` wrote it on the JVM. The model types print
    * themselves. The doubles, tuples and lists around them are laid out as on the JVM here, so that
    * the same text is expected on Scala.js, where JavaScript writes `5.0` as `5`.
    */
  private def shown(value: Any): String = value match
    case number: Double                                            => jvmText(number)
    case tuple: Product if tuple.productPrefix.startsWith("Tuple") =>
      tuple.productIterator.map(shown).mkString("(", ",", ")")
    case seq: Seq[?] => seq.map(shown).mkString("List(", ", ", ")")
    case other       => String.valueOf(other)

  private def probes(): Seq[(String, String)] =
    val results                              = ArrayBuffer[(String, String)]()
    def v(name: String)(value: => Any): Unit = results += name -> shown(value)

    def angleId(a: Double) =
      val s = if a == a.floor then a.toInt.toString else a.toString
      s.replace("-", "m").replace(".", "_")

    val a = Pos(3, 4)
    val b = Pos(-1.5, 2.25)
    v("pos_toString")(a)
    v("pos_toString_fraction")(b)
    v("pos_roughly")(Pos(1.23456, -7.891).roughly)
    v("pos_description")(b.description)
    v("pos_add")(a + b)
    v("pos_add_xy")(a.add(1, 2))
    v("pos_offset")(a.offset(b))
    v("pos_subtract")(a - b)
    v("pos_subtract_xy")(a.subtract(1, 1))
    v("pos_unaryMinus")(-a)
    v("pos_multiply")(a * 2.5)
    v("pos_divide")(a / 3)
    v("pos_addX_addY")(a.addX(1).addY(-1))
    v("pos_subtractX_Y")(a.subtractX(1).subtractY(1))
    v("pos_setX_setY")(a.setX(10).setY(20))
    v("pos_withX")(a.withX(_ * 10))
    v("pos_withY")(a.withY(_ - 10))
    v("pos_withXY")(a.withXY((x, y) => (y, x)))
    v("pos_xDiff_yDiff")((a.xDiff(b), a.yDiff(b)))
    v("pos_distance")(a.distance(Pos.Origin))
    v("pos_distance_b")(a.distance(b))
    v("pos_vectorTo")(a.vectorTo(b))
    v("pos_directionOf")(a.directionOf(b))
    v("pos_directionOf_self")(a.directionOf(a))
    v("pos_nextPos")(a.nextPos(Velocity(1, 2)))
    v("pos_clamp")(Pos(50, -50).clamp(0, 10, 0, 10))
    v("pos_clampX")(Pos(50, -50).clampX(0, 10))
    v("pos_clampY")(Pos(50, -50).clampY(0, 10))
    v("pos_noLowerThan")(Pos(5, 50).noLowerThan(10))
    v("pos_noHigherThan")(Pos(5, 5).noHigherThan(10))
    v("pos_noFurtherLeftThan")(Pos(-5, 5).noFurtherLeftThan(0))
    v("pos_noFurtherRightThan")(Pos(15, 5).noFurtherRightThan(10))
    v("pos_toPair")(a.toPair)
    v("pos_turnInto")(a.turnInto(_ * _))
    v("pos_fromPair")(Pos((7.0, 8.0)))
    v("pos_equality")(Pos(1, 2) == Pos(1.0, 2.0))
    v("pos_hashCode_eq")(Pos(1, 2).hashCode == Pos(1.0, 2.0).hashCode)
    v("pos_origin")(Pos.Origin)
    v("pos_negZero")(Pos(-0.0, 0.0))

    for deg <- Seq(0.0, 30.0, 45.0, 90.0, 135.0, 180.0, 225.0, 270.0, 359.0, 360.0, -45.0, 720.0)
    do
      val d = Direction.fromDegrees(deg)
      v(s"dir_fromDegrees_${angleId(deg)}")(d)
      v(s"dir_fromDegrees_${angleId(deg)}.toDegrees")(d.toDegrees)
      v(s"dir_fromDegrees_${angleId(deg)}.roughly")(d.roughly)
    v("dir_fromRadians_pi")(Direction.fromRadians(math.Pi))
    v("dir_fromDeltas")(Direction.fromDeltas(3, -4))
    v("dir_fromDeltas_zero")(Direction.fromDeltas(0, 0))
    val presets =
      Seq(Direction.Up, Direction.Down, Direction.Left, Direction.Right, Direction.NoDirection)
    v("dir_presets")(presets)
    v("dir_preset_degrees")(presets.map(_.toDegrees))
    v("dir_preset_radians")(Seq(Direction.Up, Direction.Left).map(_.toRadians))
    val d = Direction.fromDegrees(30)
    v("dir_dx_dy")((d.dx, d.dy))
    v("dir_switchX")(d.switchX)
    v("dir_switchY")(d.switchY)
    v("dir_opposite")(d.opposite)
    v("dir_cw")(d.clockwise(45))
    v("dir_ccw")(d.counterclockwise(45))
    v("dir_plus")(d + Direction.fromDegrees(60))
    v("dir_flags")((d.isRightward, d.isLeftward, d.isUpward, d.isDownward))
    val none = Direction.NoDirection
    v("dir_flags_none")((none.isRightward, none.isLeftward, none.isUpward, none.isDownward))
    v("dir_sharesQuadrant")(
      (
        d.sharesQuadrant(Direction.fromDegrees(60)),
        d.sharesQuadrant(Direction.fromDegrees(200)),
        d.sharesQuadrant(Direction.Up)
      )
    )
    v("dir_equality")(Direction.fromDegrees(90) == Direction.Up)
    v("dir_equality_deltas")(Direction.fromDeltas(0, -5) == Direction.Up)

    val vel = Velocity(Direction.fromDegrees(30), 10)
    v("vel_toString")(vel)
    v("vel_toString_special")(Velocity(Direction.Up, 5))
    v("vel_toString_still")(Velocity.Still)
    v("vel_dxdy")(Velocity(3, -4))
    v("vel_fromPos")(Velocity(Pos(6, 8)))
    v("vel_between")(Velocity.between(Pos(1, 1), Pos(4, 5)))
    v("vel_fields")((vel.dx, vel.dy, vel.speed, vel.direction))
    v("vel_plus")(vel + Velocity(1, 1))
    v("vel_minus")(vel - Velocity(1, 1))
    v("vel_unaryMinus")(-vel)
    v("vel_opposite")(vel.opposite)
    v("vel_cw")(vel.clockwise(90))
    v("vel_ccw")(vel.counterclockwise(90))
    v("vel_cw_m30")(vel.clockwise(-30))
    v("vel_switchX")(vel.switchX)
    v("vel_switchY")(vel.switchY)
    v("vel_changeDirection")(vel.changeDirection(Direction.Left))
    v("vel_changeSpeed")(vel.changeSpeed(3))
    v("vel_times")(vel * 2)
    v("vel_div")(vel / 4)
    v("vel_faster")(vel.faster(5))
    v("vel_slower")(vel.slower(15))
    v("vel_stopped")(vel.stopped)
    v("vel_noFasterThan")(vel.noFasterThan(4))
    v("vel_noSlowerThan")(vel.noSlowerThan(40))
    v("vel_nextFrom")(vel.nextFrom(Pos(10, 10)))
    v("vel_roughly")(vel.roughly)
    v("vel_toPos")(vel.toPos)
    v("vel_isStill")((vel.isStill, Velocity.Still.isStill))
    v("vel_sum")(Velocity.sum(Seq(Velocity(1, 0), Velocity(0, 1), Velocity(-2, 0))))
    v("vel_sum_empty")(Velocity.sum(Seq()))
    v("vel_average")(Velocity.average(Seq(Velocity(2, 0), Velocity(0, 2))))
    v("vel_average_empty")(Velocity.average(Seq()))
    v("vel_equality")(Velocity(3, 4) == Velocity(Direction.fromDeltas(3, 4), 5))
    results.toSeq

  /** A number may be a few units in the last place off its golden value. The probes go through
    * `atan2`, `cos`, `sin` and `hypot`, whose last digit Java and ECMAScript leave to the platform.
    * The JVM's intrinsics on x64 end the `vel_ccw` probe's `dy` in -8.660254037844387, where ARM,
    * which printed the golden values, ends it in 389. For the direction of 30 degrees the JVM,
    * Firefox 150 and Chromium 148 give 29.999999999999993, and Chromium 153 and WebKit 26.4 give
    * 29.999999999999996. WebKit 26.4 also ends `Direction.fromDeltas(3, -4)` in 0.6 and
    * `Velocity(3, -4)` in 3.0, where the others end them in 0.6000000000000001 and
    * 3.0000000000000004. The text around the numbers is the course library's on every platform.
    */
  private val number = """-?[0-9][0-9.]*(E-?[0-9]+)?""".r

  /** Whether two texts are the same but for numbers a few units in the last place apart. */
  private def sameButForLastDigits(expected: String, actual: String): Boolean =
    val expectedNumbers = number.findAllIn(expected).map(_.toDouble).toSeq
    val actualNumbers   = number.findAllIn(actual).map(_.toDouble).toSeq
    number.replaceAllIn(expected, "#") == number.replaceAllIn(actual, "#") &&
    expectedNumbers.length == actualNumbers.length &&
    expectedNumbers.zip(actualNumbers).forall { (e, a) =>
      math.abs(e - a) <= 4 * math.ulp(math.max(math.abs(e), math.abs(a)))
    }

  /** Whether SMILe's text for an entry relates to the course library's as the departure says. */
  private def departsAsListed(
      expected: String,
      departure: O1GoldenValues.Departure
  ): Boolean =
    import O1GoldenValues.Difference.*
    val (o1, smile) = (
      number.findAllIn(expected).map(_.toDouble).toSeq,
      number.findAllIn(departure.smile).map(_.toDouble).toSeq
    )
    departure.kind match
      case Changed => expected != departure.smile
      case Near    =>
        number.replaceAllIn(expected, "#") == number.replaceAllIn(departure.smile, "#") &&
        o1.length == smile.length &&
        o1.zip(smile)
          .forall((e, a) =>
            math.abs(e - a) <= math.max(1e-15, 4 * math.ulp(math.max(math.abs(e), math.abs(a))))
          )
      case Turns =>
        o1.length == 1 && smile.length == 1 && {
          val turns = (o1.head - smile.head) / 360
          math.abs(turns - math.rint(turns)) < 1e-12
        }

  test(
    "every probe prints what the course library's own Pos, Direction and Velocity printed"
  ) {
    val actual     = probes()
    val mismatches =
      for
        ((name, expected), (actualName, text)) <- O1GoldenValues.expected.zip(actual)
        smileText = O1GoldenValues.deliberateDifferences.get(name).map(_.smile).getOrElse(expected)
        if name != actualName || text != smileText
        if !sameButForLastDigits(smileText, text)
      yield s"$name: expected $smileText, got $actualName -> $text"

    assertEquals(actual.map(_._1), O1GoldenValues.expected.map(_._1))
    assert(mismatches.isEmpty, mismatches.mkString("\n"))
  }

  test(
    "the entries where SMILe departs from the course library stay within what their reason allows"
  ) {
    val names = O1GoldenValues.expected.map(_._1).toSet
    for (name, departure) <- O1GoldenValues.deliberateDifferences do
      assert(names(name), s"$name is not a golden entry")
      val expected = O1GoldenValues.expected.toMap.apply(name)
      assert(departure.smile != expected, s"$name no longer differs from the course library")
      assert(
        departsAsListed(expected, departure),
        s"$name: ${departure.smile} vs the course library's $expected"
      )
  }

  test("a Pos prints as (x,y) with the JVM's number text, and roughly with two decimals") {
    assertEquals(Pos(3, 4).toString, "(3.0,4.0)")
    assertEquals(s"${Pos(-1.5, 2.25)}", "(-1.5,2.25)")
    assertEquals(Pos(1e7, 1e-4).toString, "(1.0E7,1.0E-4)")
    assertEquals(Pos(Double.NaN, Double.NegativeInfinity).toString, "(NaN,-Infinity)")
    assertEquals(Pos(1.23456, -7.891).roughly, "1.23,-7.89")
    assertEquals(Pos(-0.001, 2).roughly, "-0.00,2.00")
  }

  test("Pos equality is the case class's: -0.0 equals 0.0, NaN equals nothing") {
    assertEquals(Pos(-0.0, 0.0), Pos(0.0, -0.0))
    assertEquals(Pos(-0.0, 0.0).hashCode, Pos(0.0, -0.0).hashCode)
    assertEquals(Pos(-0.0, 0.0).toString, "(-0.0,0.0)")
    assertNotEquals(Pos(Double.NaN, 0), Pos(Double.NaN, 0))
    assert(Pos(1, 2) == Pos(1.0, 2.0))
  }

  test("atLeast and atMost are max and min, and clamp is the one and then the other") {
    assertEquals(3.0 atLeast 5.0, 5.0)
    assertEquals(7.0 atMost 5.0, 5.0)
    assert((Double.NaN atLeast 0.0).isNaN)
    assert((Double.NaN atMost 0.0).isNaN)
    assertEquals(1.0 / (-0.0 atLeast 0.0), Double.PositiveInfinity)
    assertEquals(1.0 / (0.0 atMost -0.0), Double.NegativeInfinity)
    assertEquals(15.0.clamp(0, 10), 10.0)
    assertEquals((-5.0).clamp(0, 10), 0.0)
    assertEquals(5.0.clamp(0, 10), 5.0)
    // With a low bound above the high one, the high one wins, as max-then-min gives.
    assertEquals(5.0.clamp(10, 0), 0.0)
    assertEquals(Pos(5, 5).clampX(10, 0), Pos(0, 5))
  }

  test("a number is laid out as the JVM's Double.toString writes it, on every platform") {
    val cases = Seq(
      3.0                     -> "3.0",
      -1.5                    -> "-1.5",
      0.0                     -> "0.0",
      -0.0                    -> "-0.0",
      100.0                   -> "100.0",
      0.001                   -> "0.001",
      0.000999                -> "9.99E-4",
      1e-4                    -> "1.0E-4",
      9999999.0               -> "9999999.0",
      1e7                     -> "1.0E7",
      12345678.9              -> "1.23456789E7",
      1e21                    -> "1.0E21",
      1.5e300                 -> "1.5E300",
      6.123233995736766e-17   -> "6.123233995736766E-17",
      -1.2246467991473532e-16 -> "-1.2246467991473532E-16",
      0.1 + 0.2               -> "0.30000000000000004",
      Double.NaN              -> "NaN",
      Double.PositiveInfinity -> "Infinity",
      Double.NegativeInfinity -> "-Infinity"
    )
    for (value, text) <- cases do
      assertEquals(jvmText(value), text)
      assertEquals(relaidAsOnTheJVM(value), text)
  }

  test("Direction comes from degrees, radians and deltas, with NoDirection for no change") {
    assertEquals(Direction.fromDeltas(0, 0), Direction.NoDirection)
    assert(Direction.fromDeltas(0, 0) eq Direction.NoDirection)
    assertEquals(Direction.fromDeltas(-0.0, 0.0).toString, "(no direction)")
    assertEquals(Direction.NoDirection.toDegrees, 0.0)
    assertEquals(1.0 / Direction.Right.toDegrees, Double.PositiveInfinity)
    assertEquals(Direction.Up.toString, "Direction.Up")
    assertEqualsDouble(Direction.fromDeltas(0, -5).dx, 0.0, 1e-15)
    assertEqualsDouble(Direction.fromDeltas(0, -5).dy, -1.0, 1e-15)
    assertEqualsDouble(Direction.fromDegrees(90).toDegrees, 90.0, 1e-12)
    assertEqualsDouble(Direction.fromRadians(-math.Pi / 4).toDegrees, 315.0, 1e-12)
    assertEquals(Direction.fromDegrees(30).roughly, "dx=0.87,dy=-0.50")
    for _ <- 1 to 20 do
      val random = Direction.random()
      assertEqualsDouble(math.hypot(random.dx, random.dy), 1.0, 1e-12)
  }

  test("a direction's constants print their names, and equal directions their components") {
    assertEquals(Direction.fromDeltas(1, 0), Direction.Right)
    assertEquals(Direction.Right.toString, "Direction.Right")
    assertEquals(Direction.fromDeltas(1, 0).toString, "Direction(dx=1.0,dy=0.0)")
  }

  test("Velocity adds by components, keeps its speed non-negative, and prints by direction") {
    val v = Velocity(3, -4)
    assertEqualsDouble(v.speed, 5.0, 1e-12)
    assertEqualsDouble((v + Velocity(1, 1)).dx, 4.0, 1e-12)
    assertEqualsDouble((v + Velocity(1, 1)).dy, -3.0, 1e-12)
    assertEqualsDouble((v - v).speed, 0.0, 1e-12)
    assertEquals((v * 2).direction, v.direction)
    assertEquals(v.slower(100).speed, 0.0)
    assertEquals(v.noFasterThan(-1).speed, 0.0)
    assertEquals(Velocity(Direction.Down, 2).toString, "Velocity(Direction.Down,2.0)")
    assertEquals(Velocity(0, 0), Velocity.Still)
    assertEquals(Velocity.Still.toString, "Velocity((no direction),0.0)")
    val negative = intercept[IllegalArgumentException](Velocity(Direction.Up, -1))
    assert(negative.getMessage.contains("cannot be negative"), negative.getMessage)
    assertEquals(Pos(10, 10).nextPos(Velocity(Direction.Left, 4)), Pos(6, 10))
    assertEquals(Velocity(Direction.Left, 4).nextFrom(Pos(10, 10)), Pos(6, 10))
  }

  test("SMILe's Pos members still work beside the course library's") {
    assertEquals(Pos(1, 2).moveBy(3, 4), Pos(1, 2).add(3, 4))
    assertEquals(Pos(1, 0).rotateAroundOrigin(90), Pos(0, -1))
    assertEquals(Pos(2, 0).rotateAround(90, Pos(1, 0)), Pos(1, -1))
    assertEquals(Pos(1, 0).rotateAroundOrigin(Direction.Up.toDegrees), Pos(0, -1))
    assertEquals(Pos(4, 6).centerBetween(Pos(0, 0)), Pos(2, 3))
    assertEquals(Pos((1.0, 2.0)), Pos(1, 2))
  }
