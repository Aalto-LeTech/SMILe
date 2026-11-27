package smile.modeling

/** What version 3.8.4 of the course library's own `Pos`, `Direction` and `Velocity` printed for the
  * probes that [[smile.modeling.O1ModelTypesSuite]] repeats. These are the `pos_`, `dir_` and
  * `vel_` lines of a run of the course library's behaviour-baseline harness on the JVM, copied
  * verbatim. The three `dir_` lines for Swing keys are left out, as those methods stay there.
  *
  * `lines` is never edited to follow SMILe. Where SMILe deliberately prints something else, the
  * entry is listed in `deliberateDifferences` with SMILe's text and the reason, and the suite
  * checks it against the course library's value as that kind of difference says.
  */
object O1GoldenValues:

  /** How SMILe's value for an entry relates to the course library's. */
  enum Difference derives CanEqual:
    /** The same numbers to within 4 ulp or 1e-15, as the course library's `6e-17` is where an exact
      * `0` is meant.
      */
    case Near

    /** The same angle a whole number of turns apart, as SMILe wraps an angle into [0, 360). */
    case Turns

    /** A different answer, because of what the exact values make true. */
    case Changed

  /** An entry where SMILe deliberately differs from the course library: SMILe's text, the kind of
    * difference and why.
    */
  final case class Departure(smile: String, kind: Difference, reason: String)

  private val rightAngles = "SMILe computes the sine and cosine of a right angle exactly"
  private val wrapped     = "SMILe wraps an angle into [0, 360) before taking sine and cosine"

  /** By entry name, where SMILe's output differs from `lines`. */
  val deliberateDifferences: Map[String, Departure] = Map(
    "dir_fromDegrees_90"  -> Departure("Direction(dx=0.0,dy=-1.0)", Difference.Near, rightAngles),
    "dir_fromDegrees_180" -> Departure("Direction(dx=-1.0,dy=-0.0)", Difference.Near, rightAngles),
    "dir_fromDegrees_270" -> Departure("Direction(dx=0.0,dy=1.0)", Difference.Near, rightAngles),
    "dir_fromDegrees_270.roughly" -> Departure("dx=0.00,dy=1.00", Difference.Near, rightAngles),
    "dir_fromDegrees_360" -> Departure("Direction(dx=1.0,dy=-0.0)", Difference.Near, rightAngles),
    "dir_fromDegrees_360.toDegrees" -> Departure("0.0", Difference.Turns, wrapped),
    "dir_fromDegrees_360.roughly"   -> Departure("dx=1.00,dy=-0.00", Difference.Near, rightAngles),
    "dir_fromDegrees_720" -> Departure("Direction(dx=1.0,dy=-0.0)", Difference.Near, rightAngles),
    "dir_fromDegrees_720.toDegrees" -> Departure("0.0", Difference.Turns, wrapped),
    "dir_fromDegrees_720.roughly"   -> Departure("dx=1.00,dy=-0.00", Difference.Near, rightAngles),
    "dir_fromDegrees_m45"           -> Departure(
      "Direction(dx=0.7071067811865474,dy=0.7071067811865477)",
      Difference.Near,
      wrapped
    ),
    "dir_cw" -> Departure(
      "Direction(dx=0.9659258262890683,dy=0.2588190451025207)",
      Difference.Near,
      wrapped
    ),
    "vel_cw" -> Departure(
      "Velocity(dx=5.000000000000001,dy=8.660254037844386)",
      Difference.Near,
      wrapped
    ),
    "dir_equality" -> Departure("true", Difference.Changed, rightAngles + ", so 90 degrees is Up")
  )

  val lines: String =
    """
      |pos_toString -> (3.0,4.0)
      |pos_toString_fraction -> (-1.5,2.25)
      |pos_roughly -> 1.23,-7.89
      |pos_description -> (-1.5,2.25)
      |pos_add -> (1.5,6.25)
      |pos_add_xy -> (4.0,6.0)
      |pos_offset -> (1.5,6.25)
      |pos_subtract -> (4.5,1.75)
      |pos_subtract_xy -> (2.0,3.0)
      |pos_unaryMinus -> (-3.0,-4.0)
      |pos_multiply -> (7.5,10.0)
      |pos_divide -> (1.0,1.3333333333333333)
      |pos_addX_addY -> (4.0,3.0)
      |pos_subtractX_Y -> (2.0,3.0)
      |pos_setX_setY -> (10.0,20.0)
      |pos_withX -> (30.0,4.0)
      |pos_withY -> (3.0,-6.0)
      |pos_withXY -> (4.0,3.0)
      |pos_xDiff_yDiff -> (-4.5,-1.75)
      |pos_distance -> 5.0
      |pos_distance_b -> 4.828301978956992
      |pos_vectorTo -> (-4.5,-1.75)
      |pos_directionOf -> Direction(dx=-0.932004671541296,dy=-0.36244626115494827)
      |pos_directionOf_self -> (no direction)
      |pos_nextPos -> (4.0,6.0)
      |pos_clamp -> (10.0,0.0)
      |pos_clampX -> (10.0,-50.0)
      |pos_clampY -> (50.0,0.0)
      |pos_noLowerThan -> (5.0,10.0)
      |pos_noHigherThan -> (5.0,10.0)
      |pos_noFurtherLeftThan -> (0.0,5.0)
      |pos_noFurtherRightThan -> (10.0,5.0)
      |pos_toPair -> (3.0,4.0)
      |pos_turnInto -> 12.0
      |pos_fromPair -> (7.0,8.0)
      |pos_equality -> true
      |pos_hashCode_eq -> true
      |pos_origin -> (0.0,0.0)
      |pos_negZero -> (-0.0,0.0)
      |dir_fromDegrees_0 -> Direction(dx=1.0,dy=-0.0)
      |dir_fromDegrees_0.toDegrees -> 0.0
      |dir_fromDegrees_0.roughly -> dx=1.00,dy=-0.00
      |dir_fromDegrees_30 -> Direction(dx=0.8660254037844387,dy=-0.49999999999999994)
      |dir_fromDegrees_30.toDegrees -> 29.999999999999993
      |dir_fromDegrees_30.roughly -> dx=0.87,dy=-0.50
      |dir_fromDegrees_45 -> Direction(dx=0.7071067811865476,dy=-0.7071067811865475)
      |dir_fromDegrees_45.toDegrees -> 45.0
      |dir_fromDegrees_45.roughly -> dx=0.71,dy=-0.71
      |dir_fromDegrees_90 -> Direction(dx=6.123233995736766E-17,dy=-1.0)
      |dir_fromDegrees_90.toDegrees -> 90.0
      |dir_fromDegrees_90.roughly -> dx=0.00,dy=-1.00
      |dir_fromDegrees_135 -> Direction(dx=-0.7071067811865475,dy=-0.7071067811865476)
      |dir_fromDegrees_135.toDegrees -> 135.0
      |dir_fromDegrees_135.roughly -> dx=-0.71,dy=-0.71
      |dir_fromDegrees_180 -> Direction(dx=-1.0,dy=-1.2246467991473532E-16)
      |dir_fromDegrees_180.toDegrees -> 180.0
      |dir_fromDegrees_180.roughly -> dx=-1.00,dy=-0.00
      |dir_fromDegrees_225 -> Direction(dx=-0.7071067811865477,dy=0.7071067811865475)
      |dir_fromDegrees_225.toDegrees -> 225.0
      |dir_fromDegrees_225.roughly -> dx=-0.71,dy=0.71
      |dir_fromDegrees_270 -> Direction(dx=-1.8369701987210297E-16,dy=1.0)
      |dir_fromDegrees_270.toDegrees -> 270.0
      |dir_fromDegrees_270.roughly -> dx=-0.00,dy=1.00
      |dir_fromDegrees_359 -> Direction(dx=0.9998476951563913,dy=0.01745240643728356)
      |dir_fromDegrees_359.toDegrees -> 359.0
      |dir_fromDegrees_359.roughly -> dx=1.00,dy=0.02
      |dir_fromDegrees_360 -> Direction(dx=1.0,dy=2.4492935982947064E-16)
      |dir_fromDegrees_360.toDegrees -> 360.0
      |dir_fromDegrees_360.roughly -> dx=1.00,dy=0.00
      |dir_fromDegrees_m45 -> Direction(dx=0.7071067811865476,dy=0.7071067811865475)
      |dir_fromDegrees_m45.toDegrees -> 315.0
      |dir_fromDegrees_m45.roughly -> dx=0.71,dy=0.71
      |dir_fromDegrees_720 -> Direction(dx=1.0,dy=4.898587196589413E-16)
      |dir_fromDegrees_720.toDegrees -> 359.99999999999994
      |dir_fromDegrees_720.roughly -> dx=1.00,dy=0.00
      |dir_fromRadians_pi -> Direction(dx=-1.0,dy=-1.2246467991473532E-16)
      |dir_fromDeltas -> Direction(dx=0.6000000000000001,dy=-0.7999999999999999)
      |dir_fromDeltas_zero -> (no direction)
      |dir_presets -> List(Direction.Up, Direction.Down, Direction.Left, Direction.Right, (no direction))
      |dir_preset_degrees -> List(90.0, 270.0, 180.0, 0.0, 0.0)
      |dir_preset_radians -> List(1.5707963267948966, 3.141592653589793)
      |dir_dx_dy -> (0.8660254037844387,-0.49999999999999994)
      |dir_switchX -> Direction(dx=-0.8660254037844387,dy=-0.49999999999999994)
      |dir_switchY -> Direction(dx=0.8660254037844387,dy=0.49999999999999994)
      |dir_opposite -> Direction(dx=-0.8660254037844387,dy=0.49999999999999994)
      |dir_cw -> Direction(dx=0.9659258262890682,dy=0.2588190451025209)
      |dir_ccw -> Direction(dx=0.25881904510252074,dy=-0.9659258262890683)
      |dir_plus -> Direction(dx=2.83276944882399E-16,dy=-1.0)
      |dir_flags -> (true,false,true,false)
      |dir_flags_none -> (true,true,true,true)
      |dir_sharesQuadrant -> (true,false,true)
      |dir_equality -> false
      |dir_equality_deltas -> false
      |vel_toString -> Velocity(dx=8.660254037844387,dy=-4.999999999999999)
      |vel_toString_special -> Velocity(Direction.Up,5.0)
      |vel_toString_still -> Velocity((no direction),0.0)
      |vel_dxdy -> Velocity(dx=3.0000000000000004,dy=-3.9999999999999996)
      |vel_fromPos -> Velocity(dx=6.000000000000001,dy=7.999999999999999)
      |vel_between -> Velocity(dx=3.0000000000000004,dy=3.9999999999999996)
      |vel_fields -> (8.660254037844387,-4.999999999999999,10.0,Direction(dx=0.8660254037844387,dy=-0.49999999999999994))
      |vel_plus -> Velocity(dx=9.660254037844387,dy=-3.9999999999999987)
      |vel_minus -> Velocity(dx=7.660254037844388,dy=-5.999999999999999)
      |vel_unaryMinus -> Velocity(dx=-8.660254037844387,dy=4.999999999999999)
      |vel_opposite -> Velocity(dx=-8.660254037844387,dy=4.999999999999999)
      |vel_cw -> Velocity(dx=4.999999999999999,dy=8.660254037844387)
      |vel_ccw -> Velocity(dx=-4.999999999999998,dy=-8.660254037844389)
      |vel_cw_m30 -> Velocity(dx=5.000000000000001,dy=-8.660254037844386)
      |vel_switchX -> Velocity(dx=-8.660254037844387,dy=-4.999999999999999)
      |vel_switchY -> Velocity(dx=8.660254037844387,dy=4.999999999999999)
      |vel_changeDirection -> Velocity(Direction.Left,10.0)
      |vel_changeSpeed -> Velocity(dx=2.598076211353316,dy=-1.4999999999999998)
      |vel_times -> Velocity(dx=17.320508075688775,dy=-9.999999999999998)
      |vel_div -> Velocity(dx=2.165063509461097,dy=-1.2499999999999998)
      |vel_faster -> Velocity(dx=12.99038105676658,dy=-7.499999999999999)
      |vel_slower -> Velocity(dx=0.0,dy=-0.0)
      |vel_stopped -> Velocity(dx=0.0,dy=-0.0)
      |vel_noFasterThan -> Velocity(dx=3.464101615137755,dy=-1.9999999999999998)
      |vel_noSlowerThan -> Velocity(dx=34.64101615137755,dy=-19.999999999999996)
      |vel_nextFrom -> (18.66025403784439,5.000000000000001)
      |vel_roughly -> 10.00 with direction dx=0.87,dy=-0.50
      |vel_toPos -> (8.660254037844387,-4.999999999999999)
      |vel_isStill -> (false,true)
      |vel_sum -> Velocity(dx=-1.0,dy=1.0000000000000002)
      |vel_sum_empty -> Velocity((no direction),0.0)
      |vel_average -> Velocity(dx=1.0000000000000002,dy=1.0)
      |vel_average_empty -> Velocity((no direction),0.0)
      |vel_equality -> true
      |""".stripMargin

  /** The probe names and the text the course library printed for them, in the harness's order. */
  val expected: Seq[(String, String)] =
    lines.linesIterator
      .filter(_.nonEmpty)
      .map { line =>
        val arrow = line.indexOf(" -> ")
        line.take(arrow) -> line.drop(arrow + 4)
      }
      .toSeq
