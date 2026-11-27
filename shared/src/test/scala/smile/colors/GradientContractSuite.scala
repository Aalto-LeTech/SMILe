package smile.colors

import smile.colors.LinearGradient.CycleMethod
import smile.modeling.Pos
import smile.pictures.*
import smile.testing.Cases

/** A `LinearGradient` that can be constructed can be painted. Every value a renderer would choke on
  * is refused when the gradient is made, with a message about the gradient. The JVM's own
  * complaints ("Keyframe fractions must be increasing", "Start point cannot equal endpoint") came
  * out of the middle of a render, long after the mistake.
  */
class GradientContractSuite extends munit.FunSuite:

  private val red  = Cases.red
  private val blue = Cases.blue

  private def gradient(fractions: Seq[Double], colors: Seq[Color], end: Pos = Pos(10, 0)) =
    LinearGradient(Pos(0, 0), end, fractions.zip(colors))

  private def refusal(body: => Any): String = intercept[IllegalArgumentException](body).getMessage

  private def renders(paint: LinearGradient): Bitmap =
    Rectangle(20, 10, Pos.Origin, fillWith(paint), Some(StrokeStyle(paint, 2))).toBitmap()

  test("there are at least two stops") {
    assert(refusal(gradient(Seq(0.0), Seq(red))).contains("two"))
    assert(refusal(gradient(Seq.empty, Seq.empty)).contains("two"))
  }

  test("fractions lie within zero to one and do not run backwards") {
    for bad <- Seq(Seq(-0.1, 1.0), Seq(0.0, 1.1), Seq(0.0, 1.5), Seq(0.0, Double.NaN)) do
      val message = refusal(gradient(bad, Seq(red, blue)))
      assert(message.contains("within [0.0, 1.0]"), message)

    val backwards = refusal(gradient(Seq(1.0, 0.0), Seq(red, blue)))
    // Numbers print as "1.0" on the JVM and as "1" in JavaScript.
    assert(backwards.contains("order") && backwards.contains("were: 1"), backwards)
    assert(refusal(gradient(Seq(0.0, 0.6, 0.4, 1.0), Seq(red, blue, red, blue))).contains("order"))
  }

  test("the two points differ, and are numbers") {
    val same = refusal(gradient(Seq(0.0, 1.0), Seq(red, blue), end = Pos(0, 0)))
    assert(same.contains("different points") && same.contains("(0.0,0.0)"), same)
    assert(refusal(LinearGradient(Pos(5, 5), Pos(5, 5), red, blue)).contains("different points"))
    assert(refusal(LinearGradient(Pos(0, 0), Pos(Double.NaN, 0), red, blue)).contains("finite"))
    assert(
      refusal(LinearGradient(Pos(0, 0), Pos(Double.PositiveInfinity, 0), red, blue))
        .contains("finite")
    )
  }

  test("two stops at one fraction are a hard edge between their colors, on every backend") {
    // The canvas and SVG take equal offsets as they are. The JVM wants them strictly
    // increasing and is handed them a hair apart.
    val hardEdge = gradient(Seq(0.0, 0.5, 0.5, 1.0), Seq(red, red, blue, blue), end = Pos(20, 0))
    val bitmap   = Rectangle(20, 10, Pos.Origin, fillWith(hardEdge), None).toBitmap()

    assertEquals(bitmap.getColor(7, 5), red)
    assertEquals(bitmap.getColor(12, 5), blue)

    for fractions <- Seq(
        Seq(0.0, 0.0, 1.0),
        Seq(0.0, 1.0, 1.0),
        Seq(1.0, 1.0),
        Seq(0.0, 0.0),
        Seq(0.3, 0.3, 0.3),
        Seq(0.3, 0.3 + 1e-12, 1.0)
      )
    do
      val colors = Seq(red, blue, red).take(fractions.size)
      val _      = renders(gradient(fractions, colors))
  }

  test("every gradient that can be constructed renders, whatever its cycle method and direction") {
    val ends      = Seq(Pos(10, 0), Pos(0, 10), Pos(-7, 3), Pos(1e-7, 0), Pos(500, 500))
    val fractions = Seq(Seq(0.0, 1.0), Seq(0.2, 0.8), Seq(0.0, 0.5, 1.0))
    for end <- ends; stops <- fractions; cycle <- CycleMethod.values do
      val paint  = LinearGradient(Pos(0, 0), end, stops.zip(Seq(red, blue, red)), cycle)
      val bitmap = renders(paint)
      assert(bitmap.pixels().colors.exists(_.opacity > 0), s"to $end, at $stops, $cycle")
  }

  test("a gradient too short for a renderer's floats is painted as its average color") {
    // Two points a ten-millionth apart round to the same single-precision value, so they are one
    // point to every backend. Left to the backends, each makes something else of it. The results
    // are an exception, nothing at all, or the last stop's color.
    val speck  = LinearGradient(Pos(3, 3), Pos(3 + 1e-7, 3), red, blue)
    val bitmap = Rectangle(20, 10, Pos.Origin, fillWith(speck), None).toBitmap()
    val middle = bitmap.getColor(10, 5)

    assertEquals(middle.opacity, 255)
    assert((middle.red - 128).abs <= 1 && (middle.blue - 128).abs <= 1 && middle.green == 0, middle)
    assertEquals(bitmap.getColor(1, 1), middle)
    assertEquals(bitmap.getColor(18, 8), middle)
  }

  test("a gradient is a value: equal by its fields, and unchanged by the shapes that use it") {
    val one   = LinearGradient(Pos(0, 0), Pos(10, 0), red, blue)
    val other = LinearGradient(Pos(0, 0), Pos(10, 0), Seq(0.0 -> red, 1.0 -> blue))

    assertEquals(one, other)
    assertEquals(one.hashCode, other.hashCode)
    assertNotEquals(one, other.copy(cycleMethod = CycleMethod.Reflect))
    assertEquals(
      LinearGradient(Pos(0, 0), Pos(10, 0), red, blue, CycleMethod.Repeat),
      other.copy(cycleMethod = CycleMethod.Repeat)
    )

    val square = Rectangle(20, 20, Pos.Origin, fillWith(one), None)
    val _      = square.rotateAround(30, Pos(5, 5)).scaleBy(2, 3)
    assertEquals(one, other)
    assertEquals(square.fill, fillWith(other))
  }

  test("a gradient of two colors does not cycle unless asked to") {
    assertEquals(LinearGradient(Pos(0, 0), Pos(1, 0), red, blue).cycleMethod, CycleMethod.NoCycle)
  }

  test("the average color integrates the gradient over its length") {
    assertEquals(LinearGradient(Pos(0, 0), Pos(10, 0), red, blue).averageColor, Color(128, 0, 128))
    assertEquals(
      LinearGradient(Pos(0, 0), Pos(10, 0), Seq(0.0 -> red, 0.9 -> red, 1.0 -> blue)).averageColor,
      Color(242, 0, 13)
    )
    // Before the first stop and after the last, the end colors hold.
    assertEquals(
      LinearGradient(Pos(0, 0), Pos(10, 0), Seq(0.5 -> red, 0.5 -> blue)).averageColor,
      Color(128, 0, 128)
    )
  }

  test("the average color weighs each color by its opacity, as the renderers interpolate it") {
    // Transparent is white without opacity. The renderers interpolate unpremultiplied, so a fade
    // to it passes through pink, and so does its average.
    assertEquals(PresetColor.Transparent, Color(255, 255, 255, 0))
    assertEquals(
      LinearGradient(Pos(0, 0), Pos(10, 0), red, PresetColor.Transparent).averageColor,
      Color(255, 85, 85, 128)
    )
    assertEquals(
      LinearGradient(
        Pos(0, 0),
        Pos(10, 0),
        Color(10, 20, 30, 0),
        Color(30, 40, 50, 0)
      ).averageColor,
      Color(0, 0, 0, 0)
    )
  }
