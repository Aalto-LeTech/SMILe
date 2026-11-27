package smile.animation

import smile.colors.Color
import smile.modeling.Pos

class AnimationSuite extends munit.FunSuite:
  test("curves clamp input, preserve endpoints, and back deliberately overshoots") {
    val curves = Seq(
      Easing.linear,
      Easing.inQuad,
      Easing.outQuad,
      Easing.inOutQuad,
      Easing.inCubic,
      Easing.outCubic,
      Easing.inOutCubic,
      Easing.inOutSine,
      Easing.outBounce,
      Easing.outBack
    )
    for curve <- curves do
      assertEqualsDouble(curve(-1), 0, 1e-12)
      assertEqualsDouble(curve(2), 1, 1e-12)
      intercept[IllegalArgumentException](curve(Double.NaN))
    assert(Easing.outBack(0.7) > 1)
    assertEqualsDouble(Easing.inOutSine(0.5), 0.5, 1e-12)
  }

  test("timing has explicit endpoints, full-cycle periods, delays, and negative wrapping") {
    assertEquals(Timing.once(1, 2, delay = 1), 0.0)
    assertEquals(Timing.once(2, 2, delay = 1), 0.5)
    assertEquals(Timing.once(5, 2, delay = 1), 1.0)
    assertEquals(Timing.repeat(4, 4), 0.0)
    assertEquals(Timing.repeat(-1, 4), 0.75)
    assertEquals(Timing.repeat(0, 4, phase = 0.25), 0.25)
    assertEquals(Timing.pingPong(2, 4), 1.0)
    assertEquals(Timing.pingPong(4, 4), 0.0)
    assertEquals(Timing.sine(2, 4), 1.0)
    assertEquals(Timing.wrap(-11, -10, 10), 9.0)
    for bad <- Seq(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      intercept[IllegalArgumentException](Timing.repeat(0, bad))
  }

  test("interpolation preserves overshoot; color channels stay representable") {
    assertEquals(Tween.number(0, 10, 1.2), 12.0)
    assertEquals(Tween.position(Pos(0, 2), Pos(10, 4), 0.5), Pos(5, 3))
    assertEquals(
      Tween.color(Color(0, 20, 40, 0), Color(100, 40, 80, 100), 0.5),
      Color(50, 30, 60, 50)
    )
    assertEquals(Tween.color(Color(0, 0, 0), Color(200, 200, 200), 2), Color(255, 255, 255))
  }
