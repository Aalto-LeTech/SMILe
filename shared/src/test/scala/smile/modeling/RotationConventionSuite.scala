package smile.modeling

import smile.testing.InkAssertions

/** A positive angle turns counterclockwise on screen, where the y axis points down. This holds for
  * positions, the transformation matrix, `Pos`'s exact quarter turns, and the named constants.
  * Rotations keep the pivot and every distance from it, and undo each other.
  */
class RotationConventionSuite extends munit.FunSuite with InkAssertions:

  test("a positive angle rotates positions counterclockwise on screen") {
    // The y axis points down, so three o'clock turns to twelve o'clock.
    assertEquals(Pos(1, 0).rotateAroundOrigin(90), Pos(0, -1))
    assertEquals(Pos(1, 0).rotateAroundOrigin(-90), Pos(0, 1))
    assertEquals(Pos(1, 0).rotateAroundOrigin(180), Pos(-1, 0))
    assertEquals(Pos(1, 0).rotateAroundOrigin(270), Pos(0, 1))
    assertEquals(Pos(11, 10).rotateAround(90, Pos(10, 10)), Pos(10, 9))
    assertEquals(Pos(11, 10).rotateAround(-90, Pos(10, 10)), Pos(10, 11))

    // The general, trigonometric path turns the same way as the exact right-angle ones.
    val turned = Pos(2, 0).rotateAroundOrigin(30)
    assertEqualsDouble(turned.x, math.sqrt(3), 1e-12)
    assertEqualsDouble(turned.y, -1.0, 1e-12)
    val turnedAroundPivot = Pos(12, 10).rotateAround(30, Pos(10, 10))
    assertEqualsDouble(turnedAroundPivot.x, 10 + math.sqrt(3), 1e-12)
    assertEqualsDouble(turnedAroundPivot.y, 9.0, 1e-12)
    val almostQuarter = Pos(1, 0).rotateAroundOrigin(90.000001)
    assertEqualsDouble(almostQuarter.x, 0.0, 1e-6)
    assertEqualsDouble(almostQuarter.y, -1.0, 1e-6)
  }

  test("Pos rotations keep the pivot fixed and are reversible") {
    val point = Pos(13, -4)
    val pivot = Pos(2, 7)

    assertEquals(pivot.rotateAround(90, pivot), pivot)
    assertEquals(point.rotateAround(90, pivot).rotateAround(-90, pivot), point)

    // The same holds at every case angle, off the exact path a quarter turn takes.
    for angle <- smile.testing.Cases.angles do
      assertClose(pivot.rotateAround(angle, pivot), pivot, 1e-12, s"the pivot, by $angle")
      assertClose(
        point.rotateAround(angle, pivot).rotateAround(-angle, pivot),
        point,
        1e-9,
        s"by $angle and back"
      )
      // A rotation keeps every distance from the pivot.
      val turned = point.rotateAround(angle, pivot) - pivot
      assertEqualsDouble(math.hypot(turned.x, turned.y), math.hypot(11, -11), 1e-9, s"by $angle")
  }

  test("origin rotations match point rotations around the origin") {
    val point = Pos(13, -4)

    assertEquals(point.rotateAroundOrigin(90), point.rotateAround(90, Pos.Origin))
    assertEquals(point.rotateAroundOrigin(-90), point.rotateAround(-90, Pos.Origin))
    assertEquals(point.rotateAroundOrigin(37), point.rotateAround(37, Pos.Origin))
  }

  test("Affine.rotation keeps the pivot fixed") {
    val transformation = Affine.rotation(90, Pos(10, 20))

    // Its translation lies on the 2^-20 pixel grid, so it is exact to about a millionth of a pixel.
    assertClose(transformation(Pos(10, 20)), Pos(10, 20), 1e-6, "the pivot")

    // The pivot stays put whichever way the rotation turns. Another point tells the
    // direction. The matrix takes it where Pos's counterclockwise rotation does.
    assertClose(
      transformation(Pos(11, 20)),
      Pos(10, 19),
      1e-6,
      "a quarter turn counterclockwise"
    )
    for angle <- Seq(30.0, -45.0, 200.0) do
      assertClose(
        Affine.rotation(angle, Pos(10, 20))(Pos(13, 16)),
        Pos(13, 16).rotateAround(angle, Pos(10, 20)),
        1e-6,
        s"by $angle"
      )
  }

  test("a quarter turn by -90 is clockwise on screen and by 90 counterclockwise") {
    val right = Pos(1, 0)
    assertEquals(right.rotateAroundOrigin(-90), Pos(0, 1))
    assertEquals(right.rotateAroundOrigin(90), Pos(0, -1))
  }

  test("the exact quarter turns go the way seen on screen") {
    val pivot = Pos(10, 10)

    // The y axis points down, so "down" is positive y. Clockwise carries the three o'clock
    // point to six o'clock, counterclockwise to twelve o'clock.
    assertEquals(Pos(11, 10).rotateAround(-90, pivot), Pos(10, 11))
    assertEquals(Pos(11, 10).rotateAround(90, pivot), Pos(10, 9))

    // An angle and the one a full turn from it are the same quarter turn.
    val point = Pos(13, -4)
    assertEquals(point.rotateAroundOrigin(90), Pos(-4, -13))
    assertEquals(point.rotateAroundOrigin(-270), Pos(-4, -13))
    assertEquals(point.rotateAroundOrigin(-90), Pos(4, 13))
    assertEquals(point.rotateAroundOrigin(270), Pos(4, 13))
    assertEquals(point.rotateAround(-270, pivot), point.rotateAround(90, pivot))
    assertEquals(point.rotateAround(270, pivot), point.rotateAround(-90, pivot))
    val general = Pos(2, 0).rotateAroundOrigin(30)
    assertEqualsDouble(general.x, math.sqrt(3), 1e-12)
    assertEqualsDouble(general.y, -1.0, 1e-12)
  }
