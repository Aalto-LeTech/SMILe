package smile.modeling

import smile.pictures.*
import smile.testing.{Cases, InkAssertions}

/** Rotations by right angles take exact shortcuts all over the library: `Pos`'s dispatch on 90,
  * 180, 270, an arc's swapped axes, its named axis extremes. Each shortcut has to agree with the
  * trigonometry it replaces. The suite checks that without reaching into either. A rotation by
  * `a + 360` degrees takes the general path to the place the shortcut for `a` names, and a boundary
  * cannot jump as an angle crosses a right one.
  */
class RotationPathsSuite extends munit.FunSuite with InkAssertions:

  private val points = Seq(Pos(1, 0), Pos(13, -4), Pos(-2.5, 7.25), Pos(0, 0))

  /** The rotation written out. A positive angle turns counterclockwise on screen. */
  private def byTrigonometry(point: Pos, angle: Double, pivot: Pos): Pos =
    val (sin, cos) = (math.sin(math.toRadians(angle)), math.cos(math.toRadians(angle)))
    val relative   = point - pivot
    pivot + (cos * relative.x + sin * relative.y, -sin * relative.x + cos * relative.y)

  test("the exact right-angle rotations agree with the trigonometric ones") {
    for angle <- Cases.rightAngles; point <- points; pivot <- Cases.pivots do
      val clue = s"$point by $angle around $pivot"
      assertClose(point.rotateAround(angle, pivot), byTrigonometry(point, angle, pivot), 1e-9, clue)
      assertClose(
        point.rotateAround(angle, pivot),
        point.rotateAround(angle + 360, pivot),
        1e-9,
        clue
      )
      assertClose(
        point.rotateAround(angle, pivot),
        point.rotateAround(angle - 720, pivot),
        1e-9,
        clue
      )
      assertClose(
        point.rotateAroundOrigin(angle),
        byTrigonometry(point, angle, Pos.Origin),
        1e-9,
        clue
      )
      assertClose(
        point.rotateAroundOrigin(angle),
        point.rotateAroundOrigin(angle + 360),
        1e-9,
        clue
      )
  }

  test("every rotation entry point computes the same rotation") {
    for angle <- Cases.angles ++ Cases.seededAngles; point <- points; pivot <- Cases.pivots do
      val expected = byTrigonometry(point, angle, pivot)
      val clue     = s"$point by $angle around $pivot"
      assertClose(point.rotateAround(angle, pivot), expected, 1e-9, clue)
      assertClose(
        point.rotateAroundOrigin(angle),
        byTrigonometry(point, angle, Pos.Origin),
        1e-9,
        clue
      )

      // The affine an element and a bitmap's pixels are turned with is the same rotation.
      assertClose(
        Affine.rotation(angle, pivot)(point),
        expected,
        // Its translation lies on the 2^-20 pixel grid.
        1e-6,
        s"$clue, by Affine.rotation"
      )
  }

  test("a boundary does not jump as a rotation angle crosses a right angle") {
    // An element just short of a right angle, at it, and just past it has three boundaries a
    // hair apart. A shortcut that named the wrong axis, or turned the wrong way, would put the
    // middle one somewhere else entirely.
    val nudge = 1e-7
    for element <- Cases.vectorGraphics; angle <- Cases.rightAngles; pivot <- Cases.pivots do
      val at = element.value.rotateAround(angle, pivot).bounds.get
      for neighbor <- Seq(angle - nudge, angle + nudge, angle + 360) do
        assertClose(
          at,
          element.value.rotateAround(neighbor, pivot).bounds.get,
          1e-4,
          s"${element.label} by $angle and by $neighbor around $pivot"
        )
  }

  test("an arc turned to a right angle has the boundary of its neighbors") {
    def arc(rotation: Double, extent: Double) =
      Arc(Pos(5, -3), 40, 20, 20, extent, fillWith(Cases.red), Cases.stroke(4))
        .rotateAround(rotation, Pos(5, -3))

    for angle <- Cases.rightAngles ++ Seq(450.0, -720.0); extent <- Seq(110.0, -200.0, 360.0) do
      val clue = s"rotation $angle, extent $extent"
      assertClose(arc(angle, extent).bounds.get, arc(angle + 1e-7, extent).bounds.get, 1e-4, clue)
      assertClose(arc(angle, extent).bounds.get, arc(angle - 1e-7, extent).bounds.get, 1e-4, clue)
  }
