package smile.modeling

import smile.pictures.*
import smile.testing.Cases

/** The small model types everything else is built from: `Bounds` and `Pos`, and the buffer flips
  * and the point that the elements rest on.
  */
class ModelContractSuite extends munit.FunSuite:

  test("every way of making Bounds puts the upper left corner up and to the left") {
    val expected = Bounds(Pos(-5, -2), Pos(5, 2))

    assertEquals(Bounds(Pos(5, 2), Pos(-5, -2)), expected)
    assertEquals(Bounds(Pos(-5, 2), Pos(5, -2)), expected)
    assertEquals(Bounds(5, -2, -5, 2), expected)
    assertEquals(Bounds(Pos.Origin, 10.0, 4.0), expected)
    assertEquals(Bounds(Pos.Origin, 10, 4), expected)
    // A size given negative gives the same box, as a box cannot be turned inside out.
    assertEquals(Bounds(Pos.Origin, -10.0, 4.0), expected)
    assertEquals(Bounds(Pos.Origin, 10, -4), expected)

    for bounds <- Seq(Bounds(Pos.Origin, -10.0, -4.0), Bounds(Pos(5, 2), Pos(-5, -2))) do
      assert(bounds.width >= 0 && bounds.height >= 0, bounds)
  }

  test("Bounds knows its size, its center, and its four corners") {
    val bounds = Bounds(Pos(10, 20), Pos(50, 100))

    assertEquals(bounds.width, 40.0)
    assertEquals(bounds.height, 80.0)
    assertEquals(bounds.center, Pos(30, 60))
    assertEquals(bounds.upperRightCorner, Pos(50, 20))
    assertEquals(bounds.lowerLeftCorner, Pos(10, 100))
    assertEquals(bounds.moveBy(-10, 5), Bounds(Pos(0, 25), Pos(40, 105)))
    assertEquals(bounds.moveBy(3, 4).width, bounds.width)
  }

  test("Pos arithmetic works by coordinate, with positions, pairs, and numbers alike") {
    val pos = Pos(6, -8)

    assertEquals(pos + Pos(1, 2), Pos(7, -6))
    assertEquals(pos + (1.0, 2.0), Pos(7, -6))
    assertEquals(pos - Pos(1, 2), Pos(5, -10))
    assertEquals(pos - (1.0, 2.0), Pos(5, -10))
    assertEquals(pos * Pos(2, 0.5), Pos(12, -4))
    assertEquals(pos * (2.0, 0.5), Pos(12, -4))
    assertEquals(pos * 0.5, Pos(3, -4))
    assertEquals(pos / Pos(2, 4), Pos(3, -2))
    assertEquals(pos / (2.0, 4.0), Pos(3, -2))
    assertEquals(pos / 2.0, Pos(3, -4))
    assertEquals(pos.moveBy(4, 8), Pos(10, 0))
    assertEquals(Pos.Origin, Pos(0, 0))
  }

  test("centerBetween is the midpoint, from either end") {
    assertEquals(Pos(2, 4).centerBetween(Pos(10, -6)), Pos(6, -1))
    assertEquals(Pos(10, -6).centerBetween(Pos(2, 4)), Pos(6, -1))
    assertEquals(Pos(3, 3).centerBetween(Pos(3, 3)), Pos(3, 3))
  }

  test("Pos scales away from a fixed point, and from the origin") {
    assertEquals(Pos(6, -8).scaleBy(2, 0.5, Pos(4, 0)), Pos(8, -4))
    assertEquals(Pos(6, -8).scaleBy(-1, 1, Pos(4, 0)), Pos(2, -8))
    assertEquals(Pos(6, -8).scaleByRelativeToOrigin(2, 0.5), Pos(12, -4))
    for
      point  <- Seq(Pos(6, -8), Pos(-1.5, 2))
      (h, v) <- Cases.positiveFactorPairs
      pivot  <- Cases.pivots
    do
      assertEquals(pivot.scaleBy(h, v, pivot), pivot)
      assertEquals(point.scaleBy(h, v, Pos.Origin), point.scaleByRelativeToOrigin(h, v))
  }

  test("the flips of a bitmap's pixels are transformations that undo themselves") {
    def linearPart(affine: Affine) = (affine.a, affine.b, affine.c, affine.d)

    assertEquals(Affine.Identity(Pos(3, 4)), Pos(3, 4))
    assertEquals(
      linearPart(Affine.mirroring(horizontally = true, vertically = false, Pos(4, 2))),
      (-1.0, 0.0, 0.0, 1.0)
    )
    assertEquals(
      linearPart(Affine.mirroring(horizontally = false, vertically = true, Pos(4, 2))),
      (1.0, 0.0, 0.0, -1.0)
    )
    assertEquals(
      linearPart(Affine.mirroring(horizontally = true, vertically = true, Pos(4, 2))),
      (-1.0, 0.0, 0.0, -1.0)
    )

    val bitmap = Cases.twoColorBitmap
    assertEquals(
      bitmap.flipHorizontally.flipHorizontally.pixels().colors,
      bitmap.pixels().colors
    )
    assertEquals(bitmap.flipVertically.flipVertically.pixels().colors, bitmap.pixels().colors)
    assertEquals(bitmap.flipDiagonally.flipDiagonally.pixels().colors, bitmap.pixels().colors)
    assertEquals(
      bitmap.flipHorizontally.flipVertically.pixels().colors,
      bitmap.flipDiagonally.pixels().colors
    )
    // Red was on the left.
    assertEquals(bitmap.flipHorizontally.getColor(0, 0), Cases.blue)
    assertEquals(bitmap.flipHorizontally.getColor(7, 3), Cases.red)
    assertEquals(bitmap.flipHorizontally.bounds, bitmap.bounds)
  }

  test("a point's bounds are the pixel it paints, and it scales like a shape") {
    val point = Point(Pos(7, -4), Cases.red)
    assertEquals(point.bounds, Some(Bounds(Pos(6.5, -4.5), Pos(7.5, -3.5))))
    // A point is a 1x1 square, so a pixel-art picture scaled 3x shows 3x3 squares.
    assertEquals(point.scaleBy(3, 3).bounds, Some(Bounds(Pos(5.5, -5.5), Pos(8.5, -2.5))))
    assertEquals(point.scaleAround(3, 3, Pos(1, 1)).anchor, Pos(19, -14))
  }

  test("Bounds normalizes corners given in reverse order") {
    assertEquals(Bounds(Pos(5, 5), Pos(0, 0)), Bounds(Pos(0, 0), Pos(5, 5)))
    assertEquals(Bounds(Pos(3, 3), Pos(3, 3)).width, 0.0)
  }
