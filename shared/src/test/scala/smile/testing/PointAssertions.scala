package smile.testing

import smile.modeling.Pos

/** Assertions about the vertices of a shape, to the precision of a five-decimal table. */
trait PointAssertions:
  self: munit.Assertions =>

  protected def assertPoints(obtained: Seq[Pos], expected: Seq[(Double, Double)]): Unit =
    assertEquals(obtained.size, expected.size, s"$obtained")
    for (point, (x, y)) <- obtained.zip(expected) do
      assertEqualsDouble(point.x, x, 5e-5, s"x of $point in $obtained, expected $expected")
      assertEqualsDouble(point.y, y, 5e-5, s"y of $point in $obtained, expected $expected")
