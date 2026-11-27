package smile.pictures

import smile.colors.Color
import smile.modeling.Pos
import smile.testing.PointAssertions

/** Where the shape factories put their vertices. The regular shapes start straight up, the
  * triangles put the apex on top, and an off-center shape keeps its size.
  * [[smile.pictures.FactoryContractSuite]] covers the sizes they owe and the arguments they refuse.
  */
class ShapeFactorySuite extends munit.FunSuite with PointAssertions:

  private val red = Color(255, 0, 0)

  private def pointsOf(shape: Element): Seq[Pos] = shape.vertices

  /** The vertices of a regular shape around its own center, where its circumradius starts. */
  private def aroundCenter(shape: Element): Seq[Pos] =
    val points = shape.vertices
    val center = Pos(points.map(_.x).sum / points.size, points.map(_.y).sum / points.size)
    points.map(_ - center)

  test("an off-center Triangle keeps its size and center") {
    val triangle = Triangle(10, Pos(100, 0), fillWith(red), line = None)

    assertEqualsDouble(triangle.bounds.get.width, 10.0, 1e-9)
    assertEqualsDouble(triangle.bounds.get.center.x, 100.0, 1e-9)
    // The height is an equilateral triangle's, centered on the position.
    assertEqualsDouble(triangle.bounds.get.height, 5 * math.sqrt(3), 1e-9)
    assertEqualsDouble(triangle.bounds.get.center.y, 0.0, 1e-9)
    assertEquals(triangle.anchor, Pos(100, 0))
    assertEquals(
      triangle.bounds.get,
      Triangle(10, Pos.Origin, fillWith(red), line = None).bounds.get.moveBy(100, 0)
    )
  }

  test("the regular shape factories keep their vertices: first one straight up") {
    // These factories place their vertices by rotating a point. The vertices are listed leftward
    // from the top one. Neither they nor their order depend on the angle convention.
    // They are measured around the shape's own center. A shape's anchor is the middle of its box,
    // which for a pentagon or a star lies a little below the center of its circle.
    assertPoints(
      aroundCenter(Pentagon(20, 20, Pos.Origin, fillWith(red), None)),
      Seq((0, -10.5146), (-10, -3.2492), (-6.1803, 8.5065), (6.1803, 8.5065), (10, -3.2492))
    )
    // Without a center, as every other factory has it, the box is centered at the origin.
    assertEquals[Element, Element](
      Pentagon(20, 20, fillWith(red), None),
      Pentagon(20, 20, Pos.Origin, fillWith(red), None)
    )
    assertPoints(
      aroundCenter(Pentagon(10, Pos.Origin, fillWith(red), None)),
      Seq((0, -10), (-9.5106, -3.0902), (-5.8779, 8.0902), (5.8779, 8.0902), (9.5106, -3.0902))
    )
    assertPoints(
      aroundCenter(Star5(10, 4, Pos.Origin, fillWith(red), None)),
      Seq(
        (0, -10),
        (-2.3511, -3.2361),
        (-9.5106, -3.0902),
        (-3.8042, 1.2361),
        (-5.8779, 8.0902),
        (0, 4),
        (5.8779, 8.0902),
        (3.8042, 1.2361),
        (9.5106, -3.0902),
        (2.3511, -3.2361)
      )
    )
  }

  test("the Triangle factories put the apex on top and the base below it") {
    assertPoints(
      pointsOf(Triangle(10, Pos.Origin, fillWith(red), None)),
      Seq((0, -4.3301), (5, 4.3301), (-5, 4.3301))
    )
    assertPoints(
      pointsOf(Triangle(8, 10, Pos.Origin, fillWith(red), None)),
      Seq((0, -3.1225), (5, 3.1225), (-5, 3.1225))
    )
    // In a 6-8-10 triangle the left side rises from the base's left end to an apex 3.6 along
    // the base and 4.8 above it.
    assertPoints(
      pointsOf(Triangle(10, 6, 8, Pos.Origin, fillWith(red), None)),
      Seq((-1.4, -2.4), (-5, 2.4), (5, 2.4))
    )
    // An obtuse angle at the base's left end leans the apex out past it.
    assertPoints(
      pointsOf(Triangle(10, 4, 13, Pos.Origin, fillWith(red), None)),
      Seq((-6.325, -1.4981), (-3.675, 1.4981), (6.325, 1.4981))
    )
  }

  test("Circle rejects a negative radius") {
    intercept[IllegalArgumentException]:
      Circle(Pos.Origin, -1, fillWith(red), line = None)
  }
