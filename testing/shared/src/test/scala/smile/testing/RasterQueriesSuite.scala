package smile.testing

import smile.colors.PresetColor.{Red, White}
import smile.modeling.{Bounds, Pos}
import smile.pictures.*

class RasterQueriesSuite extends munit.FunSuite:

  /** A red circle of radius 15 centered at (30, 20) on a white 60 x 40 background. */
  private lazy val circle: Raster =
    val frame      = Bounds(Pos(0, 0), Pos(60, 40))
    val background = Rectangle(60, 40, Pos(30, 20), fillWith(White), None)
    val disc       = Circle(Pos(30, 20), 15, fillWith(Red), None)
    // A picture's first element is on top.
    Raster.of(Picture(disc, background).crop(frame))

  private def near(a: Double, b: Double, slack: Double) = (a - b).abs <= slack

  test("the circle's color is found around its center and not below it") {
    assert(circle.hasColorAround(Red, Pos(30, 20), 2))
    assert(!circle.hasColorAround(Red, Pos(30, 39), 2))
  }

  test("colorsAround gives the pixels whose centers are within the radius") {
    assertEquals(circle.colorsAround(Pos(30, 20), 1).size, 4)
    assert(circle.colorsAround(Pos(30, 20), 1).forall(_.toARGBInt == Red.toARGBInt))
  }

  test("a query outside the raster finds no colors") {
    assertEquals(circle.colorsAround(Pos(500, 500), 3), Seq.empty)
    assert(!circle.hasColorAround(Red, Pos(500, 500), 3))
  }

  test("boundsOf gives the box of the circle within a pixel") {
    val box = circle.boundsOf(Red).get
    assert(near(box.upperLeftCorner.x, 15, 1) && near(box.upperLeftCorner.y, 5, 1), box.toString)
    assert(near(box.lowerRightCorner.x, 45, 1) && near(box.lowerRightCorner.y, 35, 1), box.toString)
    assertEquals(circle.boundsOf(new smile.colors.Color(0xff0000ff)), None)
  }

  test("centroidOf gives the circle's center") {
    val center = circle.centroidOf(Red).get
    assert(near(center.x, 30, 0.5) && near(center.y, 20, 0.5), center.toString)
  }

  test("dominantColor of a box inside the circle is red") {
    assertEquals(circle.dominantColor(Bounds(Pos(25, 15), Pos(35, 25))).toARGBInt, Red.toARGBInt)
  }

  test("nearestOf finds the circle about five pixels above a point below it") {
    val nearest = circle.nearestOf(Red, Pos(30, 40)).get
    assert(near(nearest.x, 30.5, 1) && near(40 - nearest.y, 5.5, 1), nearest.toString)
  }

  /** One pixel of each color on a black background. */
  private def pixels(colors: Int*): Raster = Raster(colors.size, 1, colors.toArray)

  private val yellow = new smile.colors.Color(0xffffff00)
  private val black  = new smile.colors.Color(0xff000000)

  test("a pixel half way from the background to a color counts as a blend of it") {
    assert(pixels(0xff808000).hasBlendAround(yellow, black, Pos(0.5, 0.5), 0.5))
  }

  test("a different color or a gray does not count as a blend, however close") {
    assert(!pixels(0xffffa500).hasBlendAround(yellow, black, Pos(0.5, 0.5), 0.5)) // orange
    assert(!pixels(0xff808080).hasBlendAround(yellow, black, Pos(0.5, 0.5), 0.5)) // gray
  }

  test("a faint blend below the coverage asked for does not count") {
    assert(!pixels(0xff303000).hasBlendAround(yellow, black, Pos(0.5, 0.5), 0.5))
    assert(pixels(0xff303000).hasBlendAround(yellow, black, Pos(0.5, 0.5), 0.5, minCoverage = 0.1))
  }
