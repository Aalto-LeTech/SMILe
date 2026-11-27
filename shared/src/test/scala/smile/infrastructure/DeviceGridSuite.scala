package smile.infrastructure

import smile.modeling.{Affine, Bounds, Pos}

class DeviceGridSuite extends munit.FunSuite:

  private def isWhole(v: Double): Boolean = math.abs(v - math.rint(v)) < 1e-9

  test("snap lands on a whole device pixel at scale 1.5") {
    val grid = DeviceGrid(1.5)
    for step <- 0 to 200 do
      val x = step * 0.25
      assert(isWhole(grid.snap(x) * 1.5), s"x = $x")
  }

  test("covering rounds the far edge outward at a fractional scale") {
    val box = DeviceGrid(1.5).covering(Bounds(Pos(0, 0), Pos(5, 5)))
    assertEquals(box, DeviceBox(0, 0, 8, 8))
  }

  test("covering is exact for whole bounds at scale 1") {
    val box = DeviceGrid(1).covering(Bounds(Pos(2, 3), Pos(10, 7)))
    assertEquals(box, DeviceBox(2, 3, 8, 4))
  }

  test("covering floors the minima and ceils the maxima in device pixels") {
    val box = DeviceGrid(2).covering(Bounds(Pos(1.2, 0.3), Pos(3.1, 2.6)))
    assertEquals(box, DeviceBox(2, 0, 5, 6))
  }

  test("tiny float errors do not push an exact edge outward") {
    // 0.1 * 3 is 0.30000000000000004, but the edge is meant to be device pixel 0.3 * 10 = 3.
    assertEquals(DeviceGrid(3).covering(Bounds(Pos(0, 0), Pos(0.1, 0.1))), DeviceBox(0, 0, 1, 1))
    assertEquals(DeviceGrid(10).covering(Bounds(Pos(0, 0), Pos(0.3, 0.3))), DeviceBox(0, 0, 3, 3))
  }

  test("an edge off by an affine's translation grid is still on its whole device pixel") {
    // A text fitted to a 30-pixel box by scaleTo and a move came out at -15.0000000611, because a
    // translation is kept on a grid of 2^-20 pixels, and covered one row of device pixels too many.
    val edge = -15.0000000611
    for scale <- Seq(1.0, 1.5, 2.0, 4.0) do
      assertEquals(
        DeviceGrid(scale).covering(Bounds(Pos(-40, edge), Pos(40, -edge))),
        DeviceGrid(scale).covering(Bounds(Pos(-40, -15), Pos(40, 15))),
        s"scale = $scale"
      )
  }

  test("a 7-pixel sprite centered at x = 10 snaps to a whole device pixel") {
    for scale <- Seq(1.0, 1.5, 2.0) do
      assert(isWhole(DeviceGrid(scale).snap(10 - 3.5) * scale), s"scale = $scale")
  }

  test("toDevice scales both axes") {
    assertEquals(DeviceGrid(1.5).toDevice, Affine.scaling(1.5, 1.5))
    assertEquals(DeviceGrid(1).toDevice, Affine.Identity)
  }

  test("the scale must be positive and finite") {
    val _ = intercept[IllegalArgumentException](DeviceGrid(0))
    val _ = intercept[IllegalArgumentException](DeviceGrid(-1))
    val _ = intercept[IllegalArgumentException](DeviceGrid(Double.NaN))
    intercept[IllegalArgumentException](DeviceGrid(Double.PositiveInfinity))
  }
