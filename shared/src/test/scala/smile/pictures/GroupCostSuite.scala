package smile.pictures

import smile.colors.Color
import smile.modeling.Pos
import smile.modeling.Support
import smile.pictures.{FillStyle, StrokeStyle}

/** What reading a group's size costs. The cached boxes keep it proportional to what changed. */
class GroupCostSuite extends munit.FunSuite:

  test("reading a scene's width after moving one sprite evaluates fewer than two leaves") {
    val fill    = Some(FillStyle(Color(0, 128, 0)))
    val line    = Some(StrokeStyle(Color(0, 0, 0), 1))
    var sprites =
      Vector.tabulate(500)(i => Star5(16, 6, Pos(i % 25 * 20.0, i / 25 * 20.0), fill, line))
    assert(Picture(sprites*).width > 0)

    val reads  = 1000
    val before = Support.leafEvaluations
    for read <- 0 until reads do
      val i = read * 7 % sprites.size
      sprites = sprites.updated(i, sprites(i).moveBy(1, 0.5))
      assert(Picture(sprites*).width > 0)
    val perRead = (Support.leafEvaluations - before).toDouble / reads
    assert(perRead < 2, s"$perRead leaf evaluations per read")
  }

  test("turning a scene by a quarter turn maps its cached box without evaluating a leaf") {
    val sprites = Vector.tabulate(100)(i => Circle(Pos(i * 3.0, i * 2.0), 4, None, None))
    val scene   = Picture(sprites*)
    assert(scene.width > 0)
    val before = Support.leafEvaluations
    assert(scene.rotateBy(90).moveBy(5, 5).scaleBy(2).width > 0)
    assertEquals(Support.leafEvaluations - before, 0L)
  }
end GroupCostSuite
