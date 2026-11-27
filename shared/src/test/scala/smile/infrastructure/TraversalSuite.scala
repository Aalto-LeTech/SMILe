package smile.infrastructure

import smile.RenderSettings
import smile.colors.Color
import smile.infrastructure.PlanListing.listed
import smile.pictures.{AnimatedPicture, DeepTrees, Element}

/** The fold walks a tree of any depth, even under Node, whose stack is about 1 MB. */
class TraversalSuite extends munit.FunSuite:

  // Deep enough that a walk that recurses once per level overflows a stack of 1 MB.
  private val Steps = 10000

  private val green = Color(0, 128, 0).toARGBInt

  for d <- DeepTrees.nested do
    test(s"${d.name}: paints and mapped paints at $Steps") {
      val built = d.build(Steps)
      assertEquals(Traversal.paints(built), Set(green))
      val mapped = Traversal.mapPaints(built, _ ^ 0x00ffffff)
      assertEquals(Traversal.paints(mapped), Set(green ^ 0x00ffffff))
    }

  test("an animation of deep frames plans one frame and paints all") {
    val animation = DeepTrees.nested.iterator
      .map(_.build(Steps))
      .collectFirst:
        case animation: AnimatedPicture => animation
      .getOrElse(fail("No fixture is an animation"))
    def plan(e: Element, time: Long): Vector[(DrawOp, Vector[Int])] =
      listed(DrawPlan.of(e, DeviceGrid(1), RenderSettings.Default, time)._2)
    val (first, second) = (0L, 100L)
    for (frame, time) <- animation.frames.zip(Seq(first, second)) do
      assertEquals(plan(animation, time), plan(frame, time))
    assertNotEquals(plan(animation, first), plan(animation, second))
    assertEquals(Traversal.paints(animation), animation.frames.map(Traversal.paints).reduce(_ ++ _))
  }
end TraversalSuite
