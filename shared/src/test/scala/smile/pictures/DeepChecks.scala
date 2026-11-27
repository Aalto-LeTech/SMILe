package smile.pictures

import smile.RenderSettings
import smile.infrastructure.Traversal

/** Every operation on a tree, for the suites that check that each works however deep the tree. */
object DeepChecks extends munit.Assertions:

  private val settings = RenderSettings.Default

  /** Checks every operation on `e`, built twice: equality and hashes, bounds turned or not, the
    * string, the pixels, the colors and a recolor, with `draw` drawing `e` and `e` turned through
    * the renderer of the platform.
    */
  def everything(e: => Element, draw: Element => Unit): Unit =
    val built = e
    val again = e
    assertEquals(built, again)
    assertEquals(built.hashCode, again.hashCode)
    assert(built.bounds.isDefined)
    assert(built.rotateBy(30).bounds.isDefined)
    assert(built.toString.nonEmpty)
    assert(built.toBitmap().pixels().width > 0)
    draw(built)
    draw(built.rotateBy(30))
    assert(Bitmap(built, settings).pixels().width > 0)
    assert(Recolor(built, argb => argb ^ 0x00ffffff, settings).width > 0)
    assert(Traversal.paints(built).nonEmpty)
end DeepChecks
