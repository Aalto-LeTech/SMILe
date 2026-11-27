package surface

import smile.colors.Color
import smile.infrastructure.{Renderer, renderInto}
import smile.pictures.{Bitmap, Empty, toAWTImage, toSwingIcon}

import java.awt.image.BufferedImage

/** The JVM half of `surface.PublicSurfaceSuite`, for the names only the JVM has. Its twin is
  * `smile.surface.JVMInternalSurfaceSuite`.
  */
class JVMPublicSurfaceSuite extends munit.FunSuite:

  test("the JVM platform seam is hidden") {
    assert(compileErrors("smile.infrastructure.JVMBufferAdapter(1, 1)").nonEmpty)
    assert(compileErrors("smile.infrastructure.JVMRenderer").nonEmpty)
    assert(compileErrors("smile.infrastructure.JVMResourceFactory").nonEmpty)
    assert(compileErrors("smile.modeling.JVMBoundaryCalculator").nonEmpty)
  }

  test("a bitmap converts to an AWT image and a Swing icon from outside") {
    val image = Bitmap(1, 1, Seq(Color(1, 2, 3))).toAWTImage()
    assertEquals(image.getRGB(0, 0), Color(1, 2, 3).toARGBInt)
    assertEquals(Bitmap(1, 1, Seq(Color(1, 2, 3))).toSwingIcon().getIconWidth, 1)
  }

  test("a picture is drawn onto a caller's graphics from outside") {
    // The pixel's bounds run from (-0.5, -0.5), so its bitmap covers 2 by 2 pixels, as here.
    val image = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
    val g     = image.createGraphics()
    try
      Renderer().renderInto(g, Bitmap(1, 1, Seq(Color(1, 2, 3))), 1.0)
      Renderer().renderInto(g, Empty)
    finally g.dispose()
    assertEquals(image.getRGB(1, 1), Color(1, 2, 3).toARGBInt)
  }
end JVMPublicSurfaceSuite
