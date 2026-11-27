package surface

import org.scalajs.dom
import smile.colors.Color
import smile.infrastructure.JSRenderer
import smile.pictures.{Bitmap, Empty, toCanvas}

/** The Scala.js half of `surface.PublicSurfaceSuite`, for the names only Scala.js has. Its twin is
  * `smile.surface.JSInternalSurfaceSuite`.
  */
class JSPublicSurfaceSuite extends munit.FunSuite:

  test("the Scala.js platform seam is hidden") {
    assert(compileErrors("smile.infrastructure.JSBufferAdapter(1, 1)").nonEmpty)
    assert(compileErrors("smile.infrastructure.WebCodecs.hasImageDecoder").nonEmpty)
    assert(compileErrors("smile.infrastructure.JSResourceFactory").nonEmpty)
    assert(compileErrors("smile.modeling.JSBoundaryCalculator").nonEmpty)
  }

  test("SVGs and canvases are made from outside") {
    assert(JSRenderer.svgString(Empty).startsWith("<svg"))
    val canvas = Bitmap(1, 1, Seq(Color(1, 2, 3))).toCanvas()
    assertEquals((canvas.width, canvas.height), (1, 1))
  }

  test("a picture is drawn onto a caller's canvas from outside") {
    // The pixel's bounds run from (-0.5, -0.5), so its bitmap covers 2 by 2 pixels, as here.
    val canvas = Bitmap(2, 2, Seq.fill(4)(Color(1, 2, 3))).toCanvas()
    val ctx    = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    JSRenderer.renderInto(ctx, Bitmap(1, 1, Seq(Color(4, 5, 6))), 1.0)
    JSRenderer.renderInto(ctx, Empty)
    assertEquals(ctx.getImageData(1, 1, 1, 1).data.take(3).toSeq, Seq(4, 5, 6))
  }
end JSPublicSurfaceSuite
