package surface

import smile.colors.Color
import smile.infrastructure.Renderer
import smile.pictures.{Bitmap, Empty}

/** Code outside package `smile` sees none of the platform-specific types but `Renderer` and, on
  * Scala.js, `JSRenderer`. Of the helpers behind public methods it sees nothing.
  *
  * Scala 3 lets a `private[smile]` type appear in a public signature without a warning, so the
  * compiler cannot be trusted to find a leak. These snippets pin the surface. Each one fails to
  * compile here. Its twin `smile.surface.InternalSurfaceSuite` compiles the same snippets from
  * inside `smile`, which shows that access is what makes them fail here.
  *
  * A snippet depends on nothing the incremental compiler tracks, so after a change of visibility
  * these suites keep their old verdict until they are compiled from clean.
  */
class PublicSurfaceSuite extends munit.FunSuite:

  test("the shared platform seam is hidden") {
    assert(compileErrors("smile.infrastructure.BufferAdapter(1, 1)").nonEmpty)
    assert(compileErrors("smile.infrastructure.Constants.MaximumOpacity").nonEmpty)
    assert(compileErrors("smile.infrastructure.ResourceFactory()").nonEmpty)
    assert(compileErrors("smile.infrastructure.PlatformSpecific.renderer").nonEmpty)
    assert(compileErrors("(null: smile.infrastructure.PlatformClassesTrait)").nonEmpty)
    assert(compileErrors("(null: smile.infrastructure.NativeBuffer)").nonEmpty)
    assert(compileErrors("(null: smile.infrastructure.ImageOperations)").nonEmpty)
    assert(compileErrors("smile.modeling.NumberSupport.jvmText(1.0)").nonEmpty)
    assert(
      compileErrors(
        "smile.pictures.Bitmap(1, 1, Seq(smile.colors.Color(1, 2, 3))).nativeBuffer()"
      ).nonEmpty
    )
  }

  test("the helpers behind public methods are hidden") {
    assert(compileErrors("smile.ScalingMethod.Smooth.value").nonEmpty)
    assert(compileErrors("smile.TransformMethod.Bilinear.value").nonEmpty)
    assert(compileErrors("smile.pictures.SvgReading.MaxSide").nonEmpty)
    assert(
      compileErrors(
        "smile.colors.LinearGradient(smile.modeling.Pos(0, 0), smile.modeling.Pos(1, 0), smile.colors.Color(1, 2, 3), smile.colors.Color(4, 5, 6)).fractions"
      ).nonEmpty
    )
    assert(
      compileErrors(
        "smile.colors.LinearGradient(smile.modeling.Pos(0, 0), smile.modeling.Pos(1, 0), smile.colors.Color(1, 2, 3), smile.colors.Color(4, 5, 6)).colors"
      ).nonEmpty
    )
  }

  test("SVG pictures load as bitmaps do, and their documents are hidden") {
    assertEquals(compileErrors("""smile.pictures.SvgPicture("p")"""), "")
    assertEquals(compileErrors("""smile.pictures.SvgPicture.load("p")"""), "")
    assertEquals(compileErrors("""smile.pictures.SvgPicture.fromString("<svg/>")"""), "")
    assert(compileErrors("""smile.pictures.SvgPicture.fromPath("p")""").nonEmpty)
    assert(compileErrors("smile.pictures.SvgLoading").nonEmpty)
    assert(compileErrors("""smile.pictures.SvgDocument.fromString("<svg/>")""").nonEmpty)
  }

  test("the public surface renders and reads pixels from outside") {
    val bitmap = Bitmap(1, 1, Seq(Color(1, 2, 3)))
    assertEquals(Renderer().createBitmapFrom(Empty).width, 0.0)
    assertEquals(bitmap.pixels().argbData.toSeq, Seq(Color(1, 2, 3).toARGBInt))
  }
end PublicSurfaceSuite
