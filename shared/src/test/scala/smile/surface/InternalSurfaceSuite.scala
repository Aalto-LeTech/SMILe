package smile.surface

/** The twin of `surface.PublicSurfaceSuite`. The snippets that fail to compile outside package
  * `smile` compile inside it, so access is what makes them fail there.
  */
class InternalSurfaceSuite extends munit.FunSuite:

  test("the shared platform seam is reachable inside smile") {
    assertEquals(compileErrors("smile.infrastructure.BufferAdapter(1, 1)"), "")
    assertEquals(compileErrors("smile.infrastructure.Constants.MaximumOpacity"), "")
    assertEquals(compileErrors("smile.infrastructure.ResourceFactory()"), "")
    assertEquals(compileErrors("smile.infrastructure.PlatformSpecific.renderer"), "")
    assertEquals(compileErrors("(null: smile.infrastructure.PlatformClassesTrait)"), "")
    assertEquals(compileErrors("(null: smile.infrastructure.NativeBuffer)"), "")
    assertEquals(compileErrors("(null: smile.infrastructure.ImageOperations)"), "")
    assertEquals(compileErrors("smile.modeling.NumberSupport.jvmText(1.0)"), "")
    assertEquals(
      compileErrors("smile.pictures.Bitmap(1, 1, Seq(smile.colors.Color(1, 2, 3))).nativeBuffer()"),
      ""
    )
  }

  test("the helpers behind public methods are reachable inside smile") {
    assertEquals(compileErrors("smile.ScalingMethod.Smooth.value"), "")
    assertEquals(compileErrors("smile.TransformMethod.Bilinear.value"), "")
    assertEquals(compileErrors("smile.pictures.SvgReading.MaxSide"), "")
    assertEquals(compileErrors("""smile.pictures.SvgDocument.fromString("<svg/>")"""), "")
    assertEquals(
      compileErrors(
        "smile.colors.LinearGradient(smile.modeling.Pos(0, 0), smile.modeling.Pos(1, 0), smile.colors.Color(1, 2, 3), smile.colors.Color(4, 5, 6)).fractions"
      ),
      ""
    )
    assertEquals(
      compileErrors(
        "smile.colors.LinearGradient(smile.modeling.Pos(0, 0), smile.modeling.Pos(1, 0), smile.colors.Color(1, 2, 3), smile.colors.Color(4, 5, 6)).colors"
      ),
      ""
    )
  }
end InternalSurfaceSuite
