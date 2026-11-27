package smile.surface

/** The twin of `surface.JSPublicSurfaceSuite`, as `smile.surface.InternalSurfaceSuite` is of the
  * shared suite.
  */
class JSInternalSurfaceSuite extends munit.FunSuite:

  test("the Scala.js platform seam is reachable inside smile") {
    assertEquals(compileErrors("smile.infrastructure.JSBufferAdapter(1, 1)"), "")
    assertEquals(compileErrors("smile.infrastructure.WebCodecs.hasImageDecoder"), "")
    assertEquals(compileErrors("smile.infrastructure.JSResourceFactory"), "")
    assertEquals(compileErrors("smile.modeling.JSBoundaryCalculator"), "")
  }
end JSInternalSurfaceSuite
