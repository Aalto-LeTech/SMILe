package smile.surface

/** The twin of `surface.JVMPublicSurfaceSuite`, as `smile.surface.InternalSurfaceSuite` is of the
  * shared suite.
  */
class JVMInternalSurfaceSuite extends munit.FunSuite:

  test("the JVM platform seam is reachable inside smile") {
    assertEquals(compileErrors("smile.infrastructure.JVMBufferAdapter(1, 1)"), "")
    assertEquals(compileErrors("smile.infrastructure.JVMRenderer"), "")
    assertEquals(compileErrors("smile.infrastructure.JVMResourceFactory"), "")
    assertEquals(compileErrors("smile.modeling.JVMBoundaryCalculator"), "")
  }
end JVMInternalSurfaceSuite
