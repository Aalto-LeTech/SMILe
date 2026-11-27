package smile.modeling

/** The one transform type: exact angles, normalized values, total inverses. */
class AffineSuite extends munit.FunSuite:

  private val pivot = Pos(300, 200)

  test("sine and cosine are exact at multiples of 90 after wrapping the angle") {
    assertEquals(Angles.exactSinCos(450), (1.0, 0.0))
    assertEquals(Angles.exactSinCos(-360), (0.0, 1.0))
    assertEquals(Angles.exactSinCos(720), (0.0, 1.0))
    assertEquals(Angles.exactSinCos(-90), (-1.0, 0.0))
    assertEquals(Angles.exactSinCos(180), (0.0, -1.0))
    val (sin, cos) = Angles.exactSinCos(30)
    assertEqualsDouble(sin, 0.5, 1e-15)
    assertEqualsDouble(cos, math.sqrt(3) / 2, 1e-15)
  }

  test("rotating around an anchor and back gives exactly the identity") {
    val turned = Affine.rotation(30, pivot)
    assertEquals(turned.andThen(Affine.rotation(-30, pivot)), Affine.Identity)
    assertEquals(turned.andThen(turned.inverse), Affine.Identity)
  }

  test("twelve turns of 30 degrees around a pivot make the identity") {
    // Each turn's translation is rounded to 2^-20, so the composition may end one grid step off
    // in rare cases. The pivot (300, 200) ends one step off in f. The linear part is exact.
    val grid = 1.0 / 1048576
    for p <- Seq(Pos.Origin, Pos(17.3, -42.9), Pos(1000, 1000), pivot) do
      val turn = Affine.rotation(30, p)
      val full = (1 until 12).foldLeft(turn)((sum, _) => sum.andThen(turn))
      assertEquals((full.a, full.b, full.c, full.d), (1.0, 0.0, 0.0, 1.0), s"pivot $p")
      assert(full.e.abs <= grid && full.f.abs <= grid, s"pivot $p: ${full.e}, ${full.f}")
    for p <- Seq(Pos.Origin, Pos(17.3, -42.9), Pos(1000, 1000)) do
      val turn = Affine.rotation(30, p)
      assertEquals((1 until 12).foldLeft(turn)((sum, _) => sum.andThen(turn)), Affine.Identity)
  }

  test("a quarter turn has exact coefficients and keeps the axes") {
    val quarter = Affine.rotation(90, Pos.Origin)
    assertEquals(quarter.b, -1.0) // (1, 0) goes up, to (0, -1), on screen
    assertEquals(quarter.c, 1.0)
    assertEquals(quarter(Pos(1, 0)), Pos(0, -1))
    assertEquals(quarter.a, 0.0)
    assertEquals(quarter.d, 0.0)
    assert(quarter.keepsAxes)
    assert(quarter.keepsPixelGrid)
    assert(!Affine.rotation(30, Pos.Origin).keepsAxes)
  }

  test("-0.0 is normalized, so equal affines have equal hashes") {
    val negative = Affine(1, -0.0, 0, 1, -0.0, 0)
    assertEquals(negative, Affine.Identity)
    assertEquals(negative.hashCode, Affine.Identity.hashCode)
  }

  test("translations round to a multiple of 2^-20 and linear values snap") {
    val moved = Affine(1 + 1e-14, 1e-14, 0, 1 - 1e-14, 0.1, 0)
    assertEquals(moved.a, 1.0)
    assertEquals(moved.b, 0.0)
    assertEquals(moved.d, 1.0)
    assertEquals(moved.e * 1048576.0, math.rint(moved.e * 1048576.0))
  }

  test("an affine that is not finite or not invertible is rejected") {
    val _ = intercept[IllegalArgumentException](Affine(Double.NaN, 0, 0, 1, 0, 0))
    val _ = intercept[IllegalArgumentException](Affine(1, 0, 0, 1, Double.PositiveInfinity, 0))
    val _ = intercept[IllegalArgumentException](Affine.scaling(0, 1))
    val _ = intercept[IllegalArgumentException](Affine(1, 2, 2, 4, 0, 0))
    val _ = intercept[IllegalArgumentException](Affine.rotation(Double.NaN, Pos.Origin))
  }

  test("an infinite coefficient is reported as not finite, not snapped to zero") {
    val error = intercept[IllegalArgumentException](Affine(Double.PositiveInfinity, 0, 0, 1, 0, 0))
    assert(error.getMessage.contains("finite"), error.getMessage)
    assert(!Affine.uncheckedScaling(Double.PositiveInfinity, 1).isFinite)
  }

  test("angles are wrapped before the sine and cosine are taken") {
    assertEquals(Affine.rotation(390, pivot), Affine.rotation(30, pivot))
    assertEquals(Affine.rotation(-330, pivot), Affine.rotation(30, pivot))
  }

  test("a very anisotropic scaling is accepted and invertible") {
    val map = Affine.scaling(1e12, 1)
    assertEquals((map.a, map.d), (1e12, 1.0))
    assertEqualsDouble(map.inverse.a, 1e-12, 1e-24)
  }

  test("the inverse undoes quarter turns and mirrors exactly") {
    val exact = for
      degrees <- Seq(0.0, 90.0, 180.0, 270.0, -90.0, 450.0)
      p       <- Seq(Pos.Origin, pivot, Pos(-12.5, 7.25))
    yield Affine.rotation(degrees, p)
    val mirrors = for
      h <- Seq(false, true)
      v <- Seq(false, true)
    yield Affine.mirroring(h, v, Pos(10.5, 20.25))
    for map <- exact ++ mirrors do
      assertEquals(map.andThen(map.inverse), Affine.Identity)
      assertEquals(map.inverse.andThen(map), Affine.Identity)
  }

  test("the inverse undoes random invertible affines within the tolerance") {
    val random = new scala.util.Random(7)
    def value  = random.nextDouble() * 4 - 2
    var made   = 0
    while made < 1000 do
      val (a, b, c, d) = (value, value, value, value)
      if (a * d - b * c).abs > 0.2 then
        made += 1
        val map                 = Affine(a, b, c, d, value * 100, value * 100)
        val product             = map.andThen(map.inverse)
        val Seq(pa, pb, pc, pd) = Seq(product.a, product.b, product.c, product.d)
        assertEqualsDouble(pa, 1.0, Affine.Tolerance)
        assertEqualsDouble(pb, 0.0, Affine.Tolerance)
        assertEqualsDouble(pc, 0.0, Affine.Tolerance)
        assertEqualsDouble(pd, 1.0, Affine.Tolerance)
        // The translations are rounded to 2^-20 twice, and one of them is scaled by the map.
        assertEqualsDouble(product.e, 0.0, 1e-5)
        assertEqualsDouble(product.f, 0.0, 1e-5)
  }

  test("the box around the unit square turned by 45 degrees is as wide as its diagonal") {
    val box = Affine.rotation(45, Pos.Origin).boxBounds(0, 0, 1, 1)
    assertEqualsDouble(box.width, math.sqrt(2), 1e-12)
    assertEqualsDouble(box.height, math.sqrt(2), 1e-12)
  }

  test("a direction turned by a right angle is exactly the direction there") {
    assertEquals(Direction.Right.counterclockwise(90), Direction.Up)
    assertEquals(Direction.Up.counterclockwise(90), Direction.Left)
    assertEquals(Direction.Right.clockwise(90), Direction.Down)
  }
end AffineSuite
