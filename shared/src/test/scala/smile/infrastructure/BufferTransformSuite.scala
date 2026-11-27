package smile.infrastructure

import smile.modeling.{Affine, Pos}
import smile.testing.weightedMiddle

/** A transformed buffer is the transformed corners' extent rounded down to whole pixels, with the
  * transformed pixels in its middle wherever the transformation takes them, on both platforms
  * alike.
  */
class BufferTransformSuite extends munit.FunSuite:

  /** A buffer of 30 by 12 pixels, each of its own opaque color. */
  private def patterned: NativeBuffer =
    val buffer = BufferAdapter(30, 12)
    buffer.setARGBByLocation((x, y) => 0xff000000 | (x * 8) << 16 | (y * 20) << 8 | 90)
    buffer

  test("a turn with any translation gives the pixels of the turn about the buffer's middle") {
    val buffer = patterned
    val middle = Pos(buffer.width / 2.0, buffer.height / 2.0)
    for angle <- Seq(30.0, 90.0, 137.0) do
      val expected = buffer.transform(Affine.rotation(angle, middle)).argbData.toSeq
      for turn <- Seq(
          Affine.rotation(angle, Pos.Origin),
          Affine.rotation(angle, middle).andThen(Affine.translation(50, 40)),
          Affine.rotation(angle, middle).andThen(Affine.translation(-50, 40))
        )
      do assertEquals(buffer.transform(turn).argbData.toSeq, expected, s"$turn")
  }

  test("a turned buffer is the turned corners' extent rounded down, on every platform") {
    val sizes = Seq(
      0.0   -> (200, 50),
      15.0  -> (206, 100),
      30.0  -> (198, 143),
      45.0  -> (176, 176),
      90.0  -> (50, 200),
      100.0 -> (83, 205),
      137.5 -> (181, 171),
      180.0 -> (200, 50),
      -37.0 -> (189, 160)
    )
    for (angle, size) <- sizes do
      val turn = Affine.rotation(angle, Pos(100, 25))
      assertEquals(ImageOperations.transformedSize(200, 50, turn), size, s"$angle")
      val turned = BufferAdapter(200, 50).transform(turn)
      assertEquals((turned.width, turned.height), size, s"$angle")
  }

  test("a turned buffer holds the turned pixels in its middle") {
    val buffer = BufferAdapter(200, 50)
    buffer.setARGBByLocation((_, _) => 0xff0000ff)
    for angle <- Seq(15.0, 30.0, 45.0, 100.0, 137.5, -37.0) do
      val turned = buffer.transform(Affine.rotation(angle, Pos(100, 25)))
      // Each pixel weighs its opacity.
      val (x, y) = weightedMiddle(turned, argb => (argb >>> 24) / 255.0)
      assertEqualsDouble(x, turned.width / 2.0, 0.02, s"$angle")
      assertEqualsDouble(y, turned.height / 2.0, 0.02, s"$angle")
  }

end BufferTransformSuite
