package smile.pictures

import org.scalajs.dom

import smile.colors.Color

class JSBitmapConversionsSuite extends munit.FunSuite:

  // Opaque colors only, because a canvas premultiplies.
  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  private def redBlue = Bitmap(2, 1, Seq(red, blue))

  private def context(canvas: dom.html.Canvas): dom.CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

  test("toCanvas holds the bitmap's pixels") {
    val canvas = redBlue.toCanvas()

    assertEquals((canvas.width, canvas.height), (2, 1))
    val pixel = context(canvas).getImageData(1, 0, 1, 1).data
    assertEquals((pixel(0), pixel(1), pixel(2), pixel(3)), (0, 0, 255, 255))
  }

  test("toCanvas is a copy that drawing on leaves the bitmap unchanged") {
    val bitmap = redBlue
    val canvas = bitmap.toCanvas()
    val ctx    = context(canvas)
    ctx.fillStyle = "rgb(0, 255, 0)"
    ctx.fillRect(0, 0, 2, 1)

    assertEquals(bitmap.getColor(0, 0), red)
    assertEquals(bitmap.pixels().colors, Vector(red, blue))
  }

  test("each toCanvas is a new canvas") {
    val bitmap = redBlue
    assert(bitmap.toCanvas() ne bitmap.toCanvas())
  }

  test("a translucent pixel comes back from toCanvas within 1 per channel") {
    val pixel =
      context(Bitmap.fromARGB(1, 1, Array(0x80123456)).toCanvas()).getImageData(0, 0, 1, 1).data
    val drawn = Seq(pixel(3), pixel(0), pixel(1), pixel(2))
    for (channel, expected) <- drawn.zip(Seq(0x80, 0x12, 0x34, 0x56)) do
      assert((channel - expected).abs <= 1, s"$drawn")
  }
end JSBitmapConversionsSuite
