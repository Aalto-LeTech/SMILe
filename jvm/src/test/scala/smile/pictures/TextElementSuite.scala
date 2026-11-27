package smile.pictures

import smile.colors.Color
import smile.modeling.{Bounds, Pos}
import smile.modeling.TextShape
import smile.pictures.FillStyle

/** Text elements, which need the platform to measure them. */
class TextElementSuite extends munit.FunSuite:

  private val black = Some(FillStyle(Color(0, 0, 0)))

  private def text(content: String, box: Option[Bounds] = None, angle: Double = 0.0) =
    Text(Pos(100, 50), box, content, "Arial", 24, 400, black, None, angle)

  private def boundsOf(e: Element): Bounds = e.bounds.getOrElse(fail(s"$e has no bounds"))

  private def assertClose(actual: Double, expected: Double, clue: => Any = ""): Unit =
    assert((actual - expected).abs <= 1e-9 * expected.abs.max(1), s"$actual is not $expected $clue")

  test("a text has its natural size around its position") {
    val hello = text("Hello")
    val b     = boundsOf(hello)
    assert(b.width > 24 && b.height > 12, b)
    assert(b.upperLeftCorner.x < 100 && b.lowerRightCorner.x > 100, b)
    assert(b.upperLeftCorner.y < 50 && b.lowerRightCorner.y > 50, b)
  }

  test("a text's natural layout box is centered on its position") {
    text("Hello") match
      case hello: Shape =>
        hello.shape match
          case shape: TextShape =>
            val center = hello.affine(shape.layout.layoutBox.center)
            assertClose(center.x, 100)
            assertClose(center.y, 50)
          case other => fail(s"$other is not text")
      case other => fail(s"$other is not a shape")
  }

  test("a text with custom bounds fills them") {
    val box = Bounds(10, 20, 110, 60)
    val b   = boundsOf(text("Hello", Some(box)))
    assertClose(b.width, 100)
    assertClose(b.height, 40)
    assertClose(b.center.x, 60)
    assertClose(b.center.y, 40)
  }

  test("a text turned by a quarter turn swaps its width and height") {
    val (flat, turned) = (text("Hello"), text("Hello", angle = 90))
    assertClose(turned.width, flat.height)
    assertClose(turned.height, flat.width)
  }

  test("an empty text scales without failing and keeps its zero width") {
    val empty  = text("")
    val scaled = empty.scaleTo(5, 5)
    val b      = boundsOf(scaled)
    assert(Seq(b.upperLeftCorner, b.lowerRightCorner).forall(p => p.x.isFinite && p.y.isFinite))
    assert(scaled.affine.determinant != 0 && scaled.affine.determinant.isFinite)
    assertEquals(scaled.width, empty.width)
    assertClose(scaled.height, 5)
  }

  test("a text mirrors like any other shape, and equal texts are equal") {
    val hello = text("Hello")
    assertEquals(hello.flipHorizontally.flipHorizontally, hello)
    assertClose(hello.flipHorizontally.width, hello.width)
    assertEquals(text("Hello"), hello)
    assertEquals(text("Hello").hashCode, hello.hashCode)
    assertNotEquals(text("Hullo"), hello)
  }
end TextElementSuite
