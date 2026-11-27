package smile.pictures

import smile.colors.Color
import smile.modeling.*

/** The 2.0 element API. Elements compare structurally, over their geometry, style, normalized
  * affine and anchor, and every model type carries `CanEqual`, so that strict callers can compare
  * them and nothing compares them with unrelated types by accident.
  */
class ElementApiSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  test("structurally equal elements are equal, and different ones are not") {
    val square      = Rectangle(4, 2, Pos(1, 1), fillWith(red), line = None)
    val sameSquare  = Rectangle(4, 2, Pos(1, 1), fillWith(red), line = None)
    val otherSquare = Rectangle(4, 2, Pos(1, 1), fillWith(blue), line = None)

    assertEquals(square, sameSquare)
    assertEquals(square.hashCode, sameSquare.hashCode)
    assertNotEquals(square, otherSquare)

    assertEquals(Point(Pos(3, 4), red), Point(Pos(3, 4), red))
    assertNotEquals(Point(Pos(3, 4), red), Point(Pos(3, 5), red))

    assertEquals(
      Circle(Pos.Origin, 5, fillWith(red), line = None),
      Circle(Pos.Origin, 5, fillWith(red), line = None)
    )

    // Pictures too, by their parts, their affine and their anchor.
    assertEquals(Picture(square, otherSquare), Picture(sameSquare, otherSquare))
    assertNotEquals(Picture(square, otherSquare), Picture(otherSquare, square))
    // The anchor takes part. The same drawing turned around another point is another element.
    assertNotEquals(square.withAnchor(Pos(0, 0)), square)
  }

  test("moveTo sets an absolute anchor on every element type") {
    val target = Pos(-7, 13)
    for element <- smile.testing.Cases.elements do
      assertEquals(element.value.moveTo(target).anchor, target, element.label)
  }

  test("the model types refuse to be compared with unrelated types") {
    // The model types derive CanEqual, which makes the compiler reject an == comparison between
    // a model type and an unrelated reference type even without the -language:strictEquality flag.
    // Dropping a derivation would silently re-admit these comparisons, so pin a few here.
    def rejects(errors: String): Unit =
      assert(errors.contains("cannot be compared"), errors)

    rejects(compileErrors("Pos.Origin == Bounds(Pos.Origin, Pos.Origin)"))
    rejects(compileErrors("PositionType.Center == Side.Top"))
    rejects(compileErrors("Empty == Pos.Origin"))

    // Same-type comparisons keep working, which is what the derivations are for.
    assertEquals(Pos(1.0, 2.0), Pos(1.0, 2.0))
    assertEquals(Side.Top, Side.Top)
  }

  test("elements, bounds and paints carry CanEqual for strict callers") {
    // SMILe's own build compiles under strictEquality, and so may downstream code. Without a
    // derivation the universal fallback disappears and an == comparison stops compiling.
    // The compileErrors macro needs literal snippets, hence the repetition.
    def compiles(errors: String): Unit = assert(errors.isEmpty, errors)

    compiles(compileErrors("""{
      import scala.language.strictEquality
      Option(Bounds(Pos.Origin, Pos(1.0, 2.0))) == Option(Bounds(Pos.Origin, Pos(1.0, 2.0)))
    }"""))

    compiles(compileErrors("""{
      import scala.language.strictEquality
      val paint: smile.colors.Paint = Color(1, 2, 3)
      paint == paint
    }"""))

    compiles(compileErrors("""{
      import scala.language.strictEquality
      val element: Element = Empty
      element == Picture(Empty)
    }"""))
  }
end ElementApiSuite
