package smile.pictures

import smile.RenderSettings
import smile.modeling.{Bounds, Pos}
import smile.testing.{Cases, Labeled}

/** Nothing a caller does with an element changes that element. Every operation hands back a new
  * one. That is checked by comparing each with an untouched twin afterwards, and for bitmaps, which
  * own a mutable buffer and compare by it, by comparing the pixels.
  */
class ImmutabilitySuite extends munit.FunSuite:

  private val operations: Seq[Labeled[Element => Any]] = Seq(
    Labeled("moveBy", _.moveBy(5, 5)),
    Labeled("moveTo", _.moveTo(Pos(50, 50))),
    Labeled("withAnchor", _.withAnchor(Pos(1, 2))),
    Labeled("rotateAround", _.rotateAround(33, Pos(7, -3))),
    Labeled("rotateBy", _.rotateBy(90)),
    Labeled("scaleBy", _.scaleBy(2, 0.5)),
    Labeled("scaleBy around a point", _.scaleAround(-1, 2, Pos(7, -3))),
    Labeled("scaleTo", _.scaleTo(30, 30)),
    Labeled("toBitmap", _.toBitmap()),
    Labeled("crop", element => element.crop(Bounds(-5, -5, 5, 5))),
    Labeled("wrapShift", _.wrapShift(3, 2)),
    Labeled("flipHorizontally", _.flipHorizontally),
    Labeled("addToRight", element => element.addToRight(element, 3)),
    Labeled("replicateDownwards", _.replicateDownwards(2)),
    Labeled("bounds", _.bounds)
  )

  test("no operation changes the element it is called on") {
    // Each fixture is built twice, and the twin is never touched.
    val touched = Cases.elements
    val twins   = Cases.elements
    for (element, twin) <- touched.zip(twins); operation <- operations do
      val _ = operation.value(element.value)
      element.value match
        case bitmap: Bitmap =>
          val untouched = twin.value.asInstanceOf[Bitmap]
          assertEquals(bitmap.bounds, untouched.bounds, s"${operation.label} on a bitmap")
          assertEquals(
            bitmap.pixels().colors,
            untouched.pixels().colors,
            s"${operation.label} on a bitmap"
          )
        case other => assertEquals(other, twin.value, s"${operation.label} on ${element.label}")
  }

  test("no operation changes the picture it is called on") {
    val picture  = Cases.picture
    val elements = picture.elements
    val boundary = picture.bounds.get

    val _ = picture.moveBy(5, 5)
    val _ = picture.rotateAround(33, Pos(7, -3))
    val _ = picture.scaleAround(2, 0.5, Pos(1, 1))
    val _ = picture.addToRight(picture)
    val _ = picture.crop(Bounds(Pos(0, 0), Pos(5, 5)))
    val _ = picture.wrapShift(2, 1)
    val _ = picture.withAnchor(Pos(1, 1))

    assertEquals(picture.elements, elements)
    assertEquals(picture.bounds.get, boundary)
    assertEquals(picture, Cases.picture)
  }

  test("a bitmap's pixel operations leave it alone and hand back their own pixels") {
    val bitmap = Cases.twoColorBitmap
    val before = bitmap.pixels().colors
    val bounds = bitmap.bounds.get

    val results = Seq[Labeled[Bitmap]](
      Labeled(
        "transformColorToColor",
        bitmap.transformColorToColor(_ => Cases.green, RenderSettings.Default)
      ),
      Labeled(
        "transformARGB",
        bitmap.transformARGB(_ => Cases.green.toARGBInt, RenderSettings.Default)
      ),
      Labeled("mergeWith", bitmap.mergeWith(bitmap, (_, _) => Cases.green, RenderSettings.Default)),
      Labeled(
        "mergeARGBWith",
        bitmap.mergeARGBWith(bitmap, (_, _) => Cases.green.toARGBInt, RenderSettings.Default)
      ),
      Labeled("setColorsByLocation", bitmap.setColorsByLocation((_, _) => Cases.green)),
      Labeled("crop", bitmap.crop(bitmap.bounds.get).asBitmap),
      Labeled("flipHorizontally", bitmap.flipHorizontally),
      Labeled("scaleBy(1, 1)", bitmap.scaleBy(1, 1).asBitmap)
    )
    // A turn by nothing, or by something and back, gives the bitmap itself back, whose pixels
    // nothing may paint into.
    assert(bitmap.rotateBy(0) eq bitmap)
    assertEquals(bitmap.rotateBy(1).rotateBy(-1), bitmap)
    assertEquals(bitmap.pixels().colors, before)
    assertEquals(bitmap.bounds.get, bounds)

    for result <- results do
      assertEquals(result.value.bounds.get, bounds, result.label)
      result.value.nativeBuffer().setRGBA(0, 0, Cases.green)
      assertEquals(bitmap.pixels().colors, before, s"${result.label} shares its pixels")
  }

  test("setColorsByLocation keeps the bitmap where it is") {
    // A result built from the pixels alone lands at the default position, wherever the original is.
    val bitmap  = Cases.twoColorBitmap
    val painted = bitmap.setColorsByLocation((x, y) => if x == y then Cases.green else Cases.blue)

    assertEquals(painted.bounds.get, bitmap.bounds.get)
    assertEquals(painted.anchor, bitmap.anchor)
    assertEquals(painted.getColor(2, 2), Cases.green)
    assertEquals(painted.getColor(3, 2), Cases.blue)
    assertEquals((painted.pixels().width, painted.pixels().height), (8, 4))
  }

  test("equal elements have equal hash codes and work as set members and map keys") {
    for shape <- Cases.vectorGraphics do
      // Moving away and back gives another instance with the same fields.
      val twin = shape.value.moveBy(3, 4).moveBy(-3, -4)
      assert(twin ne shape.value)
      assertEquals(twin, shape.value, shape.label)
      assertEquals(twin.hashCode, shape.value.hashCode, shape.label)
    assertEquals(Cases.vectorGraphics.map(_.value).toSet.size, Cases.vectorGraphics.size)

    val point = Point(Pos(7, -4), Cases.red)
    assertEquals(
      Map[Element, String](point -> "found")(Point(Pos(7, -4), Cases.red)),
      "found"
    )
    assertNotEquals(point: Element, Point(Pos(7, -4), Cases.blue): Element)
    assertEquals(Cases.maskGroup, Cases.maskGroup)
    assertEquals(Cases.maskGroup.hashCode, Cases.maskGroup.hashCode)
  }
