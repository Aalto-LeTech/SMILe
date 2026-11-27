package smile.colors

import smile.infrastructure.DrawPlan
import smile.modeling.{Pos, TextShape}
import smile.pictures.*
import smile.testing.{Cases, InkAssertions, Labeled}

/** `LinearGradient` promises that a gradient stays where it was on its shape: "a rotated or scaled
  * element rotates or stretches its gradient with it, rigidly". A gradient lives in its shape's
  * local frame, which every transform only places differently, so the law is that a gradient's two
  * points, taken to world coordinates, go through a transformation exactly like any other point of
  * the shape, and that the paint itself never changes. It is checked for every element that carries
  * a gradient, with a gradient that lies diagonally and off the shape's middle.
  */
class GradientRigiditySuite extends munit.FunSuite with InkAssertions:

  private val Exact = 1e-6

  private def shapeOf(element: Element): Shape = element match
    case shape: Shape => shape
    case other        => fail(s"$other is not a shape")

  private def gradientOf(element: Element): LinearGradient =
    shapeOf(element).fill.map(_.paint).collect { case gradient: LinearGradient => gradient }.get

  /** Where a shape's gradient starts and ends in world coordinates. Its points are measured from
    * the corner of the shape's local box, or of a text's layout box, less the stroke's radius, as
    * the renderers measure them, and placed by the shape's affine.
    */
  private def worldGradient(element: Element): (Pos, Pos) =
    val shape    = shapeOf(element)
    val gradient = gradientOf(element)
    val box      = shape.shape match
      case text: TextShape => text.layout.layoutBox
      case other           => other.localBox
    val origin = DrawPlan.paintOrigin(box, shape.stroke)
    (shape.affine(origin + gradient.start), shape.affine(origin + gradient.end))

  private val gradientShapes: Seq[Labeled[Element]] =
    Cases.vectorGraphics.filter(_.label.startsWith("gradient-filled"))

  private def assertCarried(
      shape: Labeled[Element],
      transformed: Element,
      onPoint: Pos => Pos,
      clue: String
  ): Unit =
    val (start, end)       = worldGradient(shape.value)
    val (newStart, newEnd) = worldGradient(transformed)
    assertClose(newStart, onPoint(start), Exact, s"${shape.label} $clue: the gradient's start")
    assertClose(newEnd, onPoint(end), Exact, s"${shape.label} $clue: the gradient's end")
    assertEquals(gradientOf(transformed), gradientOf(shape.value), s"${shape.label} $clue")

  test("a gradient moves with its element") {
    for shape <- gradientShapes do
      assertCarried(shape, shape.value.moveBy(31, -17), _ + (31.0, -17.0), "moved")
  }

  test("a gradient turns with its element, around any pivot, by any angle") {
    for shape <- gradientShapes; angle <- Cases.angles ++ Cases.seededAngles; pivot <- Cases.pivots
    do
      assertCarried(
        shape,
        shape.value.rotateAround(angle, pivot),
        _.rotateAround(angle, pivot),
        s"turned by $angle around $pivot"
      )
  }

  test("a gradient stretches with its element, turned or not") {
    for shape <- gradientShapes; pivot <- Cases.pivots; (h, v) <- Cases.positiveFactorPairs do
      assertCarried(
        shape,
        shape.value.scaleAround(h, v, pivot),
        _.scaleBy(h, v, pivot),
        s"scaled by ($h, $v) around $pivot"
      )
      assertCarried(
        shape,
        shape.value.scaleBy(h, v),
        _.scaleBy(h, v, shape.value.anchor),
        s"scaled by ($h, $v) around itself"
      )
  }

  test("a gradient mirrors with the element it fills, a text's included") {
    for shape <- gradientShapes; (h, v) <- Cases.mirroringFactorPairs; pivot <- Cases.pivots do
      assertCarried(
        shape,
        shape.value.scaleAround(h, v, pivot),
        _.scaleBy(h, v, pivot),
        s"mirrored by ($h, $v) around $pivot"
      )
  }

  test("transformations of a gradient-filled shape compose like those of its points") {
    val pivot = Pos(7, -3)
    for shape <- gradientShapes do
      val transformed = shape.value
        .scaleAround(2, 0.5, pivot)
        .rotateAround(40, pivot)
        .moveBy(5, 5)
        .rotateAround(-110, Pos.Origin)
      assertCarried(
        shape,
        transformed,
        point =>
          (point.scaleBy(2, 0.5, pivot).rotateAround(40, pivot) + (5.0, 5.0))
            .rotateAroundOrigin(-110),
        "scaled, turned, moved, and turned again"
      )
  }

  test("the colors a gradient paints travel with the shape") {
    // The numeric law above, seen in pixels. A point of the shape shows the same side of the
    // gradient before and after, on a lopsided shape that a mirrored gradient would betray.
    val gradient = LinearGradient(Pos(2, 0), Pos(28, 6), Cases.red, Cases.blue)
    val triangle = Triangle(30, 18, 26, Pos(5, -3), fillWith(gradient), None)
    val redSpot  = boundsOf(triangle).upperLeftCorner + (5.0, 13.0)
    val blueSpot = boundsOf(triangle).upperLeftCorner + (22.0, 13.0)
    val pivot    = Pos(7, -3)

    def colorAt(element: Element, spot: Pos): Color =
      onInkCanvas(element).getColor(
        (spot.x - inkCanvas.upperLeftCorner.x).floor.toInt,
        (spot.y - inkCanvas.upperLeftCorner.y).floor.toInt
      )
    def isRed(color: Color)  = color.opacity > 0 && color.red > color.blue + 60
    def isBlue(color: Color) = color.opacity > 0 && color.blue > color.red + 60

    val views = Seq[Labeled[(Element, Pos => Pos)]](
      Labeled("as built", (triangle, identity)),
      Labeled("turned by 70", (triangle.rotateAround(70, pivot), _.rotateAround(70, pivot))),
      Labeled(
        "mirrored",
        (triangle.scaleAround(-1, 1, pivot), _.scaleBy(-1, 1, pivot))
      ),
      Labeled(
        "stretched",
        (triangle.scaleAround(2, 3, pivot), _.scaleBy(2, 3, pivot))
      )
    )
    for view <- views do
      val (shape, onPoint) = view.value
      assert(
        isRed(colorAt(shape, onPoint(redSpot))),
        s"${view.label}: ${colorAt(shape, onPoint(redSpot))}"
      )
      assert(
        isBlue(colorAt(shape, onPoint(blueSpot))),
        s"${view.label}: ${colorAt(shape, onPoint(blueSpot))}"
      )
  }
end GradientRigiditySuite
