// Design the logo in a fixed coordinate space, then fit it to the preview.
import smile.colors.*
import smile.colors.PresetColor.*
import smile.modeling.*
import smile.pictures.*
import scala.util.Random

val unit = 40.0
val width = 20 * unit
val height = 8 * unit
// Positions are fractions of the panel, measured from its upper-left corner.
def at(x: Double, y: Double): Pos = Pos((x - 0.5) * width, (y - 0.5) * height)
val white = White
val blue = Color(0, 113, 185)
val green = Color(42, 178, 97)
val coral = Color(241, 88, 88)

// A round-joined outline expands a smaller rectangle to the requested size.
def roundedRectangle(width: Double, height: Double, radius: Double, color: Color) =
  Rectangle(width - 2 * radius, height - 2 * radius,
    fillWith(color), lineWith(color, 2 * radius, true))

val gradient = LinearGradient(
  Pos(width * 0.02, height * 0.15), Pos(width * 0.98, height * 0.85),
  Color(182, 228, 250), Color(126, 132, 192)
)
val random = new Random(42)
val grainStrength = 40
// Independent color grain gives the original its fine, colorful texture.
val textured = Rectangle(width, height, fillWith(gradient), None).toBitmap()
  .transformARGB { pixel =>
    def channel(value: Int): Int =
      val grain = random.nextInt(2 * grainStrength + 1) - grainStrength
      (value + grain).max(0).min(255)
    val r = channel((pixel >>> 16) & 255)
    val g = channel((pixel >>> 8) & 255)
    val b = channel(pixel & 255)
    (pixel & 0xff000000) | (r << 16) | (g << 8) | b
  }
val background = MaskGroup(roundedRectangle(width, height, height / 4, white), textured)

// Puts the middle of an element's bounds at the point p, where a turned shape's anchor need not be.
def centeredAt(element: Element, p: Pos): Element =
  val middle = element.bounds.fold(p)(_.center)
  element.moveBy(p.x - middle.x, p.y - middle.y)

val squareEye = centeredAt(
  roundedRectangle(1.35 * unit, 1.35 * unit, unit / 5, blue).rotateAround(20, Pos.Origin),
  at(0.15, 0.38)
)
val triangleEye = centeredAt(
  Triangle(1.25 * unit, fillWith(green), lineWith(green, unit / 4, true))
    .rotateAround(45, Pos.Origin),
  at(0.265, 0.335)
)
val mouth = Arc(
  pos = at(0.21, 0.35), width = 5.5 * unit, height = 5.4 * unit,
  startAngle = -135, arcAngle = 125,
  fill = None, line = lineWith(coral, 0.9 * unit, true)
)
val artwork = background.addToFront(squareEye).addToFront(triangleEye).addToFront(mouth)

val word = Text(
  at(0.69, 0.51), None, "SMILe", "Hack", 3 * unit, 700, fillWith(white), None
)
canvas.fit(artwork.addToFront(word), width, height)
  .addToBack(canvas.backdrop)
