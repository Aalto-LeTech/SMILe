package smile.testing

import smile.modeling.{Bounds, Pos}
import smile.pictures.{Element, Text, fillWith}

/** Texts that can be measured and rasterized on every platform. */
object TextFixtures:

  /** A text stretched onto a 40 x 20 box around the origin. The custom bounds keep it measurable on
    * both platforms. jsdom lays out no SVG text, so an unboxed text cannot be measured there.
    */
  def boxedText(content: String, rotationAngle: Double): Element = Text(
    pos = Pos.Origin,
    customBounds = Some(Bounds(Pos(-20, -10), Pos(20, 10))),
    content = content,
    font = "SansSerif",
    size = 10,
    weight = 400,
    fill = fillWith(Cases.red),
    line = None,
    rotationAngle = rotationAngle
  )
