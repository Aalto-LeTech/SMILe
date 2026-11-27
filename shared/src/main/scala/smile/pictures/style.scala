package smile.pictures

import smile.colors.Paint

/** A fill style for the given paint, ready to pass as a shape's `fill` argument.
  *
  * Named `fillWith` so that it stays reachable inside any scope that has a parameter or field
  * called `fill`, as every shape factory does.
  *
  * @param paint
  *   A single color or a gradient to fill with.
  */
def fillWith(paint: Paint): Option[FillStyle] = Some(FillStyle(paint))

/** A stroke style for the given paint, ready to pass as a shape's `line` argument.
  *
  * Named `lineWith` for the same reason as `fillWith`. It names the parameter it produces a value
  * for, and it does not shadow anything inside a shape's own scope.
  *
  * @param paint
  *   A single color or a gradient to line with.
  * @param width
  *   The width of the line in pixels.
  * @param round
  *   Whether the line's ends and joins are rounded. Otherwise the ends are squared off and the
  *   joins mitered.
  */
def lineWith(paint: Paint, width: Double, round: Boolean): Option[StrokeStyle] =
  Some(
    StrokeStyle(
      paint,
      width,
      if round then StrokeStyle.Cap.Round else StrokeStyle.Cap.Square,
      if round then StrokeStyle.Join.Round else StrokeStyle.Join.Miter
    )
  )

/** How a shape is filled.
  *
  * @param paint
  *   The single color or gradient used for the fill.
  */
case class FillStyle(paint: Paint) derives CanEqual:
  /** This style with its paint's colors replaced through `table`, which maps packed ARGB colors.
    */
  private[smile] def mapPaints(table: Int => Int): FillStyle = copy(paint = paint.mapPaints(table))

/** The cap and join styles of a stroke, and the miter limit that every backend uses. */
object StrokeStyle:
  /** How long a miter join may get before it is cut off into a bevel, as a multiple of the stroke's
    * width. Java2D, the HTML canvas, and SVG all measure the miter limit this way. A miter's length
    * is the stroke's width divided by the sine of half the corner's angle, so the limit of 10
    * bevels corners sharper than about 11.5 degrees. Every backend is set to this one value, and
    * the boundaries count on it. SVG's own default is 4.
    */
  val MiterLimit: Double = 10.0

  /** How the start and end of stroked lines and paths are drawn. */
  enum Cap derives CanEqual:
    /** The end of the stroke is flat. */
    case Butt

    /** The end of the stroke is rounded. */
    case Round

    /** The end of the stroke is squared off. */
    case Square

  /** How the connection between two line segments of a stroke is drawn.
    *
    * The join applies to shapes. The outlines of a [[smile.pictures.Text]]'s glyphs are always
    * stroked with round joins, whatever its style says, so that the text's bounds can reserve one
    * stroke radius around its glyphs.
    */
  enum Join derives CanEqual:
    /** The outer edges of a join meet at a sharp point. */
    case Miter

    /** The join is rounded. */
    case Round

    /** The join is flattened to a line. */
    case Bevel

/** How an outline is stroked, given by its paint, its line width, and its cap and join styles.
  *
  * ```scala
  * import smile.colors.PresetColor.*
  *
  * val thick    = StrokeStyle(Navy, 12, StrokeStyle.Cap.Round, StrokeStyle.Join.Round)
  * val outlined = Triangle(80, fillWith(Gold), Some(thick))
  * ```
  *
  * ![A golden triangle with a thick navy outline and round corners](pictures/outlined.png)
  *
  * @param paint
  *   The single color or gradient used for the stroke.
  * @param width
  *   The width of the stroke, finite and not negative. Defaults to 1.
  * @param cap
  *   The cap style of the stroke. Defaults to Square.
  * @param join
  *   The join style of the stroke. Defaults to Miter. A text's glyphs are stroked with round joins
  *   regardless. A stroke lives in its shape's own frame and scales with the shape, by each axis's
  *   own factor.
  * @throws IllegalArgumentException
  *   if the width is negative or not finite
  */
case class StrokeStyle(
    paint: Paint,
    width: Double = 1,
    cap: StrokeStyle.Cap = StrokeStyle.Cap.Square,
    join: StrokeStyle.Join = StrokeStyle.Join.Miter,
    onTop: Boolean = true
) derives CanEqual:
  require(width >= 0 && width.isFinite, s"A stroke's width must be finite and not negative: $width")

  /** This style with round joins, as every backend strokes the outlines of glyphs with it.
    *
    * A shape's bounds follow its miter tips corner by corner, but a text's cannot. The glyphs'
    * outlines belong to the font and the platform, and the model never sees their corners. Letters
    * are full of sharp ones, such as the feet of an A or the valleys of an M and a W, and a miter
    * on each would send a spike radii out of the text's bounds, where the renderers cut it off. A
    * round join reaches exactly one stroke radius from the outline, in every direction, and that is
    * what a text's bounds reserve around its glyphs.
    */
  private[smile] def forGlyphs: StrokeStyle = copy(join = StrokeStyle.Join.Round)

  /** This style with its paint's colors replaced through `table`, which maps packed ARGB colors.
    */
  private[smile] def mapPaints(table: Int => Int): StrokeStyle =
    copy(paint = paint.mapPaints(table))
