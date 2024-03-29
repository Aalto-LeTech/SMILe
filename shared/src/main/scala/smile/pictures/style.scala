package smile.pictures

import smile.colors.Paint

def fill(paint: Paint): Option[FillStyle] = Some(FillStyle(paint))

def stroke(paint: Paint, width: Double, round: Boolean): Option[StrokeStyle] =
  Some(
    StrokeStyle(
      paint,
      width,
      if round then StrokeStyle.Cap.Round else StrokeStyle.Cap.Square,
      if round then StrokeStyle.Join.Round else StrokeStyle.Join.Miter
    )
  )

/** Represents the fill style for drawing operations.
  *
  * @param paint
  *   The paint (single color or gradient) used for the fill.
  */
case class FillStyle(paint: Paint)

/** Defines stroke styles for drawing outlines of shapes and paths, providing customization options
  * for the stroke's appearance.
  */
object StrokeStyle:
  /** Represents the cap style for the stroke, determining how the start and end of stroked lines
    * and paths are rendered.
    */
  enum Cap:
    /** The end of the stroke is flat. */
    case Butt

    /** The end of the stroke is rounded. */
    case Round

    /** The end of the stroke is squared off. */
    case Square

  /** Represents the join style for the stroke, determining how the connection between two line
    * segments is rendered.
    */
  enum Join:
    /** The outer edges of a join meet at a sharp point. */
    case Miter

    /** The join is rounded. */
    case Round

    /** The join is flattened to a line. */
    case Bevel

/** Encapsulates the style attributes for stroking operations, including color, line width, and line
  * cap and join styles.
  *
  * @param paint
  *   The paint (single color or gradient) used for the stroke.
  * @param width
  *   The width of the stroke. Defaults to 1.
  * @param cap
  *   The cap style of the stroke. Defaults to Square.
  * @param join
  *   The join style of the stroke. Defaults to Miter.
  */
case class StrokeStyle(
    paint: Paint,
    width: Double = 1,
    cap: StrokeStyle.Cap = StrokeStyle.Cap.Square,
    join: StrokeStyle.Join = StrokeStyle.Join.Miter,
    onTop: Boolean = true
)
