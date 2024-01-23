package smile.pictures

import smile.colors.Color

import java.awt.BasicStroke

/** Represents the fill style for drawing operations.
  *
  * @param color
  *   The color used for the fill.
  */
case class FillStyle(color: Color)

/** Defines stroke styles for drawing outlines of shapes and paths, providing customization options
  * for the stroke's appearance.
  */
object StrokeStyle:
  /** Represents the cap style for the stroke, determining how the start and end of stroked lines
    * and paths are rendered.
    */
  enum Cap(val value: Int):
    /** The end of the stroke is flat. */
    case Butt extends Cap(BasicStroke.CAP_BUTT)

    /** The end of the stroke is rounded. */
    case Round extends Cap(BasicStroke.CAP_ROUND)

    /** The end of the stroke is squared off. */
    case Square extends Cap(BasicStroke.CAP_SQUARE)

  /** Represents the join style for the stroke, determining how the connection between two line
    * segments is rendered.
    */
  enum Join(val value: Int):
    /** The outer edges of a join meet at a sharp point. */
    case Miter extends Join(BasicStroke.JOIN_MITER)

    /** The join is rounded. */
    case Round extends Join(BasicStroke.JOIN_ROUND)

    /** The join is flattened to a line. */
    case Bevel extends Join(BasicStroke.JOIN_BEVEL)

/** Encapsulates the style attributes for stroking operations, including color, line width, and line
  * cap and join styles.
  *
  * @param color
  *   The color of the stroke.
  * @param width
  *   The width of the stroke. Defaults to 1.
  * @param cap
  *   The cap style of the stroke. Defaults to Square.
  * @param join
  *   The join style of the stroke. Defaults to Miter.
  */
case class StrokeStyle(
    color: Color,
    width: Double = 1,
    cap: StrokeStyle.Cap = StrokeStyle.Cap.Square,
    join: StrokeStyle.Join = StrokeStyle.Join.Miter
):
  /** Converts this `StrokeStyle` into an AWT `BasicStroke` object.
    *
    * @return
    *   An instance of `BasicStroke` configured according to this `StrokeStyle`.
    */
  def toAWTStroke = new BasicStroke(width.toFloat, cap.value, join.value)
