package smile.colors

/** A paint pattern, usable as the fill or stroke of a shape.
  */
trait Paint derives CanEqual:
  /** The color this paint amounts to over its whole pattern. A renderer that cannot paint the
    * pattern paints this color.
    */
  def averageColor: Color

  /** The packed ARGB colors this paint is made of. A paint the renderers do not know is painted as
    * its average color, so that is its one color.
    */
  private[smile] def paints: Set[Int] = Set(averageColor.toARGBInt)

  /** This paint with each of its colors replaced through `table`, which maps packed ARGB colors.
    */
  private[smile] def mapPaints(table: Int => Int): Paint = new Color(table(averageColor.toARGBInt))
