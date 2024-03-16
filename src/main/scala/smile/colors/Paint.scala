package smile.colors

import java.awt

/** A trait representing a paint pattern. Paints can be used as the fill or stroke of a shape.
  */
trait Paint:
  /** @return
    *   An AWT `Paint` object corresponding to this paint.
    */
  lazy val toAWTPaint: awt.Paint

  /** @return
    *   A `Color` object representing the average color of the paint pattern.
    */
  lazy val averageColor: Color
