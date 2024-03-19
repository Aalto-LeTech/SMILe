package smile.colors

/** A trait representing a paint pattern. Paints can be used as the fill or stroke of a shape.
  */
trait Paint:
  /** @return
    *   A `Color` object representing the average color of the paint pattern.
    */
  lazy val averageColor: Color
