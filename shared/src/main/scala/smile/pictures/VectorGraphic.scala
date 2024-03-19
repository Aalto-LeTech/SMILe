package smile.pictures

/** A trait for vector graphics, which are picture elements defined in terms of points, lines, and
  * curves. Vector graphics can be filled and/or stroked with specific styles.
  */
trait VectorGraphic extends PictureElement:

  /** The style used to fill the vector graphic. If `None`, the graphic is not filled. */
  val fillStyle: Option[FillStyle] = None

  /** The style used to stroke the outline of the vector graphic. If `None`, the graphic is not
    * stroked.
    */
  val strokeStyle: Option[StrokeStyle] = None // TODO: scale stroke width?
