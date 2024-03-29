package smile.pictures

import smile.modeling.Pos

/** A trait for vector graphics, which are picture elements defined in terms of points, lines, and
  * curves. Vector graphics can be filled and/or stroked with specific styles.
  */
trait VectorGraphic extends DrawableElement:
  override def copy(newPosition: Pos): VectorGraphic
  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): VectorGraphic
  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): VectorGraphic
  override def rotateBy(angle: Double, centerOfRotation: Pos): VectorGraphic
  override def rotateByAroundOrigin(angle: Double): VectorGraphic

  /** The style used to fill the vector graphic. If `None`, the graphic is not filled. */
  val fillStyle: Option[FillStyle] = None

  /** The style used to stroke the outline of the vector graphic. If `None`, the graphic is not
    * stroked.
    */
  val strokeStyle: Option[StrokeStyle] = None // TODO: scale stroke width?
