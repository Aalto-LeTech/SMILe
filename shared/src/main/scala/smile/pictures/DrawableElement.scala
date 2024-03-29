package smile.pictures

import smile.modeling.Pos

trait DrawableElement extends PictureElement:
  override def copy(newPosition: Pos): DrawableElement
  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): DrawableElement
  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): DrawableElement
  override def rotateBy(angle: Double, centerOfRotation: Pos): DrawableElement
  override def rotateByAroundOrigin(angle: Double): DrawableElement
