package smile.pictures
import smile.modeling.{Bounds, Pos}

/** Masks a picture with a mask. Black parts of the mask will be transparent and white parts will be
  * opaque.
  *
  * @param mask
  *   The mask to use
  * @param masked
  *   The picture to mask
  */
class MaskGroup(val mask: DrawableElement, val masked: Picture) extends PictureElement:
  override lazy val boundary: Bounds = mask.boundary

  override def moveBy(xOffset: Double, yOffset: Double): MaskGroup =
    internalCopy(
      mask.moveBy(xOffset, yOffset).asInstanceOf[DrawableElement],
      masked.moveBy(xOffset, yOffset)
    )

  override def copy(newPosition: Pos): MaskGroup =
    internalCopy(
      mask.copy(newPosition),
      masked.copy(newPosition)
    )

  private def internalCopy(
      newMask: DrawableElement = mask,
      newMasked: Picture = masked
  ) =
    MaskGroup(newMask, newMasked)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): MaskGroup =
    internalCopy(newMask = mask.scaleBy(horizontalFactor, verticalFactor))

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): MaskGroup =
    internalCopy(newMask = mask.scaleBy(horizontalFactor, verticalFactor, relativityPoint))

  override def rotateBy(angle: Double, centerOfRotation: Pos): MaskGroup =
    internalCopy(newMask = mask.rotateBy(angle, centerOfRotation))

  override def rotateByAroundOrigin(angle: Double): MaskGroup =
    internalCopy(newMask = mask.rotateByAroundOrigin(angle))
