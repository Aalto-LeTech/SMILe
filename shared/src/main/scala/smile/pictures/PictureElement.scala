package smile.pictures

/** Represents a basic element within a picture, capable of undergoing transformations such as
  * translation, scaling, and rotation. This trait is a foundational component for all graphical
  * elements that can be part of a picture.
  */
trait PictureElement extends Transformable[PictureElement]:

  /** Converts this `PictureElement` into a `Picture` by wrapping it within a new `Picture`
    * instance.
    *
    * @return
    *   A `Picture` containing only this `PictureElement`.
    */
  def toPicture: Picture = Picture(this)
