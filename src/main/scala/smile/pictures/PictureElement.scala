package smile.pictures

trait PictureElement extends Transformable[PictureElement]:

  def toPicture: Picture = Picture(this)
