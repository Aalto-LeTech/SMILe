package smile.pictures

import smile.Settings.DefaultPositionType
import smile.colors.Color
import smile.modeling.{Bounds, Len, Pos, PositionType}

trait PictureElement extends Transformable[PictureElement]:
  def toPicture: Picture = Picture(this)
