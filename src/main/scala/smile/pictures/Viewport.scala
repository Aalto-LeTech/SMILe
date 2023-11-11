package smile.pictures

import smile.modeling.{Bounds, Len}

class Viewport(val boundary: Bounds):
  def width: Len = boundary.width

  def height: Len = boundary.height
