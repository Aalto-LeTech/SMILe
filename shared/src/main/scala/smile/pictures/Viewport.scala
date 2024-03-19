package smile.pictures

import smile.modeling.{Bounds, Len}

/** Represents a viewport with a defined boundary. A viewport is a window or a region through which
  * a subset of the larger picture is viewed or displayed.
  *
  * @param boundary
  *   The boundary defining the limits of the viewport.
  */
class Viewport(val boundary: Bounds):

  def width: Len = boundary.width

  def height: Len = boundary.height
