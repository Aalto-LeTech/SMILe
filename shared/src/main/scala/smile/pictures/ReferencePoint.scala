package smile.pictures

import smile.colors.PresetColor
import smile.modeling.Pos

/** Represents a named reference point in a graphical context. This class can be used to label
  * specific positions within a graphical representation, which can be used as, for example, pivot
  * points.
  *
  * @param pos
  *   The position of the reference point.
  * @param name
  *   The name associated with the reference point.
  */
class ReferencePoint(pos: Pos, val name: String) extends Point(pos, PresetColor.Transparent):
  /** Creates a copy of this reference point positioned at the specified coordinates.
    *
    * @param newPosition
    *   The new position for the copied reference point.
    * @return
    *   A new instance of `ReferencePoint` at the specified position with the same name.
    */
  override def copy(newPosition: Pos): ReferencePoint = ReferencePoint(newPosition, name)
