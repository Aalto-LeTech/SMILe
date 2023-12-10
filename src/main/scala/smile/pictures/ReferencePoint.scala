package smile.pictures

import smile.colors.PresetColor
import smile.modeling.Pos

class ReferencePoint(pos: Pos, val name: String) extends Point(pos, PresetColor.Green):
  override def copy(newPosition: Pos): ReferencePoint = ReferencePoint(newPosition, name)
