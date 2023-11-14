package smile.pictures

import smile.colors.Color
import smile.modeling.{BoundaryCalculator, Bounds, Pos, TransformationMatrix, Transformer}

import java.awt.Font

sealed trait Alignment
case object LeftAlign   extends Alignment
case object RightAlign  extends Alignment
case object CenterAlign extends Alignment

class Text(
    pos: Pos,
    val content: String,
    val font: Font,
    val color: Color,
    val alignment: Alignment,
    override val transformationMatrix: TransformationMatrix = TransformationMatrix.identity
) extends VectorGraphic:

  override def copy(newPosition: Pos): Text = new Text(
    newPosition,
    content,
    font,
    color,
    alignment
  )

  override def copy(newMatrix: TransformationMatrix): PictureElement =
    new Text(position, content, font, color, alignment, newMatrix)

  override lazy val position: Pos = pos

  override lazy val boundary: Bounds = BoundaryCalculator.fromText(this)
