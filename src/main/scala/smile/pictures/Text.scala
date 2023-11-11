package smile.pictures

import smile.colors.Color
import smile.modeling.{BoundaryCalculator, Bounds, Pos, Transformer}

import java.awt.Font

sealed trait Alignment
case object LeftAlign   extends Alignment
case object RightAlign  extends Alignment
case object CenterAlign extends Alignment

class Text(
    pos: Pos,
    val content: String,
    val font: Font,
    val color: Color
//    val alignment: Alignment
) extends VectorGraphic:

  override def copy(newPosition: Pos): Text = new Text(
    newPosition,
    content,
    font,
    color
//    alignment
  )

  override lazy val boundary: Bounds = BoundaryCalculator.fromText(this)

  override lazy val position: Pos = pos

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Text =
    this

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Text =
    this

  override def rotateBy(angle: Double, centerOfRotation: Pos): Text =
    this

  override def rotateByAroundOrigo(angleInDegrees: Double): Text =
    this
