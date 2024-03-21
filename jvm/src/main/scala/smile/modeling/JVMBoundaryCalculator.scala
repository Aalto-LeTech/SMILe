package smile.modeling

import smile.infrastructure.DrawingSurface
import smile.pictures.Text

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform

/** Provides utility methods for calculating boundaries around picture elements and positions.
  */
object JVMBoundaryCalculator:
  /** Calculates the bounding box for a given text object.
    *
    * @param text
    *   The `Text` object for which to calculate the bounding box.
    * @return
    *   The `Bounds` representing the bounding box of the text.
    */
  def fromText(text: Text): Bounds =
    val frc = new FontRenderContext(new AffineTransform(), false, false)

    val font = DrawingSurface.fontToAWT(text.typeface, text.size)

    val lineMetrics = font.getLineMetrics(text.content, frc)
    val height      = lineMetrics.getHeight
    val width       = font.getStringBounds(text.content, frc).getWidth

    val upperLeft: Pos  = text.position.moveBy(-width / 2.0, -height / 2.0)
    val lowerRight: Pos = text.position.moveBy(width / 2.0, height / 2.0)

    Bounds(upperLeft, lowerRight)
