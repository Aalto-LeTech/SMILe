package smile.pictures

import smile.modeling.Pos

/** Factory object for creating lines.
  */
object Line:

  /** Creates a line between two specified points with a given stroke style.
    *
    * @param startX
    *   The x-coordinate of the start point of the line.
    * @param startY
    *   The y-coordinate of the start point of the line.
    * @param endX
    *   The x-coordinate of the end point of the line.
    * @param endY
    *   The y-coordinate of the end point of the line.
    * @param strokeStyle
    *   The style to be applied to the stroke of the line.
    * @return
    *   A `VectorGraphic` representing the line.
    */
  def apply(
      startX: Double,
      startY: Double,
      endX: Double,
      endY: Double,
      strokeStyle: StrokeStyle
  ): VectorGraphic =
    apply(Pos(startX, startY), Pos(endX, endY), strokeStyle)

  /** Creates a line between two specified positions with a given stroke style.
    *
    * @param start
    *   The start position of the line.
    * @param end
    *   The end position of the line.
    * @param strokeStyle
    *   The style to be applied to the stroke of the line.
    * @return
    *   A `VectorGraphic` representing the line.
    */
  def apply(start: Pos, end: Pos, strokeStyle: StrokeStyle): VectorGraphic =
    // Calculate the center point of the line to position it correctly
    val center = Pos((start.x + end.x) / 2.0, (start.y + end.y) / 2.0)

    // Adjust the start and end points relative to the center
    val adjustedStart = start - center
    val adjustedEnd   = end - center

    // Create a Polygon with only two points to represent the line
    new Polygon(
      center,
      Seq(adjustedStart, adjustedEnd),
      fillStyle = None, // Lines do not have fill
      strokeStyle = Some(strokeStyle)
    )
