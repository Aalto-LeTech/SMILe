package smile.pictures

import smile.modeling.Pos

/** Factory object for creating rectangles.
  */
object Rectangle:

  /** Creates a square with a given side length, fill style, and stroke style.
    *
    * @param sideLength
    *   The length of each side of the square.
    * @param fillStyle
    *   Optional fill style for the square.
    * @param strokeStyle
    *   Optional stroke style for the square.
    * @return
    *   A `VectorGraphic` representing the square.
    */
  def apply(
      sideLength: Double,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): VectorGraphic =
    apply(sideLength, Pos.Origin, fillStyle, strokeStyle)

  /** Creates a square with a given side length, center position, fill style, and stroke style.
    *
    * @param sideLength
    *   The length of each side of the square.
    * @param center
    *   The center position of the square.
    * @param fillStyle
    *   Optional fill style for the square.
    * @param strokeStyle
    *   Optional stroke style for the square.
    * @return
    *   A `VectorGraphic` representing the square.
    * @throws IllegalArgumentException
    *   If the side length is negative.
    */
  def apply(
      sideLength: Double,
      center: Pos,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): VectorGraphic =
    if sideLength < 0.0 then
      throw new IllegalArgumentException(
        s"Square's side length cannot be negative (was: $sideLength)."
      )

    apply(sideLength, sideLength, center, fillStyle, strokeStyle)

  /** Creates a rectangle with a given base length, height, fill style, and stroke style.
    *
    * @param baseLength
    *   The length of the base of the rectangle.
    * @param height
    *   The height of the rectangle.
    * @param fillStyle
    *   Optional fill style for the rectangle.
    * @param strokeStyle
    *   Optional stroke style for the rectangle.
    * @return
    *   A `VectorGraphic` representing the rectangle.
    */
  def apply(
      baseLength: Double,
      height: Double,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): VectorGraphic =
    apply(baseLength, height, Pos.Origin, fillStyle, strokeStyle)

  /** Creates a rectangle with a given base length, height, center position, fill style, and stroke
    * style.
    *
    * @param baseLength
    *   The length of the base of the rectangle.
    * @param height
    *   The height of the rectangle.
    * @param center
    *   The center position of the rectangle.
    * @param fillStyle
    *   Optional fill style for the rectangle.
    * @param strokeStyle
    *   Optional stroke style for the rectangle.
    * @return
    *   A `VectorGraphic` representing the rectangle.
    * @throws IllegalArgumentException
    *   If the base length or height is negative.
    */
  def apply(
      baseLength: Double,
      height: Double,
      center: Pos,
      fillStyle: Option[FillStyle],
      strokeStyle: Option[StrokeStyle]
  ): VectorGraphic =
    if baseLength < 0.0 then
      throw new IllegalArgumentException(
        s"Rectangle's base length cannot be negative (was: $baseLength)."
      )
    if height < 0.0 then
      throw new IllegalArgumentException(s"Rectangle's height cannot be negative (was: $height).")

    val cornerPoints =
      val halfWidth  = baseLength / 2.0
      val halfHeight = height / 2.0

      Seq(
        Pos(-halfWidth, -halfHeight),
        Pos(halfWidth, -halfHeight),
        Pos(halfWidth, halfHeight),
        Pos(-halfWidth, halfHeight)
      )

    Polygon(
      center,
      cornerPoints,
      fillStyle,
      strokeStyle
    )
