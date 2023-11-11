package smile.pictures

import smile.Settings.*
import smile.colors.Color
import smile.modeling.{Angle, Pos}

/** An object-based API for creating regular star (= concave) pentagons.
  *
  * @author
  *   Aleksi Lukkarinen
  * @author
  *   Jaakko Nakaza
  */
object StarPentagon:

  private def intersperse[ListItem](
      firstList: List[ListItem],
      secondList: List[ListItem]
  ): List[ListItem] = firstList match
    case first :: tail => first :: intersperse(secondList, tail)
    case _             => secondList

  /** @param widthInPixels
    * @param heightInPixels
    * @param cuspRadiusInPixels
    *
    * @return
    */
  def apply(
      widthInPixels: Double,
      heightInPixels: Double,
      cuspRadiusInPixels: Double
  ): VectorGraphic =
    apply(
      widthInPixels,
      heightInPixels,
      cuspRadiusInPixels,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor
    )

  /** @param widthInPixels
    * @param heightInPixels
    * @param cuspRadiusInPixels
    * @param hasBorder
    * @param hasFilling
    * @param color
    * @param fillColor
    *
    * @return
    */
  def apply(
      widthInPixels: Double,
      heightInPixels: Double,
      cuspRadiusInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =
    apply(
      widthInPixels,
      heightInPixels,
      cuspRadiusInPixels,
      Pos.Origo,
      hasBorder,
      hasFilling,
      color,
      fillColor
    )

  /** @param widthInPixels
    * @param heightInPixels
    * @param cuspRadiusInPixels
    * @param center
    *
    * @return
    */
  def apply(
      widthInPixels: Double,
      heightInPixels: Double,
      cuspRadiusInPixels: Double,
      center: Pos
  ): VectorGraphic =
    apply(
      widthInPixels,
      heightInPixels,
      cuspRadiusInPixels,
      center,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor
    )

  /** @param widthInPixels
    * @param heightInPixels
    * @param cuspRadiusInPixels
    * @param center
    * @param hasBorder
    * @param hasFilling
    * @param color
    * @param fillColor
    *
    * @return
    */
  def apply(
      widthInPixels: Double,
      heightInPixels: Double,
      cuspRadiusInPixels: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color
  ): VectorGraphic =

    if widthInPixels < 0 then
      throw new IllegalArgumentException(
        s"Star pentagon's width cannot be negative (was: $widthInPixels)."
      )

    if heightInPixels < 0 then
      throw new IllegalArgumentException(
        s"Star pentagon's width cannot be negative (was: $heightInPixels)."
      )

    if cuspRadiusInPixels < 0 then
      throw new IllegalArgumentException(
        s"Length of star pentagon's cusp radius cannot be negative (was: $cuspRadiusInPixels)."
      )

    val circumRadius = Pentagon.limitCircumRadiusTo(widthInPixels, heightInPixels)

    val outerPoints = Pentagon.pointsFor(circumRadius, Angle.Zero).toList
    val innerPoints = cuspRadiusPointsFor(cuspRadiusInPixels).toList
    val points      = intersperse(outerPoints, innerPoints)

    Polygon(center, points, Pos.Origo, hasBorder, hasFilling, color, fillColor)

  /** @param circumRadiusInPixels
    * @param cuspRadiusInPixels
    * @param center
    * @param hasBorder
    * @param hasFilling
    * @param color
    * @param fillColor
    *
    * @return
    */
  def apply(
      circumRadiusInPixels: Double = DefaultCircleRadiusInPixels,
      cuspRadiusInPixels: Double = DefaultStarCuspRadiusInPixels,
      center: Pos = Pos.Origo,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: Color = DefaultPrimaryColor,
      fillColor: Color = DefaultSecondaryColor
  ): VectorGraphic =

    if circumRadiusInPixels < 0 then
      throw new IllegalArgumentException(
        s"Length of star pentagon's circum radius cannot be negative (was: $circumRadiusInPixels)."
      )

    if cuspRadiusInPixels < 0 then
      throw new IllegalArgumentException(
        s"Length of star pentagon's cusp radius cannot be negative (was: $cuspRadiusInPixels)."
      )

    val outerPoints = Pentagon.pointsFor(circumRadiusInPixels, Angle.Zero).toList
    val innerPoints = cuspRadiusPointsFor(cuspRadiusInPixels).toList
    val points      = intersperse(outerPoints, innerPoints)

    Polygon(center, points, Pos.Origo, hasBorder, hasFilling, color, fillColor)

  /** @param cuspRadiusInPixels
    *
    * @return
    */
  def cuspRadiusPointsFor(cuspRadiusInPixels: Double): Seq[Pos] =
    Pentagon.pointsFor(cuspRadiusInPixels, Pentagon.RotationalSymmetryAngle / 2)
