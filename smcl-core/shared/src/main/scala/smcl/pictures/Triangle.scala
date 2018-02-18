/* .            .           .                   .                 +             .          +      */
/*         +-----------+  +---+    +  +---+  +-----------+  +---+    Media Programming in Scala   */
/*   *     |           |  |    \     /    |  |           | +|   |            Since 2015           */
/*         |   +-------+  |     \   /     |  |   +-------+  |   |   .                        .    */
/*         |   |          |      \ /      |  |   |          |   |         Aalto University        */
/*       . |   +-------+  |   .   V   .   |  |   |   .      |   |      .   Espoo, Finland       . */
/*  +      |           |  |   |\     /|   |  |   |          |   |                  .    +         */
/*         +------+    |  |   | \   / |   |  |   |          |   |    +        *                   */
/*    *           |    |  |   |  \ /  |   |  |   |      *   |   |                     .      +    */
/*      -- +------+    |  |   |   V  *|   |  |   +-------+  |   +-------+ --    .                 */
/*    ---  |           |  |   | .     |   |  |           |  |           |  ---      +      *      */
/*  ------ +-----------+  +---+       +---+  +-----------+  +-----------+ ------               .  */
/*                                                                                     .          */
/*     T H E   S C A L A   M E D I A   C O M P U T A T I O N   L I B R A R Y      .         +     */
/*                                                                                    *           */

package smcl.pictures

import smcl.colors.rgb
import smcl.infrastructure.MathUtils
import smcl.modeling.Angle
import smcl.modeling.d2.Pos
import smcl.settings._




/**
 * An object-based API for creating triangles.
 *
 * @author Aleksi Lukkarinen
 */
object Triangle {

  /** The number of corners (i.e., three) in a triangle. */
  val NumberOfCornersInTriangle: Int = 3

  /** Height of an equilateral triangle as a factor of the length of its side. */
  val HeightOfEquilateralTriangleAsFactorOfSide: Double = math.sqrt(3) / 2.0

  /** Side length of an equilateral triangle as a factor of its halfHeight. */
  val SideOfEquilateralTriangleAsFactorOfHeight: Double =
    1.0 / HeightOfEquilateralTriangleAsFactorOfSide

  /**
   * Creates a new equilateral triangle.
   *
   * @param sideLength
   *
   * @return
   */
  @inline
  def apply(sideLength: Double): VectorGraphic =
    apply(
      sideLength,
      Pos.Origo,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)

  /**
   * Creates a new equilateral triangle.
   *
   * @param sideLength
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    apply(
      sideLength,
      Pos.Origo,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new equilateral triangle that has a specific center point.
   *
   * @param sideLength
   * @param center
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      center: Pos): VectorGraphic = {

    apply(
      sideLength,
      center,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)
  }

  /**
   * Creates a new equilateral triangle that has a specific center point.
   *
   * @param sideLength
   * @param center
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    validateSide(sideLength, "side")

    val halfHeight: Double =
      HeightOfEquilateralTriangleAsFactorOfSide * sideLength / 2.0

    createIsosceles(
      halfHeight, sideLength / 2.0,
      center,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new equilateral triangle.
   *
   * @param height
   *
   * @return
   */
  @inline
  def basedOnHeight(height: Double): VectorGraphic =
    basedOnHeight(
      height,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)

  /**
   * Creates a new equilateral triangle.
   *
   * @param height
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def basedOnHeight(
      height: Double,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: rgb.Color = DefaultPrimaryColor,
      fillColor: rgb.Color = DefaultSecondaryColor): VectorGraphic = {

    basedOnHeight(
      height,
      Pos.Origo,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new equilateral triangle that has a specific center point.
   *
   * @param height
   * @param center
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def basedOnHeight(
      height: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    if (height < 0) {
      throw new IllegalArgumentException(
        s"Height of triangle cannot be negative (was: $height).")
    }

    val halfSide: Double =
      SideOfEquilateralTriangleAsFactorOfHeight * height / 2.0

    createIsosceles(
      height / 2.0, halfSide,
      center,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new isosceles triangle.
   *
   * @param sideLength
   * @param baseLength
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      baseLength: Double): VectorGraphic = {

    apply(
      sideLength, baseLength,
      Pos.Origo,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)
  }

  /**
   * Creates a new isosceles triangle.
   *
   * @param sideLength
   * @param baseLength
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      baseLength: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    apply(
      sideLength, baseLength,
      Pos.Origo,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new isosceles triangle that has a specific center point.
   *
   * @param sideLength
   * @param baseLength
   * @param center
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      baseLength: Double,
      center: Pos): VectorGraphic = {

    apply(
      sideLength,
      baseLength,
      center,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)
  }

  /**
   * Creates a new isosceles triangle that has a specific center point.
   *
   * @param sideLength
   * @param baseLength
   * @param center
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def apply(
      sideLength: Double,
      baseLength: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    validateSide(baseLength, "base")
    validateSide(sideLength, "side")

    val halfHeight: Double =
      math.sqrt(sideLength * sideLength - baseLength * baseLength / 4.0) / 2.0

    createIsosceles(
      halfHeight, baseLength / 2.0,
      center,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new isosceles triangle.
   *
   * @param height
   * @param baseLength
   *
   * @return
   */
  @inline
  def basedOnHeightAndBase(
      height: Double,
      baseLength: Double): VectorGraphic = {

    basedOnHeightAndBase(
      height, baseLength,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)
  }

  /**
   * Creates a new isosceles triangle.
   *
   * @param height
   * @param baseLength
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def basedOnHeightAndBase(
      height: Double,
      baseLength: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    basedOnHeightAndBase(
      height, baseLength,
      Pos.Origo,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new isosceles triangle that has a specific center point.
   *
   * @param height
   * @param baseLength
   * @param center
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def basedOnHeightAndBase(
      height: Double,
      baseLength: Double,
      center: Pos,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: rgb.Color = DefaultPrimaryColor,
      fillColor: rgb.Color = DefaultSecondaryColor): VectorGraphic = {

    validateSide(baseLength, "base")

    if (height < 0) {
      throw new IllegalArgumentException(
        s"Height of triangle cannot be negative (was: $height).")
    }

    createIsosceles(
      height / 2.0, baseLength / 2.0,
      center,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new isosceles triangle that has a specific center point.
   *
   * @param halfHeight
   * @param halfBase
   * @param center
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  private
  def createIsosceles(
      halfHeight: Double,
      halfBase: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    val firstCorner: Pos = Pos(
      center.xInPixels,
      center.yInPixels - halfHeight)

    val bottomY = center.yInPixels + halfHeight

    val secondCorner: Pos = Pos(
      center.xInPixels + halfBase,
      bottomY)

    val thirdCorner: Pos = Pos(
      center.xInPixels - halfBase,
      bottomY)

    apply(
      firstCorner, secondCorner, thirdCorner,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new (expectedly) scalene triangle.
   *
   * @param baseLength
   * @param leftSideLength
   * @param rightSideLength
   *
   * @return
   */
  def apply(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double): VectorGraphic = {

    apply(
      baseLength, leftSideLength, rightSideLength,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)
  }

  /**
   * Creates a new (expectedly) scalene triangle.
   *
   * @param baseLength
   * @param leftSideLength
   * @param rightSideLength
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  def apply(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    apply(
      baseLength, leftSideLength, rightSideLength,
      Pos.Origo,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new (expectedly) scalene triangle that has a specific center point.
   *
   * @param baseLength
   * @param leftSideLength
   * @param rightSideLength
   * @param center
   *
   * @return
   */
  def apply(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double,
      center: Pos): VectorGraphic = {

    apply(
      baseLength, leftSideLength, rightSideLength,
      center,
      hasBorder = ShapesHaveBordersByDefault,
      hasFilling = ShapesHaveFillingsByDefault,
      color = DefaultPrimaryColor,
      fillColor = DefaultSecondaryColor)
  }

  /**
   * Creates a new (expectedly) scalene triangle that has a specific center point.
   *
   * @param baseLength
   * @param leftSideLength
   * @param rightSideLength
   * @param center
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  def apply(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double,
      center: Pos,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: rgb.Color,
      fillColor: rgb.Color): VectorGraphic = {

    validateSides(baseLength, leftSideLength, rightSideLength)

    val leftAngle: Double =
      MathUtils.acos(
        (leftSideLength * leftSideLength +
            baseLength * baseLength -
            rightSideLength * rightSideLength) /
            (2 * leftSideLength * baseLength))

    println(leftAngle)

    val prelimTop =
      Pos(leftSideLength, 0).rotateBy(-leftAngle)

    val halfHeight = prelimTop.yInPixels / 2.0

    val xOffset =
      if (leftAngle <= Angle.RightAngleInDegrees) {
        val halfWidth = prelimTop.xInPixels.max(baseLength) / 2.0
        -halfWidth
      }
      else {
        val halfWidth = (prelimTop.xInPixels.abs + baseLength) / 2.0
        halfWidth - baseLength
      }

    println(xOffset)

    val firstCorner: Pos = Pos(
      center.xInPixels + prelimTop.xInPixels + xOffset,
      center.yInPixels - halfHeight)

    val bottomY = center.yInPixels + halfHeight

    val secondCorner: Pos = Pos(
      center.xInPixels + xOffset,
      bottomY)

    val thirdCorner: Pos = Pos(
      secondCorner.xInPixels + baseLength,
      bottomY)

    apply(
      firstCorner, secondCorner, thirdCorner,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   * Creates a new (expectedly) scalene triangle.
   *
   * @param firstCorner
   * @param secondCorner
   * @param thirdCorner
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  def apply(
      firstCorner: Pos,
      secondCorner: Pos,
      thirdCorner: Pos,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: rgb.Color = DefaultPrimaryColor,
      fillColor: rgb.Color = DefaultSecondaryColor): VectorGraphic = {

    val points: Seq[Pos] =
      Seq(firstCorner, secondCorner, thirdCorner)

    // TODO: When no filling, create a Polyline, after it is implemented
    Polygon(points, hasBorder, hasFilling, color, fillColor)
  }

  /**
   * Test if the triangle inequality holds for the given side lengths.
   *
   * @param baseLength      length of the base
   * @param leftSideLength  length of the left side
   * @param rightSideLength length of the right side
   *
   * @return
   */
  @inline
  def validateSides(
      baseLength: Double,
      leftSideLength: Double,
      rightSideLength: Double): Unit = {

    validateSide(baseLength, "base")
    validateSide(leftSideLength, "left side")
    validateSide(rightSideLength, "right side")

    checkTriangleInequality(
      baseLength, leftSideLength, rightSideLength)
  }

  /**
   *
   *
   * @param length
   * @param name
   *
   * @return
   */
  @inline
  def validateSide(
      length: Double,
      name: String): Unit = {

    if (length < 0) {
      throw new IllegalArgumentException(
        s"Length of triangle's $name cannot be negative (was: $length).")
    }
  }

  /**
   * Test if the triangle inequality holds for the given side lengths.
   *
   * @param a length of first side
   * @param b length of second side
   * @param c length of third side
   *
   * @return
   */
  @inline
  def checkTriangleInequality(
      a: Double,
      b: Double,
      c: Double): Unit = {

    if (!((a + b >= c) && (a + c >= b) && (b + c >= a))) {
      throw new IllegalArgumentException(
        s"Illegal side lengths ($a, $b, $c): The triangle inequality does not hold.")
    }
  }

}
