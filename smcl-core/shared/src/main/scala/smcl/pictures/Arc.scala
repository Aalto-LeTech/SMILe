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
import smcl.infrastructure.{DrawingSurfaceAdapter, Identity}
import smcl.modeling.d2.{Bounds, Dims, Pos}
import smcl.modeling.{AffineTransformation, Angle}
import smcl.settings._




/**
 * An object-based API for creating arcs.
 *
 * @author Aleksi Lukkarinen
 */
object Arc {

  /**  */
  private
  val InitialScalingFactor = 1.0

  /**  */
  private
  val InitialShearingFactor = 0.0

  /**
   *
   *
   * @param upperLeftCorner
   * @param lowerRightCorner
   * @param startAngleInDegrees
   * @param arcAngleInDegrees
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   *
   * @return
   */
  @inline
  def apply(
      upperLeftCorner: Pos,
      lowerRightCorner: Pos,
      startAngleInDegrees: Double = Angle.Zero.inDegrees,
      arcAngleInDegrees: Double = Angle.FullAngleInDegrees,
      hasBorder: Boolean = ShapesHaveBordersByDefault,
      hasFilling: Boolean = ShapesHaveFillingsByDefault,
      color: rgb.Color = DefaultPrimaryColor,
      fillColor: rgb.Color = DefaultSecondaryColor): VectorGraphic = {

    val identity = Identity()
    val currentRotationAngleInDegrees = Angle.Zero.inDegrees
    val currentHorizontalScalingFactor = InitialScalingFactor
    val currentVerticalScalingFactor = InitialScalingFactor
    val currentHorizontalShearingFactor = InitialShearingFactor
    val currentVerticalShearingFactor = InitialShearingFactor
    val currentTransformation = AffineTransformation.Identity

    new Arc(
      identity,
      upperLeftCorner, lowerRightCorner,
      startAngleInDegrees, arcAngleInDegrees,
      currentRotationAngleInDegrees,
      currentHorizontalScalingFactor,
      currentVerticalScalingFactor,
      currentHorizontalShearingFactor,
      currentVerticalShearingFactor,
      currentTransformation,
      hasBorder, hasFilling,
      color, fillColor)
  }

}




/**
 *
 *
 * @param identity
 * @param upperLeftCorner
 * @param lowerRightCorner
 * @param startAngleInDegrees
 * @param arcAngleInDegrees
 * @param currentRotationAngleInDegrees
 * @param currentHorizontalScalingFactor
 * @param currentVerticalScalingFactor
 * @param currentHorizontalShearingFactor
 * @param currentVerticalShearingFactor
 * @param currentTransformation
 * @param hasBorder
 * @param hasFilling
 * @param color
 * @param fillColor
 *
 * @author Aleksi Lukkarinen
 */
class Arc private(
    val identity: Identity,
    val upperLeftCorner: Pos,
    val lowerRightCorner: Pos,
    val startAngleInDegrees: Double,
    val arcAngleInDegrees: Double,
    val currentRotationAngleInDegrees: Double,
    val currentHorizontalScalingFactor: Double,
    val currentVerticalScalingFactor: Double,
    val currentHorizontalShearingFactor: Double,
    val currentVerticalShearingFactor: Double,
    val currentTransformation: AffineTransformation,
    val hasBorder: Boolean = ShapesHaveBordersByDefault,
    val hasFilling: Boolean = ShapesHaveFillingsByDefault,
    val color: rgb.Color = DefaultPrimaryColor,
    val fillColor: rgb.Color = DefaultSecondaryColor)
    extends VectorGraphic {

  /** Boundary of this [[Arc]]. */
  // TODO: Calculate boundary so that it reflects the current transformation!!!!
  override
  val boundary: Bounds = Bounds(upperLeftCorner, lowerRightCorner)

  /** Dimensions of this [[Arc]]. */
  override
  val dimensions: Dims = boundary.dimensions

  /** Position of this [[Arc]]. */
  override
  val position: Pos = Pos(
    internalCenter.width.inPixels,
    internalCenter.height.inPixels)

  /** Tells if this [[Arc]] can be rendered on a bitmap. */
  override
  val isRenderable: Boolean = true

  /**
   * Renders this [[Arc]] on a drawing surface.
   *
   * @param drawingSurface
   * @param offsetsToOrigo
   */
  override
  def renderOn(
      drawingSurface: DrawingSurfaceAdapter,
      offsetsToOrigo: Dims): Unit = {

    val topLeftX = offsetsToOrigo.width.inPixels + upperLeftCorner.xInPixels
    val topLeftY = offsetsToOrigo.height.inPixels + upperLeftCorner.yInPixels

    drawingSurface.drawArc(
      topLeftX, topLeftY,
      dimensions.width.inPixels,
      dimensions.height.inPixels,
      startAngleInDegrees,
      arcAngleInDegrees,
      currentTransformation,
      hasBorder, hasFilling,
      color, fillColor)
  }

  /**
   *
   *
   * @param offsets
   *
   * @return
   */
  @inline
  def moveBy(offsets: Double*): Arc = {
    copy(
      newUpperLeftCorner = upperLeftCorner + offsets,
      newLowerRightCorner = lowerRightCorner + offsets)
  }

  /**
   *
   *
   * @param newUpperLeftCorner
   * @param newLowerRightCorner
   * @param newStartAngleInDegrees
   * @param newArcAngleInDegrees
   * @param newHasBorder
   * @param newHasFilling
   * @param newColor
   * @param newFillColor
   *
   * @return
   */
  @inline
  def copy(
      newUpperLeftCorner: Pos = upperLeftCorner,
      newLowerRightCorner: Pos = lowerRightCorner,
      newStartAngleInDegrees: Double = startAngleInDegrees,
      newArcAngleInDegrees: Double = arcAngleInDegrees,
      newHasBorder: Boolean = hasBorder,
      newHasFilling: Boolean = hasFilling,
      newColor: rgb.Color = color,
      newFillColor: rgb.Color = fillColor): Arc = {

    internalCopy(
      newUpperLeftCorner,
      newLowerRightCorner,
      newStartAngleInDegrees,
      newArcAngleInDegrees,
      newHasBorder = newHasBorder,
      newHasFilling = newHasFilling,
      newColor = newColor,
      newFillColor = newFillColor)
  }


  /**
   *
   *
   * @param newUpperLeftCorner
   * @param newLowerRightCorner
   * @param newStartAngleInDegrees
   * @param newArcAngleInDegrees
   * @param newRotationAngleInDegrees
   * @param newHorizontalScalingFactor
   * @param newVerticalScalingFactor
   * @param newHorizontalShearingFactor
   * @param newVerticalShearingFactor
   * @param newTransformation
   * @param newHasBorder
   * @param newHasFilling
   * @param newColor
   * @param newFillColor
   *
   * @return
   */
  @inline
  private
  def internalCopy(
      newUpperLeftCorner: Pos = upperLeftCorner,
      newLowerRightCorner: Pos = lowerRightCorner,
      newStartAngleInDegrees: Double = startAngleInDegrees,
      newArcAngleInDegrees: Double = arcAngleInDegrees,
      newRotationAngleInDegrees: Double = currentRotationAngleInDegrees,
      newHorizontalScalingFactor: Double = currentHorizontalScalingFactor,
      newVerticalScalingFactor: Double = currentVerticalScalingFactor,
      newHorizontalShearingFactor: Double = currentHorizontalShearingFactor,
      newVerticalShearingFactor: Double = currentVerticalShearingFactor,
      newTransformation: AffineTransformation = currentTransformation,
      newHasBorder: Boolean = hasBorder,
      newHasFilling: Boolean = hasFilling,
      newColor: rgb.Color = color,
      newFillColor: rgb.Color = fillColor): Arc = {

    new Arc(
      identity,
      newUpperLeftCorner, newLowerRightCorner,
      newStartAngleInDegrees, newArcAngleInDegrees,
      newRotationAngleInDegrees,
      newHorizontalScalingFactor, newVerticalScalingFactor,
      newHorizontalShearingFactor, newVerticalShearingFactor,
      newTransformation,
      newHasBorder, newHasFilling,
      newColor, newFillColor)
  }

  /**
   *
   *
   * @param widthFactor
   * @param heightFactor
   *
   * @return
   */
  override
  def scaleBy(
      widthFactor: Double,
      heightFactor: Double): Arc = {

    val newWidthFactor = widthFactor * currentHorizontalScalingFactor
    val newHeightFactor = heightFactor * currentVerticalScalingFactor
    val newTransformation =
      currentTransformation.scaleRelativeToPoint(
        newWidthFactor, newHeightFactor, position)

    internalCopy(
      newHorizontalScalingFactor = newWidthFactor,
      newVerticalScalingFactor = newHeightFactor,
      newTransformation = newTransformation)
  }

  /**
   * Rotates this object around the origo (0,0) by 90 degrees clockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCW: ImageElement = {
    val newRotationAngle = currentRotationAngleInDegrees - Angle.RightAngleInDegrees
    val newTransformation = currentTransformation.rotate90DegsCWAroundPoint(position)

    internalCopy(
      newRotationAngleInDegrees = newRotationAngle,
      newTransformation = newTransformation)
  }

  /**
   * Rotates this object around a given point by 90 degrees clockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy90DegsCW(centerOfRotation: Pos): ImageElement = {
    this
  }

  /**
   * Rotates this object around the origo (0,0) by 90 degrees counterclockwise.
   *
   * @return
   */
  override
  def rotateBy90DegsCCW: ImageElement = {
    val newRotationAngle = currentRotationAngleInDegrees + Angle.RightAngleInDegrees
    val newTransformation = currentTransformation.rotate90DegsCCWAroundPoint(position)

    internalCopy(
      newRotationAngleInDegrees = newRotationAngle,
      newTransformation = newTransformation)
  }

  /**
   * Rotates this object around a given point by 90 degrees counterclockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy90DegsCCW(centerOfRotation: Pos): ImageElement = {
    this
  }

  /**
   * Rotates this object around the origo (0,0) by 180 degrees.
   *
   * @return
   */
  override
  def rotateBy180Degs: ImageElement = {
    val newRotationAngle = currentRotationAngleInDegrees + Angle.StraightAngleInDegrees
    val newTransformation = currentTransformation.rotate180DegsAroundPoint(position)

    internalCopy(
      newRotationAngleInDegrees = newRotationAngle,
      newTransformation = newTransformation)
  }

  /**
   * Rotates this object around a given point by 180 degrees.
   *
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy180Degs(centerOfRotation: Pos): ImageElement = {
    this
  }

  /**
   * Rotates this object around the origo (0,0) by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  override
  def rotateBy(angleInDegrees: Double): ImageElement = {
    val newRotationAngle = currentRotationAngleInDegrees + angleInDegrees
    val newTransformation =
      currentTransformation.rotateAroundPoint(Angle(angleInDegrees), position)

    internalCopy(
      newRotationAngleInDegrees = newRotationAngle,
      newTransformation = newTransformation)
  }

  /**
   * Rotates this object around a given point by the specified number of degrees.
   *
   * @param angleInDegrees
   * @param centerOfRotation
   *
   * @return
   */
  override
  def rotateBy(
      angleInDegrees: Double,
      centerOfRotation: Pos): ImageElement = {

    this
  }

}
