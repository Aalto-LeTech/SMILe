package smile.pictures

import smile.colors.{Color, LinearGradient, Paint, PresetColor}
import smile.modeling.*
import smile.pictures.StrokeStyle.{Cap, Join}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Random

@JSExportTopLevel("TestPolygon")
object TestPolygon:
  def fill(paint: Paint): Option[FillStyle] = Some(FillStyle(paint))

  def stroke(paint: Paint, width: Double, round: Boolean): Option[StrokeStyle] =
    Some(
      StrokeStyle(
        paint,
        width,
        if round then Cap.Round else Cap.Square,
        if round then Join.Round else Join.Miter
      )
    )

  def addNoise(pic: PictureElement, noiseStrength: Double): Bitmap =
    val bitmap = pic.toBitmap
    val transformed = bitmap.setColorsByLocation((x, y) =>
      val color      = bitmap.colorAt(x, y).getOrElse(PresetColor.Transparent)
      val redNoise   = Random.nextInt(255)
      val greenNoise = Random.nextInt(255)
      val blueNoise  = Random.nextInt(255)
      // Screen noise (1−(1−A)×(1−B))
      val newColor = Color(
        255,
        (1 - (1 - color.green / 255.0) * (1 - greenNoise * noiseStrength / 255.0)).toInt * 255,
        (1 - (1 - color.blue / 255.0) * (1 - blueNoise * noiseStrength / 255.0)).toInt * 255,
        color.opacity
      )
      newColor
    )
    transformed

  @JSExport
  val testPolygon = new Polygon(
    Pos(100, 100),
    Seq(
      Pos(0, 0),
      Pos(120, 10),
      Pos(90, 100),
      Pos(20, 90)
    ),
    Some(FillStyle(PresetColor.Red)),
    Some(StrokeStyle(PresetColor.Black, 4))
  )

  @JSExport
  val smile: Picture =
    val logoHeight    = 300.0
    val gradientWidth = logoHeight * 2.07
    val radius        = logoHeight / 2.0
    val padding       = 0
    val bgStartColor  = new Color(0xffb6e4fa)
    val bgEndColor    = new Color(0xff7e84c0)
    val bgGradient = new LinearGradient(
      Pos(-gradientWidth / 2.0, -logoHeight / 2.0),
      Pos(gradientWidth / 2.0, logoHeight / 2.0),
      bgStartColor,
      bgEndColor
    )
    val bgGradient2 = new LinearGradient(
      Pos(-gradientWidth / 2.0, -logoHeight / 2.1),
      Pos(gradientWidth / 2.0, logoHeight / 2.0),
      bgStartColor,
      bgEndColor
    )
    val leftEyeColor  = new Color(0xff0071b9)
    val rightEyeColor = new Color(0xff2ab261)
    val mouthColor    = new Color(0xfff15858)

//    val canvasHeight = logoHeight + padding * 2

    addNoise(
      Rectangle(
        gradientWidth - radius,
        logoHeight - radius,
        fill(bgGradient),
        stroke(bgGradient, radius, true)
      ),
      0.6
    ).moveBy(gradientWidth / 2, logoHeight / 2)
      .addAt(
        Rectangle(
          logoHeight / 10.0,
          logoHeight / 10.0,
          fill(leftEyeColor),
          stroke(leftEyeColor, logoHeight / 15.0, true)
        ).rotateByAroundOrigin(18),
        logoHeight / 4.25,
        logoHeight / 3.0,
        PositionType.UpperLeftCorner
      )
      .addAt(
        Triangle(
          logoHeight / 8.57,
          fill(rightEyeColor),
          stroke(rightEyeColor, logoHeight / 15.0, true)
        ).rotateByAroundOrigin(40),
        logoHeight / 1.87 + padding,
        logoHeight / 3.53 + padding,
        PositionType.UpperLeftCorner
      )
      .addAt(
        Arc(
          Pos.Origin,
          logoHeight / 1.51,
          logoHeight / 1.54,
          -138,
          130,
          0,
          None,
          stroke(mouthColor, logoHeight / 9.375, true)
        ),
        logoHeight / 7.21 + padding,
        logoHeight / 15.5 + padding,
        PositionType.UpperLeftCorner
      )
//      .addAt(
//        text("SMILe", "Hack", logoHeight / 3.33, fill(White), None),
//        Pos(logoHeight * 1.5, canvasHeight / 2),        PositionType.UpperLeftCorner
//
//      )
//      .show()

/** Represents a polygon defined by a sequence of points, with optional fill and stroke styles.
  *
  * @param pos
  *   The position of the polygon. This usually represents the centroid or a specific anchor point.
  * @param points
  *   The sequence of points defining the vertices of the polygon.
  * @param fillStyle
  *   Optional fill style for the interior of the polygon.
  * @param strokeStyle
  *   Optional stroke style for the outline of the polygon.
  */
@JSExportTopLevel("Polygon")
class Polygon(
    pos: Pos,
    val points: Seq[Pos],
    override val fillStyle: Option[FillStyle],
    override val strokeStyle: Option[StrokeStyle]
) extends VectorGraphic:

  override def copy(newPosition: Pos): PictureElement = new Polygon(
    newPosition,
    points,
    fillStyle,
    strokeStyle
  )

  private def internalCopy(
      newPosition: Pos = position,
      newPoints: Seq[Pos] = points,
      newFillStyle: Option[FillStyle] = fillStyle,
      newStrokeStyle: Option[StrokeStyle] = strokeStyle
  ): Polygon =
    new Polygon(
      newPosition,
      newPoints,
      newFillStyle,
      newStrokeStyle
    )

  override lazy val position: Pos = pos

  /** Computes the boundary of the polygon based on its points.
    */
  private val contentBoundary: Bounds =
    BoundaryCalculator.fromPositions(points)

  /** Determines the corners of the polygon's bounding box.
    */
  lazy val corners: Seq[Pos] =
    val strokeRadius = strokeStyle.map(_.width).getOrElse(0.0) / 2

    val ulX = contentBoundary.upperLeftCorner.x - strokeRadius
    val ulY = contentBoundary.upperLeftCorner.y - strokeRadius
    val lrX = contentBoundary.lowerRightCorner.x + strokeRadius
    val lrY = contentBoundary.lowerRightCorner.y + strokeRadius

    Seq(
      position + contentBoundary.upperLeftCorner - Pos(strokeRadius, strokeRadius),
      position + Pos(lrX, ulY),
      position + contentBoundary.lowerRightCorner + Pos(strokeRadius, strokeRadius),
      position + Pos(ulX, lrY)
    )

  lazy val upperLeftCorner: Pos  = corners.head
  lazy val lowerRightCorner: Pos = corners.tail.tail.head

  override lazy val boundary: Bounds =
    Bounds(upperLeftCorner, lowerRightCorner)

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Polygon =
    internalCopy(
      newPoints = points.map(
        _.scaleByRelativeToOrigin(horizontalFactor, verticalFactor)
      )
    )

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Polygon =
    internalCopy(
      newPosition = Transformer.scale(position, horizontalFactor, verticalFactor, relativityPoint),
      newPoints = points.map(
        _.scaleByRelativeToOrigin(horizontalFactor, verticalFactor)
      )
    )

  override def rotateBy(angle: Double, centerOfRotation: Pos): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle, centerOfRotation),
      newPoints = points.map(_.rotateBy(angle, centerOfRotation))
    )

  override def rotateByAroundOrigin(angle: Double): Polygon =
    internalCopy(
      newPosition = Transformer.rotate(position, angle),
      newPoints = points.map(_.rotateByAroundOrigin(angle))
    )
