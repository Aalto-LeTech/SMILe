package smile.pictures

import smile.colors.Color
import smile.modeling.*

class Picture(
    val elements: Seq[PictureElement],
    val viewport: Option[Viewport]
) extends Transformable[Picture]:
  val referencePoints: Map[String, Pos] = elements
    .collect:
      case e: ReferencePoint => e.name -> e.position
    .toMap

  def this(element: PictureElement) =
    this(Seq(element), None)

  def this(elements: Seq[PictureElement]) = this(elements, None)

  def this() = this(Seq(), None)

  lazy val width: Len             = boundary.width
  lazy val height: Len            = boundary.height

  override def moveBy(xOffset: Double, yOffset: Double): Picture =
    map(_.moveBy(xOffset, yOffset))

  override def copy(newPosition: Pos): Picture =
//    Picture(elements.map(_.moveTo(newPosition.x, newPosition.y, PositionType.Center)), viewport)
    moveTo(newPosition.x, newPosition.y, PositionType.Center) // TODO: check

  override def copy(newMatrix: TransformationMatrix): Picture =
    Picture(elements.map(_.copy(newMatrix)), viewport)

  def copy(newViewport: Option[Viewport]): Picture =
    Picture(elements, newViewport)

  def copy(newElements: Seq[PictureElement]): Picture =
    Picture(newElements, viewport)

  lazy val hasViewport: Boolean = viewport.isDefined

  lazy val boundary: Bounds = BoundaryCalculator.fromBoundaries(elements)

  def setViewport(viewport: Viewport): Picture =
    copy(newViewport = Option(viewport))

  def withContentBoundaryAsViewport: Picture =
    copy(newViewport = Option(Viewport(boundary)))

  def removeViewport(): Picture = copy(newViewport = None)

  /** @param another
    * @param pixelMerger
    * @return
    */
  def mergePixelsWith(another: Picture, pixelMerger: (Color, Color) => Color): Bitmap =
    toBitmap.mergeWith(another.toBitmap, pixelMerger)

  def map(f: PictureElement => PictureElement): Picture =
    copy(newElements = elements.map(f))

//  def scaleBy(horizontalFactor: Double, verticalFactor: Double): Picture =
//    val relativityPoint = this.boundary.center
//
//    val scaledPos = this.position.scaleBy(horizontalFactor, verticalFactor, relativityPoint)
//    val returnToPositionOffset = this.position - scaledPos
//
//    map(
//      _.scaleBy(horizontalFactor, verticalFactor, relativityPoint)
//        .moveBy(returnToPositionOffset.x, returnToPositionOffset.y)
//    )
//  end scaleBy

  /** Scales this object by given horizontal and vertical factors in relation to a given point.
    *
    * @param horizontalFactor
    * @param verticalFactor
    * @param relativityPoint
    * @return
    */
//  def scaleBy(
//      horizontalFactor: Double,
//      verticalFactor: Double,
//      relativityPoint: Pos
//  ): Picture =
//    val scaledPic = scaleBy(horizontalFactor, verticalFactor)
//
//    val relativityDistance = scaledPic.position - relativityPoint
//    val scaledXOffset =
//      horizontalFactor * relativityDistance.x - relativityDistance.x
//    val scaledYOffset = verticalFactor * relativityDistance.y - relativityDistance.y
//
//    scaledPic.map(_.moveBy(scaledXOffset, scaledYOffset))
//  end scaleBy

  override def rotateBy(angle: Double, centerOfRotation: Pos): Picture =
    if elements.length == 1 then
      val element = elements.head
      new Picture(Seq(element.rotateBy(angle, centerOfRotation)), viewport)
    else
      val pictureCenter = boundary.center
      new Picture(
        elements.map(element =>
          val elementCenter = element.boundary.center
          val relativeCenterOfRotation = Pos(
            centerOfRotation.x - pictureCenter.x + elementCenter.x,
            centerOfRotation.y - pictureCenter.y + elementCenter.y
          )
          element.rotateBy(angle, relativeCenterOfRotation)
        ),
        viewport
      )

  override inline def rotateByAroundOrigo(angle: Double): Picture =
    map(_.rotateByAroundOrigo(angle))

  override def scaleTo(targetWidth: Double, targetHeight: Double): Picture =
    map(_.scaleTo(targetWidth, targetHeight)) // TODO: check
//    val pictureWidth  = boundary.width.inPixels
//    val pictureHeight = boundary.height.inPixels
//
//    val scaledElements = elements.map(element =>
//      val elementCenter    = element.boundary.center
//      val horizontalFactor = targetWidth / pictureWidth
//      val verticalFactor   = targetHeight / pictureHeight
//
////      val elementTransformMatrix = new TransformationMatrix()
////        .scale(horizontalFactor, verticalFactor, elementCenter)
////
////      element.copy(elementTransformMatrix)
//      element.scaleTo(
//        horizontalFactor,
//        verticalFactor,
//        elementCenter
//      )
//    )
//
//    new Picture(scaledElements, viewport)

  def toAsciiArt: String = toBitmap.toAsciiArt()

  def toAsciiColorBlocks: String = toBitmap.toAsciiColorBlocks()
