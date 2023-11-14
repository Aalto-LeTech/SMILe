package smile.pictures

import smile.Settings.{
  DefaultHorizontalAlignment,
  DefaultPaddingInPixels,
  DefaultPositionType,
  DefaultVerticalAlignment
}
import smile.modeling.{
  BoundaryCalculator,
  Bounds,
  HorizontalAlignment,
  Pos,
  PositionType,
  Side,
  SideIndependentAlignment,
  TransformationMatrix,
  VerticalAlignment
}

import scala.annotation.tailrec

object Transformable:
  type SimpleTransformer = Picture => Picture
  val IdentitySimpleTransformer: SimpleTransformer = (i: Picture) => i

trait Transformable[TransformableType <: Transformable[TransformableType]]:
  lazy val boundary: Bounds
  val transformationMatrix: TransformationMatrix = TransformationMatrix.identity
  lazy val position: Pos                         = boundary.center

  def copy(newPosition: Pos): TransformableType
  def copy(newMatrix: TransformationMatrix): TransformableType

  def toBitmap: Bitmap = this match
    case picture: Picture =>
      if picture.elements.length != 1 then Bitmap(picture)
      else
        picture.elements.head match
          case bitmap: Bitmap =>
            bitmap
          case element: PictureElement =>
            Bitmap(element)
    case element: PictureElement =>
      Bitmap(element)

  private inline def asPicture: Picture = this match
    case picture: Picture =>
      picture
    case element: PictureElement =>
      new Picture(element)

  case class ElementWithCoordinates(element: PictureElement, x: Double, y: Double)

  def addAt(
      contentsAndCoordinates: Seq[ElementWithCoordinates],
      positionType: PositionType
  ): Picture =
    val movedContent = contentsAndCoordinates.map:
      case ElementWithCoordinates(element, x, y) =>
        element.moveTo(x, y, positionType)

    addToFront(new Picture(movedContent))

  def addAt(
      contentAndCoordinatesInPixels: ElementWithCoordinates,
      positionType: PositionType
  ): Picture =
    addAt(
      contentAndCoordinatesInPixels.element,
      contentAndCoordinatesInPixels.x,
      contentAndCoordinatesInPixels.y,
      positionType
    )

  def addAt(
      content: Transformable[?],
      xCoordinateInPixels: Double,
      yCoordinateInPixels: Double,
      positionType: PositionType = DefaultPositionType
  ): Picture =
    addToFront(content.moveTo(xCoordinateInPixels, yCoordinateInPixels, positionType))

  def addToFront(toPrepend: Transformable[?]): Picture =
    prependTo(toPrepend.asPicture, this.asPicture)

  def addToBack(toAppend: Transformable[?]): Picture =
    appendTo(toAppend.asPicture, this.asPicture)

  //  def addToBack(content: Seq[Transformable]): Picture =
  //    new Picture(appendTo(content, Seq(this)))

  protected final def appendTo(
      contentToAppend: Picture,
      existingContent: Picture
  ): Picture =
    new Picture(existingContent.elements ++ contentToAppend.elements)

  protected final def prependTo(
      contentToPrepend: Picture,
      existingContent: Picture
  ): Picture =
    // new Picture(contentToPrepend.elements ++ existingContent.elements)

    val n = new Picture(contentToPrepend.elements ++ existingContent.elements)
//    println(n.elements.map(_.position))
//    println(n.elements.map(_.getClass.getName))
    n

  def addTo(
      targetSide: Side,
      content: Transformable[?],
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: SideIndependentAlignment
  ): Picture =
    targetSide match
      case Side.Top    => addToTop(content, paddingInPixels, alignment.toHorizontal)
      case Side.Bottom => addToBottom(content, paddingInPixels, alignment.toHorizontal)
      case Side.Left   => addToLeft(content, paddingInPixels, alignment.toVertical)
      case Side.Right  => addToRight(content, paddingInPixels, alignment.toVertical)

  /** @param content
    * @param padding
    *   in pixels
    * @param alignment
    * @return
    */
  def addToTop(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    val newUpperLeftCornerX = boundary.horizontalPositionFor(alignment, content.boundary)

    val newUpperLeftCornerY =
      boundary.upperLeftCorner.y - padding - content.boundary.height.inPixels

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToTop

  /** @param content
    * @param padding
    *   in pixels
    * @param alignment
    * @return
    */
  def addToRight(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =

    val newUpperLeftCornerX = boundary.lowerRightCorner.x + padding
    val newUpperLeftCornerY = boundary.verticalPositionFor(alignment, content.boundary)

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToRight

  /** @param content
    * @param paddingInPixels
    * @param alignment
    * @return
    */
  def addToBottom(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =

    val newUpperLeftCornerX = boundary.horizontalPositionFor(alignment, content.boundary)
    val newUpperLeftCornerY = boundary.lowerRightCorner.y + padding

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToBottom

  /** @param content
    * @param paddingInPixels
    * @param alignment
    * @return
    */
  def addToLeft(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    val newUpperLeftCornerX =
      boundary.upperLeftCorner.x - padding - content.boundary.width.inPixels

    val newUpperLeftCornerY = boundary.verticalPositionFor(alignment, content.boundary)

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToLeft

  // Alternating
  // -------------------------------------------------------------------------------------------- \\
  def alternateUpwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    alternateWith(alternatives, Side.Top, numberOfAlternations, padding, alignment.sideIndependent)

  def alternateDownwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    alternateWith(
      alternatives,
      Side.Bottom,
      numberOfAlternations,
      padding,
      alignment.sideIndependent
    )

  def alternateLeftwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    alternateWith(alternatives, Side.Left, numberOfAlternations, padding, alignment.sideIndependent)

  def alternateRightwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    alternateWith(
      alternatives,
      Side.Right,
      numberOfAlternations,
      padding,
      alignment.sideIndependent
    )

  def alternateWith(
      alternatives: Seq[Transformable[?]],
      side: Side,
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: SideIndependentAlignment
  ): Picture =

    val NumberOfFirstAlternation = 1

    require(
      numberOfAlternations >= 0,
      s"Number of alternations cannot be negative (was $numberOfAlternations)"
    )

    val numberOfSourceElements = alternatives.length + 1

    @tailrec
    def alternate(numberOfNextAlternation: Int, resultPicture: Picture): Picture =

      if numberOfNextAlternation > numberOfAlternations then return resultPicture

      val numberOfNextElement = numberOfNextAlternation % numberOfSourceElements
      val selectedElement =
        if numberOfNextElement == 0 then this
        else alternatives(numberOfNextElement - 1)

      alternate(
        numberOfNextAlternation + 1,
        resultPicture.addTo(side, selectedElement, padding, alignment)
      )
    end alternate

    alternate(NumberOfFirstAlternation, resultPicture = this.asPicture)

  end alternateWith

  // Replicating
  // -------------------------------------------------------------------------------------------- \\

  def replicateUpwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(Side.Top, numberOfReplicas, padding, alignment.sideIndependent, transformer)

  def replicateDownwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(
      Side.Bottom,
      numberOfReplicas,
      padding,
      alignment.sideIndependent,
      transformer
    )

  def replicateLeftwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(
      Side.Left,
      numberOfReplicas,
      padding,
      alignment.sideIndependent,
      transformer
    )

  def replicateRightwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(
      Side.Right,
      numberOfReplicas,
      padding,
      alignment.sideIndependent,
      transformer
    )

  def replicateTo(
      side: Side,
      numberOfReplicas: Int,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: SideIndependentAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    require(numberOfReplicas >= 0, s"Number of replicas cannot be negative (was $numberOfReplicas)")

    @tailrec
    def replicate(
        replicasLeft: Int,
        previousTransformedPicture: Picture,
        resultPicture: Picture
    ): Picture =

      if replicasLeft == 0 then return resultPicture

      val transformed = transformer(previousTransformedPicture)

      replicate(
        replicasLeft - 1,
        transformed,
        resultPicture.addTo(side, transformed, paddingInPixels, alignment)
      )
    end replicate

    replicate(
      numberOfReplicas,
      previousTransformedPicture = this.asPicture,
      resultPicture = this.asPicture
    )
  end replicateTo

  def moveTo(
      x: Double,
      y: Double,
      positionType: PositionType
  ): TransformableType =
    positionType match
      case PositionType.Center =>
        moveCenterTo(x, y)

      case PositionType.UpperLeftCorner =>
        moveUpperLeftCornerTo(x, y)

  def moveCenterTo(x: Double, y: Double): TransformableType =
    moveBy(x - boundary.center.x, y - boundary.center.y)

  def moveCenterTo(position: Pos): TransformableType =
    moveCenterTo(position.x, position.y)

  def moveUpperLeftCornerTo(x: Double, y: Double): TransformableType =
    moveBy(x - boundary.upperLeftCorner.x, y - boundary.upperLeftCorner.y)

  def moveUpperLeftCornerTo(position: Pos): TransformableType =
    moveUpperLeftCornerTo(position.x, position.y)

  def moveBy(xOffset: Double, yOffset: Double): TransformableType =
    val newPos = position + (xOffset, yOffset)
    copy(newPosition = newPos)

  // Scale using both target width and target height
  // -------------------------------------------------------------------------------------------- \\

  /** Scales this object to given width and height in relation to its center.
    *
    * @param targetWidth
    * @param targetHeight
    * @return
    */
  def scaleTo(targetWidth: Double, targetHeight: Double): TransformableType =
    val horizontalFactor = targetWidth / boundary.width.inPixels
    val verticalFactor   = targetHeight / boundary.height.inPixels
    copy(transformationMatrix.scale(horizontalFactor, verticalFactor))

  /** Scales this object to given width and height in relation to a given point.
    *
    * @param targetWidth
    * @param targetHeight
    * @param relativityPoint
    * @return
    */
  def scaleTo(
      targetWidth: Double,
      targetHeight: Double,
      relativityPoint: Pos
  ): TransformableType =
    val horizontalFactor = targetWidth / boundary.width.inPixels
    val verticalFactor   = targetHeight / boundary.height.inPixels

    scaleBy(
      horizontalFactor = horizontalFactor,
      verticalFactor = verticalFactor,
      relativityPoint = relativityPoint
    )

  //  // Scale using a given factor for both horizontal and vertical directions
  //  // -------------------------------------------------------------------------------------------- \\
  //

  /** Scales this object in relation to its center by using a given factor for both horizontal and
    * vertical directions.
    *
    * @param factor
    * @return
    */
  def scaleBy(factor: Double): TransformableType =
    scaleBy(factor, factor)

  /** Scales this object in relation to a given point by using a given factor for both horizontal
    * and vertical directions.
    *
    * @param factor
    * @param relativityPoint
    * @return
    */
  def scaleBy(factor: Double, relativityPoint: Pos): TransformableType =
    scaleBy(factor, factor, relativityPoint)
  //
  //  /** Scales this object in relation to the origo by using a given factor for both horizontal and
  //    * vertical directions.
  //    *
  //    * @param factor
  //    * @return
  //    */
  //  def scaleByRelativeToOrigo(factor: Double): PictureElement

  // Scale using both horizontal and vertical factors
  // -------------------------------------------------------------------------------------------- \\

  /** Scales this object by given horizontal and vertical factors in relation to its center.
    *
    * @param horizontalFactor
    * @param verticalFactor
    * @return
    */
  def scaleBy(horizontalFactor: Double, verticalFactor: Double): TransformableType = copy(
    transformationMatrix.scale(horizontalFactor, verticalFactor)
  )

  /** Scales this object by given horizontal and vertical factors in relation to a given point.
    *
    * @param horizontalFactor
    * @param verticalFactor
    * @param relativityPoint
    * @return
    */
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): TransformableType =
    copy(transformationMatrix.scale(horizontalFactor, verticalFactor, relativityPoint))

  // Cropping
  // -------------------------------------------------------------------------------------------- \\

  inline final def crop(boundary: Bounds): Bitmap =
    crop(boundary.upperLeftCorner, boundary.lowerRightCorner)

  inline final def crop(upperLeftCorner: Pos, lowerRightCorner: Pos): Bitmap =
    crop(upperLeftCorner.x, upperLeftCorner.y, lowerRightCorner.x, lowerRightCorner.y)

  inline final def crop(
      upperLeftCorner: Pos,
      widthInPixels: Double,
      heightInPixels: Double
  ): Bitmap =
    crop(
      upperLeftCorner.x,
      upperLeftCorner.y,
      upperLeftCorner.x + widthInPixels,
      upperLeftCorner.y + heightInPixels
    )

  /** Crops this object to a given width and height.
    *
    * @param upperLeftX
    *   The x-coordinate of the upper left corner of the crop area in pixels
    * @param upperLeftY
    *   The y-coordinate of the upper left corner of the crop area in pixels
    * @param lowerRightX
    *   The x-coordinate of the lower right corner of the crop area in pixels
    * @param lowerRightY
    *   The y-coordinate of the lower right corner of the crop area in pixels
    * @return
    *   A new [[Bitmap]] object
    */
  def crop(
      upperLeftX: Double,
      upperLeftY: Double,
      lowerRightX: Double,
      lowerRightY: Double
  ): Bitmap =
    toBitmap.crop(upperLeftX, upperLeftY, lowerRightX, lowerRightY)

  // Rotating
  // -------------------------------------------------------------------------------------------- \\

  /** Rotates this object around a given point by the specified number of degrees.
    *
    * @param angle
    *   in degrees
    * @param centerOfRotation
    *   the point around which to rotate
    * @return
    */
  def rotateBy(angle: Double, centerOfRotation: Pos): TransformableType =
    copy(
      transformationMatrix.rotate(-angle, centerOfRotation)
    )

  /** Rotates this object around a given point by 90 degrees clockwise.
    */
  def rotateBy90DegreesCW(centerOfRotation: Pos): TransformableType =
    rotateBy(90.0, centerOfRotation)

  /** Rotates this object around a given point by 90 degrees counterclockwise.
    */
  def rotateBy90DegreesCCW(centerOfRotation: Pos): TransformableType =
    rotateBy(-90.0, centerOfRotation)

  /** Rotates this object around its center by the specified number of degrees.
    *
    * @param angle
    *   in degrees
    * @return
    */
  def rotateByAroundOrigo(angle: Double): TransformableType = copy(
    transformationMatrix.rotate(-angle, boundary.center)
  )

end Transformable
