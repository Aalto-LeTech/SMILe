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

package smcl.pictures.fullfeatured


import smcl.infrastructure.{DrawingSurfaceAdapter, FlatMap, Identity}
import smcl.modeling.d2._




/**
 *
 */
object Image {

  /**
   *
   *
   * @param elements
   *
   * @return
   */
  def apply(elements: ImageElement*): Image = {
    val identity: Identity = Identity()

    new Image(identity, elements, Anchor.Center)
  }

  /**
   *
   *
   * @param elements
   *
   * @return
   */
  def apply(
      elements: Seq[ImageElement],
      anchor: Anchor[HasAnchor]): Image = {

    val identity: Identity = Identity()

    new Image(identity, elements, anchor)
  }

}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class Image private(
    override val identity: Identity,
    val elements: Seq[ImageElement],
    val anchor: Anchor[HasAnchor])
    extends ImageElement
        with HasAnchor
        with FlatMap[Image, Seq[ImageElement]] {

  // TODO: Tarkistukset

  /** */
  val boundary: Bounds =
    BoundaryCalculator.fromBoundaries(elements)

  /** Dimensions of this object. */
  override
  def dimensions: Dims = Dims(boundary.width, boundary.height)

  /** Position of this object. */
  override
  def position: Pos = boundary.upperLeftMarker

  /** */
  val isRenderable: Boolean = width > 0 && height > 0

  /**
   *
   *
   * @param drawingSurface
   * @param offsetsToOrigo
   */
  @inline
  override
  def renderOn(
      drawingSurface: DrawingSurfaceAdapter,
      offsetsToOrigo: Dims): Unit = {

    if (boundary.isEmpty)
      return

    elements.foreach{e =>
      e.renderOn(drawingSurface, offsetsToOrigo)
    }
  }

  /**
   *
   *
   * @return
   */
  @inline
  override
  def toBitmap: Bmp = Bmp(elements: _*)

  /**
   *
   *
   * @param f
   *
   * @return
   */
  @inline
  def map(f: (ImageElement) => ImageElement): Image = {
    Image(elements.map(f): _*)
  }

  /**
   *
   * @param f
   *
   * @return
   */
  @inline
  override
  def flatMap(f: (Seq[ImageElement]) => Image): Image = {
    f(elements)
  }

  /**
   *
   *
   * @param newElements
   *
   * @return
   */
  @inline
  def copy(
      newElements: Seq[ImageElement] = elements,
      newAnchor: Anchor[HasAnchor]): Image = {

    new Image(identity, newElements, anchor)
  }

  /**
   *
   *
   * @param offsets
   *
   * @return
   */
  @inline
  override
  def moveBy(offsets: Double*): Image = {
    map{_.moveBy(offsets: _*)}
  }

  /**
   *
   */
  @inline
  override
  def display(): Image = {
    super.display()

    this
  }

  /**
   * Rotates this object around the origo (0,0) by 90 degrees clockwise.
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCW: Image = {
    map{_.rotateBy90DegsCW}
  }

  /**
   * Rotates this object around a given point by 90 degrees clockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCW(centerOfRotation: Pos): Image = {
    map{_.rotateBy90DegsCW(centerOfRotation)}
  }

  /**
   * Rotates this object around the origo (0,0) by 90 degrees counterclockwise.
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCCW: Image = {
    map{_.rotateBy90DegsCCW}
  }

  /**
   * Rotates this object around a given point by 90 degrees counterclockwise.
   *
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy90DegsCCW(centerOfRotation: Pos): Image = {
    map{_.rotateBy90DegsCCW(centerOfRotation)}
  }

  /**
   * Rotates this object around the origo (0,0) by 180 degrees.
   *
   * @return
   */
  @inline
  override
  def rotateBy180Degs: Image = {
    map{_.rotateBy180Degs}
  }

  /**
   * Rotates this object around a given point by 180 degrees.
   *
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy180Degs(centerOfRotation: Pos): Image = {
    map{_.rotateBy180Degs(centerOfRotation)}
  }

  /**
   * Rotates this object around the origo (0,0) by the specified number of degrees.
   *
   * @param angleInDegrees
   *
   * @return
   */
  @inline
  override
  def rotateBy(angleInDegrees: Double): Image = {
    map{_.rotateBy(angleInDegrees)}
  }

  /**
   * Rotates this object around a given point by the specified number of degrees.
   *
   * @param angleInDegrees
   * @param centerOfRotation
   *
   * @return
   */
  @inline
  override
  def rotateBy(
      angleInDegrees: Double,
      centerOfRotation: Pos): Image = {

    map{_.rotateBy(angleInDegrees, centerOfRotation)}
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
  def scaleBy(widthFactor: Double, heightFactor: Double): Image = {
    this
  }

}
