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

package smcl.modeling.d2


import scala.util.Random

import smcl.infrastructure.MathUtils
import smcl.modeling._
import smcl.modeling.misc.CoordSysIndepBoundary




/**
 * Companion object for the [[Bounds]] class.
 *
 * @author Aleksi Lukkarinen
 */
object Bounds {

  /** */
  val NumberOfCorners: Int = 4

  /** */
  val ZeroAreaAtOrigo: Bounds =
    createInstance(Pos.Origo, Pos.Origo, isDefined = false)

  /** */
  val NotDefined: Bounds =
    createInstance(Pos.Origo, Pos.Origo, isDefined = false)

  /**
   * Creates a new [[Bounds]] instance.
   *
   * @param upperLeftMarker
   * @param lowerRightMarker
   * @param isDefined
   *
   * @return
   */
  @inline
  private
  def createInstance(
      upperLeftMarker: Pos,
      lowerRightMarker: Pos,
      isDefined: Boolean): Bounds = {

    new Bounds(upperLeftMarker, lowerRightMarker, isDefined)
  }

  /**
   * Creates a new [[Bounds]] instance.
   *
   * @param upperLeftXInPixels
   * @param upperLeftYInPixels
   * @param lowerRightXInPixels
   * @param lowerRightYInPixels
   *
   * @return
   */
  @inline
  def apply(
      upperLeftXInPixels: Double,
      upperLeftYInPixels: Double,
      lowerRightXInPixels: Double,
      lowerRightYInPixels: Double): Bounds = {

    val (x0, x1) = MathUtils.sort(upperLeftXInPixels, lowerRightXInPixels)
    val (y0, y1) = MathUtils.sort(upperLeftYInPixels, lowerRightYInPixels)

    createInstance(Pos(x0, y0), Pos(x1, y1), isDefined = true)
  }

  /**
   * Creates a new [[Bounds]] instance.
   *
   * @param position
   *
   * @return
   */
  @inline
  def apply(position: Pos): Bounds = {
    createInstance(position, position, isDefined = true)
  }

  /**
   * Creates a new [[Bounds]] instance.
   *
   * @param upperLeft
   * @param lowerRight
   *
   * @return
   */
  @inline
  def apply(
      upperLeft: Pos,
      lowerRight: Pos): Bounds = {

    apply(
      upperLeft.xInPixels, upperLeft.yInPixels,
      lowerRight.xInPixels, lowerRight.yInPixels)
  }

  /**
   * Creates a new [[Bounds]] instance.
   *
   * @param markers
   *
   * @return
   */
  @inline
  def apply(markers: Pos*): Bounds = {
    require(
      markers.length == 2,
      s"Exactly two marker positions must be given (currently: ${markers.length})")

    apply(markers: _*)
  }




  /**
   *
   */
  object getWidth {

    /**
     *
     *
     * @param bounds
     *
     * @return
     */
    @inline
    def unapply(bounds: Bounds): Option[Len] = {
      Some(bounds.width)
    }

  }




  /**
   *
   */
  object getHeight {

    /**
     *
     *
     * @param bounds
     *
     * @return
     */
    @inline
    def unapply(bounds: Bounds): Option[Len] = {
      Some(bounds.height)
    }

  }




  /**
   *
   */
  object getLength {

    /**
     *
     *
     * @param bounds
     *
     * @return
     */
    @inline
    def unapply(bounds: Bounds): Option[Len] = {
      Some(bounds.length)
    }

  }




  /**
   *
   */
  object extractArea {

    /**
     *
     *
     * @param bounds
     *
     * @return
     */
    @inline
    def unapply(bounds: Bounds): Option[Area] = {
      Some(bounds.area)
    }

  }




}




/**
 * Rectangular boundary in two-dimensional Cartesian coordinate system.
 *
 * @param upperLeftMarker
 * @param lowerRightMarker
 * @param isDefined
 *
 * @author Aleksi Lukkarinen
 */
case class Bounds private(
    upperLeftMarker: Pos,
    lowerRightMarker: Pos,
    isDefined: Boolean)
    extends CoordSysIndepBoundary[Pos]
        with HasArea {

  val markers: Seq[Pos] = Seq(upperLeftMarker, lowerRightMarker)

  /** Position of this boundary. */
  @inline
  def position: Pos = upperLeftMarker

  /** */
  lazy val width: Len =
    Len(lowerRightMarker.xInPixels - upperLeftMarker.xInPixels + 1)

  /** */
  lazy val height: Len =
    Len(lowerRightMarker.yInPixels - upperLeftMarker.yInPixels + 1)

  /** */
  lazy val length: Len =
    Len(2 * width.inPixels +
        2 * height.inPixels -
        Bounds.NumberOfCorners)

  /** */
  lazy val area: Area =
    Area.forRectangle(width.inPixels, height.inPixels)

  /**
   *
   *
   * @param other
   *
   * @return
   */
  @inline
  override
  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[Bounds]
  }

  /**
   *
   *
   * @return
   */
  @inline
  def randomPosInside: Pos = {
    val offsetX = Random.nextDouble() * width.inPixels
    val offsetY = Random.nextDouble() * height.inPixels

    upperLeftMarker + (offsetX, offsetY)
  }

  /**
   *
   *
   * @param newUpperLeftMarker
   * @param newLowerRightMarker
   *
   * @return
   */
  @inline
  def copy(
      newUpperLeftMarker: Pos = upperLeftMarker,
      newLowerRightMarker: Pos = lowerRightMarker): Bounds = {

    Bounds(newUpperLeftMarker, newLowerRightMarker)
  }

}
