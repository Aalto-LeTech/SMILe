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


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
case class PixelSnapshotLeftwardsUpwardsIterator(
    relatedPixelSnapshot: PixelSnapshot)
    extends AbstractPixelSnapshotIterator {

  /** */
  private
  var _currentXInPixels: Int = maxXInPixels

  /** */
  private
  var _currentYInPixels: Int = maxYInPixels

  /** */
  private
  var _rowHasChanged: Boolean = false

  /**
   *
   *
   * @return
   */
  def currentXInPixels: Int = _currentXInPixels

  /**
   *
   *
   * @return
   */
  def currentYInPixels: Int = _currentYInPixels

  /**
   *
   *
   * @return
   */
  def rowHasChanged: Boolean = hasNext && _rowHasChanged

  /**
   *
   *
   * @return
   */
  def columnHasChanged: Boolean = true

  /**
   *
   */
  private
  def advance(): Unit = {
    if (_currentXInPixels > minXInPixels) {
      _currentXInPixels -= 1
      _rowHasChanged = false
    }
    else {
      _currentXInPixels = maxXInPixels
      _currentYInPixels -= 1
      _rowHasChanged = true
    }
  }

  /**
   *
   *
   * @return
   */
  def hasNext: Boolean =
    _currentYInPixels >= minYInPixels &&
        _currentXInPixels >= minXInPixels

  /**
   *
   *
   * @return
   */
  def next(): Pixel = {
    if (!hasNext)
      return Iterator.empty.next()

    val nextResult = Pixel(
      relatedPixelSnapshot,
      minXInPixels, maxXInPixels,
      minYInPixels, maxYInPixels,
      _currentXInPixels, _currentYInPixels)

    advance()

    nextResult
  }

}
