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

package smcl.infrastructure.enumerators


import smcl.infrastructure.exceptions.NoMoreCellsToEnumerateError




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object DownwardsLeftwardsMatrixEnumerator
    extends MatrixEnumerator2DCompanion {

  /**
   *
   *
   * @param upperLeftColumn
   * @param upperLeftRow
   * @param lowerRightColumn
   * @param lowerRightRow
   *
   * @return
   */
  def instantiateEnumerator(
      upperLeftColumn: Int,
      upperLeftRow: Int,
      lowerRightColumn: Int,
      lowerRightRow: Int): AbstractMatrixEnumerator2D = {

    new DownwardsLeftwardsMatrixEnumerator(
      upperLeftColumn,
      upperLeftRow,
      lowerRightColumn,
      lowerRightRow)
  }

}




/**
 *
 *
 * @param upperLeftColumn
 * @param upperLeftRow
 * @param lowerRightColumn
 * @param lowerRightRow
 *
 * @author Aleksi Lukkarinen
 */
class DownwardsLeftwardsMatrixEnumerator private(
    override val upperLeftColumn: Int,
    override val upperLeftRow: Int,
    override val lowerRightColumn: Int,
    override val lowerRightRow: Int)
    extends AbstractMatrixEnumerator2D(
      upperLeftColumn, upperLeftRow, lowerRightColumn, lowerRightRow) {

  /**
   *
   *
   * @return
   */
  //noinspection ConvertExpressionToSAM
  override protected
  def enumerationState: MatrixEnumerator2DInternalEnumerationState =
    new MatrixEnumerator2DInternalEnumerationState {

      _currentColumn = lowerRightColumn
      _currentRow = upperLeftRow
      _rowHasChanged = true
      _columnHasChanged = false

      /**
       *
       *
       * @return
       */
      def hasNextCell: Boolean =
        currentColumn > upperLeftColumn || currentRow < lowerRightRow

      /**
       *
       *
       * @return
       */
      override
      def advance(): Unit = {
        if (!hasNextCell)
          throw NoMoreCellsToEnumerateError

        if (_currentRow < lowerRightRow) {
          _currentRow += 1
          _columnHasChanged = false
        }
        else {
          _currentRow = upperLeftRow
          _currentColumn -= 1
          _columnHasChanged = true
        }
      }
    }

}
