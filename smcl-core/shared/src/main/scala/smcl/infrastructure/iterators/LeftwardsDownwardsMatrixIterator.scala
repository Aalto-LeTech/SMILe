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

package smcl.infrastructure.iterators


import smcl.infrastructure.enumerators.{AbstractMatrixEnumerator2D, LeftwardsDownwardsMatrixEnumerator}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object LeftwardsDownwardsMatrixIterator {

  /**
   *
   *
   * @param startColumn
   * @param startRow
   * @param width
   * @param height
   *
   * @return
   */
  def apply(
      startColumn: Int,
      startRow: Int,
      width: Int,
      height: Int): MatrixIterator2D = {

    val enumerator =
      LeftwardsDownwardsMatrixEnumerator(
        startColumn, startRow, width, height)

    new LeftwardsDownwardsMatrixIterator(enumerator)
  }

}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class LeftwardsDownwardsMatrixIterator private(
    enumerator: AbstractMatrixEnumerator2D)
    extends MatrixIterator2D(enumerator) {

}
