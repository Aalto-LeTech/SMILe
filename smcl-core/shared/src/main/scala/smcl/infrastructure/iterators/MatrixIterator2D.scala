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


import scala.collection.AbstractIterator

import smcl.infrastructure.enumerators.AbstractMatrixEnumerator2D




/**
 *
 *
 * @param enumerator
 *
 * @author Aleksi Lukkarinen
 */
class MatrixIterator2D(
    private val enumerator: AbstractMatrixEnumerator2D)
    extends AbstractIterator[(Int, Int)] {

  /** */
  val width: Int = enumerator.width

  /** */
  val height: Int = enumerator.height

  /** */
  val upperLeftColumn: Int = enumerator.upperLeftColumn

  /** */
  val lowerRightColumn: Int = enumerator.lowerRightColumn

  /** */
  val upperLeftRow: Int = enumerator.upperLeftRow

  /** */
  val lowerRightRow: Int = enumerator.lowerRightRow

  /**
   *
   *
   * @return
   */
  @inline
  def currentColumn: Int = enumerator.currentColumn

  /**
   *
   *
   * @return
   */
  @inline
  def currentRow: Int = enumerator.currentRow

  /**
   *
   *
   * @return
   */
  @inline
  def rowHasChanged: Boolean = enumerator.rowHasChanged

  /**
   *
   *
   * @return
   */
  @inline
  def columnHasChanged: Boolean = enumerator.columnHasChanged

  /**
   *
   *
   * @return
   */
  @inline
  override
  def hasNext: Boolean = enumerator.hasNextCell

  /**
   *
   *
   * @return
   */
  @inline
  def hasNoMoreCells: Boolean = enumerator.hasNoMoreCells

  /**
   *
   *
   * @return
   */
  @inline
  def next: (Int, Int) = {
    enumerator.advance()
    enumerator.colRowTuple
  }

}
