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

package smcl.infrastructure


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object DoneStatus {

  /** */
  lazy val Done: DoneStatus = apply().setDone()

  /**
   *
   *
   * @return
   */
  @inline
  def apply(): DoneStatus = new DoneStatus()

}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class DoneStatus private() {

  /** */
  private
  var hasNotBeenDone = true

  /**
   *
   */
  def setDone(): DoneStatus = {
    if (hasNotBeenDone) {
      synchronized{
        if (hasNotBeenDone) {
          hasNotBeenDone = false
        }
      }
    }

    this
  }

  /**
   *
   *
   * @return
   */
  @inline
  def isNotDone: Boolean = hasNotBeenDone

  /**
   *
   *
   * @return
   */
  @inline
  def isDone: Boolean = !hasNotBeenDone

}
