package smile.infrastructure

/** Buffers kept between renderings, so that a scene with masks or turned clips does not allocate on
  * every frame. A buffer is at least as large as the layer drawn into it, and a layer uses its
  * upper left part.
  *
  * A layer takes the smallest kept buffer it fits in. The buffers kept have at most `budget` pixels
  * in all, and the ones released longest ago go first. A buffer that is acquired is not kept until
  * it is released, so no two threads draw into one at once.
  */
private[infrastructure] abstract class BufferPool[A <: AnyRef](budget: Long):
  private var free: List[A] = Nil

  protected def create(width: Int, height: Int): A
  protected def widthOf(buffer: A): Int
  protected def heightOf(buffer: A): Int

  private def size(buffer: A): Long = widthOf(buffer).toLong * heightOf(buffer)

  def acquire(width: Int, height: Int): A = synchronized:
    val fitting = free.filter(buffer => widthOf(buffer) >= width && heightOf(buffer) >= height)
    if fitting.isEmpty then create(width, height)
    else
      val best = fitting.minBy(size)
      free = free.filterNot(_ eq best)
      best

  def release(buffer: A): Unit = synchronized:
    if size(buffer) <= budget then
      // The newest first, and the oldest ones dropped while they are over the budget.
      val kept = (buffer :: free).scanLeft((Option.empty[A], 0L)):
        case ((_, total), next) => (Some(next), total + size(next))
      free = kept.drop(1).takeWhile(_._2 <= budget).flatMap(_._1)

  /** How many buffers are kept, which tests read. */
  def keptCount: Int = synchronized(free.size)

  /** How many pixels the largest kept buffer has, or 0 when none is kept, which tests read. */
  def largestKept: Long = synchronized(free.map(size).maxOption.getOrElse(0L))

private[infrastructure] object BufferPool:
  /** The most pixels a pool keeps in its buffers. That is 64 MB, enough for a few masks over a 3840
    * x 2160 window.
    */
  val Budget: Long = 64L * 1024 * 1024 / 4
