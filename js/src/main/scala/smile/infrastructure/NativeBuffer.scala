package smile.infrastructure

/** The buffer type of this platform, a `smile.infrastructure.JSBufferAdapter` in Scala.js. It is a
  * type alias because shared code compiles once per platform, so naming it there gives that code
  * the concrete buffer and spares it every downcast.
  */
private[smile] type NativeBuffer = JSBufferAdapter

/** A buffer that `smile.infrastructure.RasterCache` keeps. A browser gives no soft references, so
  * it is held as it is, and the cache's budget alone bounds what is kept.
  *
  * The cache counts 4 bytes a pixel, those of the canvas that each rendering it keeps is drawn on.
  * What a rendering makes of those pixels once and then keeps with them, the budget, set for what a
  * browser lets its canvases take, does not count: the array its pixels are read into, its canvas
  * with a border of one pixel (`smile.infrastructure.JSBufferAdapter.paddedCanvas`), and the PNG
  * data URLs of both canvases that SVG output embeds.
  */
private[smile] final class KeptBuffer(buffer: NativeBuffer):
  /** The buffer, which is always there. */
  def get: Option[NativeBuffer] = Some(buffer)
