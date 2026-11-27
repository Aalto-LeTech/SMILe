package smile.infrastructure

/** The buffer type of this platform, a `smile.infrastructure.JVMBufferAdapter` on the JVM. It is a
  * type alias because shared code compiles once per platform, so naming it there gives that code
  * the concrete buffer and spares it every downcast.
  */
private[smile] type NativeBuffer = JVMBufferAdapter

/** A buffer that `smile.infrastructure.RasterCache` keeps, behind a soft reference, so that the
  * collector can take it before running out of memory.
  */
private[smile] final class KeptBuffer(buffer: NativeBuffer):
  private val reference = new java.lang.ref.SoftReference(buffer)

  /** The buffer, unless the collector has cleared it. */
  def get: Option[NativeBuffer] = Option(reference.get)
