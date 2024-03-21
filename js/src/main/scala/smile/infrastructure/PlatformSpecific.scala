package smile.infrastructure

import smile.modeling.{Bounds, JSBoundaryCalculator}
import smile.pictures.Text

object PlatformSpecific extends PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): BufferAdapter[?] =
    new JSBufferAdapter(width, height)

  val resourceFactory: ResourceFactory[?] = JSResourceFactory

  val renderer: Renderer = JSRenderer

  def textBoundaryCalculator(text: Text): Bounds = JSBoundaryCalculator.fromText(text)
