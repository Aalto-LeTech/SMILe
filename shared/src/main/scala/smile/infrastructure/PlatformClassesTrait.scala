package smile.infrastructure

import smile.modeling.Bounds
import smile.pictures.Text

trait PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): BufferAdapter[?]

  val resourceFactory: ResourceFactory[?]

  val renderer: Renderer

  def textBoundaryCalculator(text: Text): Bounds
