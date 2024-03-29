package smile.infrastructure

import smile.modeling.{Bounds, JVMBoundaryCalculator}
import smile.pictures.Text

import java.awt.image.BufferedImage

object PlatformSpecific extends PlatformClassesTrait:
  def bufferAdapter(width: Int, height: Int): BufferAdapter[BufferedImage] =
    new JVMBufferAdapter(width, height)

  val resourceFactory: ResourceFactory[?] = JVMResourceFactory

  val renderer: Renderer = JVMRenderer

  def textBoundaryCalculator(text: Text): Bounds = JVMBoundaryCalculator.fromText(text)
