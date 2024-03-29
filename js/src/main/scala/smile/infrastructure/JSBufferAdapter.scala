package smile.infrastructure

import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all.canvas
import smile.Settings
import smile.Settings.DefaultBackgroundColor
import smile.colors.Color
import smile.modeling.AffineTransformation

import scala.scalajs.js

object JSBufferAdapter:
  val Empty: JSBufferAdapter = JSBufferAdapter(0, 0)

/** Adapter for managing and manipulating an HTML Canvas. This class provides methods for common
  * image processing tasks such as copying, scaling, and transforming.
  */
class JSBufferAdapter(val width: Int, val height: Int) extends BufferAdapter[html.Canvas]:
  private val buffer: html.Canvas = canvas().render

  buffer.width = width
  buffer.height = height

  private val options = js.Dictionary("willReadFrequently" -> true)

  private[infrastructure] val ctx =
    buffer.getContext("2d", options).asInstanceOf[dom.CanvasRenderingContext2D]

  ctx.imageSmoothingEnabled = true

  def deepCopy: JSBufferAdapter =
    val newBuffer = JSBufferAdapter(width, height)
    val imageData = ctx.getImageData(0, 0, width, height)
    newBuffer.ctx.putImageData(imageData, 0, 0)
    newBuffer

  /** Provides access to the underlying `Canvas`.
    *
    * @return
    *   The underlying `Canvas`.
    */
  def get: html.Canvas = buffer

  def imageData: Seq[Color] =
    val data = ctx
      .getImageData(0, 0, width, height)
      .data
    (0 until width * height).map(i =>
      val index = i * 4
      Color(data(index), data(index + 1), data(index + 2), data(index + 3))
    )

  def pixelColor(x: Int, y: Int): Color =
    val argb = ctx.getImageData(x, y, 1, 1).data
    new Color(argb(0), argb(1), argb(2), argb(3))

  def setRGBA(x: Int, y: Int, color: Color): Unit =
    val imageData = ctx.createImageData(1, 1)
    imageData.data(0) = color.red
    imageData.data(1) = color.green
    imageData.data(2) = color.blue
    imageData.data(3) = color.opacity
    ctx.putImageData(imageData, x, y)

  def scaleTo(
      targetWidth: Double,
      targetHeight: Double
  ): JSBufferAdapter =
    val newWidth  = targetWidth.toInt.abs
    val newHeight = targetHeight.toInt.abs
    val newBuffer = JSBufferAdapter(newWidth, newHeight)
    val g         = newBuffer.ctx
    if Settings.BufferScalingMethod == Settings.ScalingMethod.NearestNeighbor then
      g.imageSmoothingEnabled = false
    else g.imageSmoothingEnabled = true
    g.scale(targetWidth / width, targetHeight / height)
    g.drawImage(buffer, 0, 0)
    g.scale(1, 1)
    newBuffer
  end scaleTo

  def setColorsFromSeq(colors: Seq[Color]): Unit =
    val imageData = ctx.createImageData(width, height)
    colors.zipWithIndex.foreach:
      case (color, index) =>
        val i = index * 4
        imageData.data(i) = color.red
        imageData.data(i + 1) = color.green
        imageData.data(i + 2) = color.blue
        imageData.data(i + 3) = color.opacity

    ctx.putImageData(imageData, 0, 0)
  end setColorsFromSeq

  def copyPortionXYWH(
      topLeftX: Double,
      topLeftY: Double,
      width: Double,
      height: Double
  ): JSBufferAdapter =
    if width <= 0 || height <= 0 then return JSBufferAdapter.Empty

    val flooredWidth: Int  = width.floor.toInt
    val flooredHeight: Int = height.floor.toInt

    val sourceBufferArea =
      ctx.getImageData(
        topLeftX.floor.toInt,
        topLeftY.floor.toInt,
        flooredWidth,
        flooredHeight
      )

    val newBuffer = JSBufferAdapter(flooredWidth, flooredHeight)
    newBuffer.ctx.putImageData(sourceBufferArea, 0, 0)

    newBuffer
  end copyPortionXYWH

  def createTransformedVersionWith(
      transformation: AffineTransformation,
      backgroundColor: Color = DefaultBackgroundColor
  ): JSBufferAdapter =
    def transformPoint(
        x: Double,
        y: Double
    ): (Double, Double) =
      val newX = transformation.alpha * x + transformation.gamma * y + transformation.tauX
      val newY = transformation.delta * x + transformation.beta * y + transformation.tauY
      (newX, newY)

    val corners = Seq(
      transformPoint(0, 0),
      transformPoint(width, 0),
      transformPoint(0, height),
      transformPoint(width, height)
    )

    val xs = corners.map(_._1)
    val ys = corners.map(_._2)

    val minX = xs.min
    val maxX = xs.max
    val minY = ys.min
    val maxY = ys.max

    val newWidth  = (maxX - minX).toInt
    val newHeight = (maxY - minY).toInt

    val offsetX = -minX
    val offsetY = -minY

    val newBuffer = JSBufferAdapter(newWidth, newHeight)
    val g         = newBuffer.ctx

    g.setTransform(
      transformation.alpha,
      transformation.delta,
      transformation.gamma,
      transformation.beta,
      offsetX + transformation.tauX,
      offsetY + transformation.tauY
    )

    g.drawImage(buffer, 0, 0)
    g.setTransform(1, 0, 0, 1, 0, 0)
    newBuffer
  end createTransformedVersionWith
end JSBufferAdapter
