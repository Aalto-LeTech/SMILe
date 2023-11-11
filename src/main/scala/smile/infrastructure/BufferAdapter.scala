package smile.infrastructure

import smile.Settings
import smile.Settings.{CanvasesAreResizedBasedOnTransformations, DefaultBackgroundColor}
import smile.colors.Color
import smile.modeling.{AffineTransformation, Pos}

import java.awt.*
import java.awt.geom.{AffineTransform, Rectangle2D}
import java.awt.image.{AffineTransformOp, BufferedImage}
import javax.swing.Icon

class BufferAdapter(private val buffer: BufferedImage):
  def this(width: Int, height: Int) =
    this(new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB))

  lazy val width: Int  = buffer.getWidth
  lazy val height: Int = buffer.getHeight

  val ScalingMethod: Int   = Image.SCALE_AREA_AVERAGING
  val TransformMethod: Int = AffineTransformOp.TYPE_BILINEAR

  def deepCopy: BufferAdapter =
    val newBuffer = new BufferAdapter(width, height)
    newBuffer.withGraphics2D(g => g.drawImage(buffer, 0, 0, null))
    newBuffer

  def get: BufferedImage = buffer

  def graphics: Graphics = buffer.getGraphics

  def toSwingIcon: Icon = new Icon:
    def paintIcon(target: Component, graphics: Graphics, x: Int, y: Int): Unit =
      graphics.drawImage(buffer, x, y, target)
    def getIconWidth: Int  = width
    def getIconHeight: Int = height
  end toSwingIcon

  def pixelColor(x: Int, y: Int): Color =
    val rgba = buffer.getRGB(x, y)
    new Color(
      red = rgba >> 16 & 0xff,
      green = rgba >> 8 & 0xff,
      blue = rgba & 0xff,
      opacity = rgba >> 24 & 0xff
    )

  def scaleTo(targetWidth: Double, targetHeight: Double, newCenter: Pos): BufferAdapter =
    val newWidth  = targetWidth.toInt.abs
    val newHeight = targetHeight.toInt.abs

    val image = buffer.getScaledInstance(newWidth, newHeight, ScalingMethod)

    val newBuffer = new BufferAdapter(newWidth, newHeight)

    val g = newBuffer.buffer.createGraphics()

    // Flip the image if necessary.
    if targetWidth < 0 || targetHeight < 0 then
      val tx = AffineTransform.getScaleInstance(
        if targetWidth < 0 then -1 else 1,
        if targetHeight < 0 then -1 else 1
      )
      if targetWidth < 0 then tx.translate(-newWidth, 0)
      if targetHeight < 0 then tx.translate(0, -newHeight)
      g.setTransform(tx)

    g.drawImage(image, 0, 0, null)
    g.dispose()

    newBuffer
  end scaleTo

  def setColorsFromSeq(
      colors: Seq[Color]
  ): Unit =
    def colorArray = new Array[Int](colors.length)
    val reds       = colorArray
    val greens     = colorArray
    val blues      = colorArray
    val opacities  = colorArray

    for (c, i) <- colors.zipWithIndex do
      if c != null then
        reds(i) = c.red
        greens(i) = c.green
        blues(i) = c.blue
        opacities(i) = c.opacity

    val raster     = buffer.getRaster
    val setSamples = raster.setSamples(0, 0, width, height, _: Int, _: Array[Int])

    setSamples(0, reds)
    setSamples(1, greens)
    setSamples(2, blues)
    setSamples(3, opacities)

    buffer.setData(raster)
  end setColorsFromSeq

  def copyPortionXYXY(
      topLeftXInPixels: Double,
      topLeftYInPixels: Double,
      bottomRightXInPixels: Double,
      bottomRightYInPixels: Double
  ): BufferAdapter =
    val (x0, x1) =
      if topLeftXInPixels > bottomRightXInPixels then (bottomRightXInPixels, topLeftXInPixels)
      else (topLeftXInPixels, bottomRightXInPixels)

    val (y0, y1) =
      if topLeftYInPixels > bottomRightYInPixels then (bottomRightYInPixels, topLeftYInPixels)
      else (topLeftYInPixels, bottomRightYInPixels)

    val width  = x1 - x0
    val height = y1 - y0

    copyPortionXYWH(topLeftXInPixels, topLeftYInPixels, width, height)

  def copyPortionXYWH(
      topLeftXInPixels: Double,
      topLeftYInPixels: Double,
      widthInPixels: Double,
      heightInPixels: Double
  ): BufferAdapter =

    val flooredWidth: Int  = widthInPixels.floor.toInt
    val flooredHeight: Int = heightInPixels.floor.toInt

    val sourceBufferArea =
      buffer.getSubimage(
        topLeftXInPixels.floor.toInt,
        topLeftYInPixels.floor.toInt,
        flooredWidth,
        flooredHeight
      )

    val initializer = (g: Graphics2D) => g.drawImage(sourceBufferArea, null, 0, 0)

    val newBuffer = createNormalizedLowLevelBitmapBufferOf(
      flooredWidth,
      flooredHeight,
      Some(initializer)
    )

    newBuffer

  private def createNormalizedLowLevelBitmapBufferOf(
      width: Int,
      height: Int,
      initializer: Option[Graphics2D => Unit]
  ): BufferAdapter =

    val newBuffer = new BufferAdapter(
      width,
      height
    )

    newBuffer.withGraphics2D(g =>
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC))
      g.setColor(DefaultBackgroundColor.toAWTColor)
      g.fillRect(0, 0, width, height)

      initializer.foreach(i => i(g))
    )

    newBuffer
  end createNormalizedLowLevelBitmapBufferOf

  def withGraphics2D[ResultType](
      workUnit: Graphics2D => ResultType
  ): ResultType =

    var gr2D: Graphics2D              = null
    var memorizedThrowable: Throwable = null

    try
      gr2D = buffer.createGraphics()

      setDefaultGraphics2DProperties(gr2D)
      workUnit(gr2D)
    catch
      case caughtThrowable: Throwable =>
        memorizedThrowable = caughtThrowable
        throw caughtThrowable
    finally
      if gr2D != null then
        if memorizedThrowable != null then
          try gr2D.dispose()
          catch
            case caughtThrowable: Throwable =>
              memorizedThrowable.addSuppressed(caughtThrowable)
        else gr2D.dispose()

  private inline final def setDefaultGraphics2DProperties(g: Graphics2D): Unit =
    val antialiasingState =
      if Settings.DrawingIsAntiAliased then RenderingHints.VALUE_ANTIALIAS_ON
      else RenderingHints.VALUE_ANTIALIAS_OFF

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, antialiasingState)

    val textAntialiasingState =
      if Settings.DrawingIsAntiAliased then RenderingHints.VALUE_TEXT_ANTIALIAS_ON
      else RenderingHints.VALUE_TEXT_ANTIALIAS_OFF

    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, textAntialiasingState)
  end setDefaultGraphics2DProperties

  def iterateLocations(callback: (Int, Int) => Unit): Unit =
    for
      x <- 0 until width
      y <- 0 until height
    do callback(x, y)

  def createTransformedVersionWith(
      transformation: AffineTransformation,
      resizeCanvasBasedOnTransformation: Boolean = CanvasesAreResizedBasedOnTransformations,
      backgroundColor: Color = DefaultBackgroundColor
  ): BufferAdapter =

    val globalInterpolationMethod = TransformMethod

    val lowLevelTransformation = transformation.toAWTAffineTransform
    val transformedContentBoundaries: Rectangle2D =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)
        .getBounds2D(buffer)

    val (offsetLeft, offsetTop, offsetRight, offsetBottom) =
      if !resizeCanvasBasedOnTransformation then (0.0, 0.0, 0.0, 0.0)
      else
        (
          -transformedContentBoundaries.getMinX,
          -transformedContentBoundaries.getMinY,
          transformedContentBoundaries.getMaxX - width,
          transformedContentBoundaries.getMaxY - height
        )

    val (resultingImageWidth, resultingImageHeight) =
      if !resizeCanvasBasedOnTransformation then (width, height)
      else
        (
          Math.floor(width.toDouble + offsetLeft + offsetRight).toInt,
          Math.floor(height.toDouble + offsetTop + offsetBottom).toInt
        )

    if resizeCanvasBasedOnTransformation then
      if offsetTop > 0 || offsetLeft > 0 then
        val translationToBringTheRotatedBitmapFullyVisible =
          AffineTransform.getTranslateInstance(offsetLeft, offsetTop)

        lowLevelTransformation preConcatenate translationToBringTheRotatedBitmapFullyVisible

    val finalTransformOperation =
      new AffineTransformOp(lowLevelTransformation, globalInterpolationMethod)
    val resultingBuffer = BufferAdapter(resultingImageWidth, resultingImageHeight)

    new DrawingSurface(resultingBuffer) clearUsing (backgroundColor, true)

    finalTransformOperation.filter(buffer, resultingBuffer.get)

    resultingBuffer
  end createTransformedVersionWith

  def scaleToHeight(newHeight: Int): BufferAdapter =
    val scale         = newHeight.toDouble / height
    val newWidth      = (width * scale).toInt * 3
    val scaledImage   = new BufferAdapter(newWidth, newHeight)
    val scaleInstance = AffineTransform.getScaleInstance(scale * 3, scale)
    val scaleOp       = new AffineTransformOp(scaleInstance, AffineTransformOp.TYPE_BICUBIC)
    scaleOp.filter(buffer, scaledImage.buffer)
    scaledImage
  end scaleToHeight
