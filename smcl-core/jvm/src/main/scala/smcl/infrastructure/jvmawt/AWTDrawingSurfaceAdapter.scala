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

package smcl.infrastructure.jvmawt


import java.awt.geom.{AffineTransform, Ellipse2D}
import java.awt.{AlphaComposite, BasicStroke, Graphics2D}

import smcl.colors.ColorValidator
import smcl.colors.rgb.Color
import smcl.infrastructure.{BitmapBufferAdapter, DoubleWrapper, DrawingSurfaceAdapter}
import smcl.modeling.AffineTransformation




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[smcl]
object AWTDrawingSurfaceAdapter {

  /**
   *
   *
   * @param owner
   *
   * @return
   */
  def apply(owner: AWTBitmapBufferAdapter): AWTDrawingSurfaceAdapter = {
    new AWTDrawingSurfaceAdapter(owner)
  }

}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[smcl]
class AWTDrawingSurfaceAdapter private(val owner: AWTBitmapBufferAdapter)
    extends DrawingSurfaceAdapter {

  private val HairlineStroke = new BasicStroke(0)

  /**
   *
   *
   * @param color
   */
  override
  def clearUsing(
      color: Color,
      useSourceColorLiterally: Boolean): Unit = {

    owner.withGraphics2D{g =>
      if (useSourceColorLiterally)
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC))

      g.setColor(AWTColorAdapter(color).awtColor)
      g.fillRect(0, 0, owner.widthInPixels, owner.heightInPixels)
    }
  }

  /**
   *
   *
   * @param bitmap
   *
   * @return
   */
  override
  def drawBitmap(bitmap: BitmapBufferAdapter): Boolean = {
    drawBitmap(bitmap, 0.0, 0.0, ColorValidator.MaximumOpacity)
  }

  /**
   *
   *
   * @param bitmap
   * @param xInPixels
   * @param yInPixels
   * @param opacity
   *
   * @return
   */
  override
  def drawBitmap(
      bitmap: BitmapBufferAdapter,
      xInPixels: Double,
      yInPixels: Double,
      opacity: Int): Boolean = {

    val x = xInPixels.floor.toInt
    val y = yInPixels.floor.toInt

    val normalizedOpacity: Float = opacity.toFloat / ColorValidator.MaximumOpacity

    owner.withGraphics2D{g =>
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, normalizedOpacity))

      g.drawImage(bitmap.asInstanceOf[AWTBitmapBufferAdapter].awtBufferedImage, x, y, null)
    }
  }

  /**
   *
   *
   * @param bitmap
   * @param transformation
   * @param opacity
   *
   * @return
   */
  override
  def drawBitmap(
      bitmap: BitmapBufferAdapter,
      transformation: AffineTransformation,
      opacity: Int): Boolean = {

    val normalizedOpacity: Float = opacity.toFloat / ColorValidator.MaximumOpacity

    owner.withGraphics2D{g =>
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, normalizedOpacity))

      g.drawImage(
        bitmap.asInstanceOf[AWTBitmapBufferAdapter].awtBufferedImage,
        transformation.toAWTAffineTransform,
        null)
    }
  }

  /**
   *
   *
   * @param bitmap
   * @param transformation
   *
   * @return
   */
  override
  def drawBitmap(
      bitmap: BitmapBufferAdapter,
      transformation: AffineTransformation): Boolean = {

    drawBitmap(bitmap, transformation, ColorValidator.MaximumOpacity)
  }

  /**
   *
   *
   * @param xInPixels
   * @param yInPixels
   * @param color
   */
  override
  def drawPoint(
      xInPixels: Double,
      yInPixels: Double,
      color: Color): Unit = {

    val x = xInPixels.floor.toInt
    val y = yInPixels.floor.toInt

    owner.withGraphics2D{g =>
      g.setColor(color.toAWTColor)
      g.drawLine(x, y, x, y)
    }
  }

  /**
   *
   *
   * @param boundingBoxUpperLeftX
   * @param boundingBoxUpperLeftY
   * @param widthInPixels
   * @param heightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   */
  //@deprecated(message="Use drawArc() instead.", since="0.0.4")
  override
  def drawEllipse(
      boundingBoxUpperLeftX: Double,
      boundingBoxUpperLeftY: Double,
      widthInPixels: Double,
      heightInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color): Unit = {

    val upperLeftX: Int = boundingBoxUpperLeftX.floor.toInt
    val upperLeftY: Int = boundingBoxUpperLeftY.floor.toInt
    val width: Int = widthInPixels.floor.toInt
    val height: Int = heightInPixels.floor.toInt

    owner.withGraphics2D{g =>
      if (hasFilling) {
        g.setColor(fillColor.toAWTColor)
        g.fillOval(upperLeftX, upperLeftY, width, height)
      }

      if (hasBorder) {
        g.setColor(color.toAWTColor)
        g.drawOval(upperLeftX, upperLeftY, width, height)
      }
    }
  }

  /**
   *
   *
   * @param xOffsetToOrigoInPixels
   * @param yOffsetToOrigoInPixels
   * @param xPositionInPixels
   * @param yPositionInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param startAngleInDegrees
   * @param arcAngleInDegrees
   * @param rotationAngleInDegrees
   * @param scaleFactorX
   * @param scaleFactorY
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   */
  override
  def drawArc(
      xOffsetToOrigoInPixels: Double,
      yOffsetToOrigoInPixels: Double,
      xPositionInPixels: Double,
      yPositionInPixels: Double,
      widthInPixels: Double,
      heightInPixels: Double,
      startAngleInDegrees: Double,
      arcAngleInDegrees: Double,
      rotationAngleInDegrees: Double,
      scaleFactorX: Double,
      scaleFactorY: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color): Unit = {

    val scaledWidth = (scaleFactorX * widthInPixels.floor).truncate
    val scaledHeight = (scaleFactorY * heightInPixels.floor).truncate

    val upperLeftX = (scaledWidth / 2.0).truncate
    val upperLeftY = (scaledHeight / 2.0).truncate

    owner.withGraphics2D{g =>
      g.setStroke(HairlineStroke)

      // If drawing a complete cycle, check if the cycle represents a small circle
      if (arcAngleInDegrees >= 360 &&
          ((scaledWidth == 1.0 && scaledHeight == 1.0)
              || (scaledWidth == 2.0 && scaledHeight == 2.0))) {

        //-------------------------------------------------------------------------
        // Special case for small circles
        //

        if (hasBorder || hasFilling) {
          if (hasBorder) {
            g.setColor(color.toAWTColor)
          }
          else if (hasFilling) {
            g.setColor(fillColor.toAWTColor)
          }

          g.setTransform(g.getDeviceConfiguration.getDefaultTransform)
          g.translate(
            xOffsetToOrigoInPixels + xPositionInPixels,
            yOffsetToOrigoInPixels + yPositionInPixels)

          if (scaledWidth == 1.0) {
            g.fillRect(upperLeftX.toInt, upperLeftY.toInt, 1, 1)
          }
          else {
            g.fillRect(upperLeftX.toInt - 2, upperLeftY.toInt - 2, 2, 2)
          }
        }
      }
      else {
        //-------------------------------------------------------------------------
        // General case for all arcs
        //

        //g.setTransform(g.getDeviceConfiguration.getDefaultTransform)
        //g.setTransform(AffineTransform.getTranslateInstance(0.0, 0.0))

        if (hasFilling && !hasBorder) {
          g.setTransform(AffineTransform.getTranslateInstance(
            xOffsetToOrigoInPixels + xPositionInPixels,
            yOffsetToOrigoInPixels + yPositionInPixels))

          if (rotationAngleInDegrees != 0.0)
            g.rotate(rotationAngleInDegrees)

          g.setColor(fillColor.toAWTColor)

          /*
          g.fillOval(
            -upperLeftX.toInt - 1,
            -upperLeftY.toInt - 1,
            (scaledWidth + 1).toInt,
            (scaledHeight + 1).toInt)
          // */

          /*
          val shape = new Arc2D.Double(
            -upperLeftX - 0.5,
            -upperLeftY - 0.5,
            scaledWidth,
            scaledHeight,
            startAngleInDegrees,
            arcAngleInDegrees,
            Arc2D.OPEN)

          g.fill(shape)
          // */

          //*
          val ulX =
            if (xOffsetToOrigoInPixels + xPositionInPixels - upperLeftX > 0)
              -upperLeftX + 0.5
            else
              -upperLeftX - 0.5

          val ulY =
            if (yOffsetToOrigoInPixels + yPositionInPixels - upperLeftY > 0)
              -upperLeftY + 0.5
            else
              -upperLeftY - 0.5

          val s = new Ellipse2D.Double(
            ulX,
            ulY,
            scaledWidth,
            scaledHeight)

          g.fill(s)
          // */
        }
        else if (hasBorder && !hasFilling) {
          g.setTransform(AffineTransform.getTranslateInstance(
            xOffsetToOrigoInPixels + xPositionInPixels,
            yOffsetToOrigoInPixels + yPositionInPixels))

          if (rotationAngleInDegrees != 0.0)
            g.rotate(rotationAngleInDegrees)

          g.setColor(color.toAWTColor)

          /*
          g.drawOval(
            -upperLeftX.toInt,
            -upperLeftY.toInt,
            (scaledWidth - 1).toInt,
            (scaledHeight - 1).toInt)
          // */

          /*
          val shape = new Arc2D.Double(
            -upperLeftX,
            -upperLeftY,
            scaledWidth - 1,
            scaledHeight - 1,
            startAngleInDegrees,
            arcAngleInDegrees,
            Arc2D.OPEN)

          g.draw(shape)
          // */

          //*
          val s = new Ellipse2D.Double(
            -upperLeftX,
            -upperLeftY,
            scaledWidth - 1,
            scaledHeight - 1)

          g.draw(s)
          // */
        }
        else {
          // Has both border and filling

          g.setTransform(AffineTransform.getTranslateInstance(
            xOffsetToOrigoInPixels + xPositionInPixels,
            yOffsetToOrigoInPixels + yPositionInPixels))

          if (rotationAngleInDegrees != 0.0)
            g.rotate(rotationAngleInDegrees)

          g.setStroke(HairlineStroke)
          g.setColor(fillColor.toAWTColor)

          /*
          g.fillOval(
            -upperLeftX.toInt,
            -upperLeftY.toInt,
            (scaledWidth - 1).toInt,
            (scaledHeight - 1).toInt)
          // */

          /*
          val fillingShape = new Arc2D.Double(
            -upperLeftX,
            -upperLeftY,
            scaledWidth - 1,
            scaledHeight - 1,
            startAngleInDegrees,
            arcAngleInDegrees,
            Arc2D.OPEN)

          g.fill(fillingShape)
          // */

          //*
          val fillingShape = new Ellipse2D.Double(
            -upperLeftX,
            -upperLeftY,
            scaledWidth - 1,
            scaledHeight - 1)

          g.fill(fillingShape)
          // */

          g.setStroke(HairlineStroke)
          g.setColor(color.toAWTColor)

          /*
          g.drawOval(
            -upperLeftX.toInt,
            -upperLeftY.toInt,
            (scaledWidth - 1).toInt,
            (scaledHeight - 1).toInt)
          // */

          /*
          val borderShape = new Arc2D.Double(
            -upperLeftX,
            -upperLeftY,
            scaledWidth - 1,
            scaledHeight - 1,
            startAngleInDegrees,
            arcAngleInDegrees,
            Arc2D.OPEN)

          g.draw(borderShape)
          // */

          //*
          val borderShape = new Ellipse2D.Double(
            -upperLeftX,
            -upperLeftY,
            scaledWidth - 1,
            scaledHeight - 1)

          g.draw(borderShape)
          // */
        }
      }
    }
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   */
  //@deprecated(message="Use polygons/polylines instead.", since="0.0.4")
  override
  def drawRectangle(
      upperLeftCornerXInPixels: Double,
      upperLeftCornerYInPixels: Double,
      widthInPixels: Double,
      heightInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color): Unit = {

    val upperLeftX: Int = upperLeftCornerXInPixels.floor.toInt
    val upperLeftY: Int = upperLeftCornerYInPixels.floor.toInt
    val width: Int = widthInPixels.floor.toInt
    val height: Int = heightInPixels.floor.toInt

    owner.withGraphics2D{g =>
      if (hasFilling) {
        g.setColor(fillColor.toAWTColor)
        g.fillRect(upperLeftX, upperLeftY, width, height)
      }

      if (hasBorder) {
        g.setColor(color.toAWTColor)
        g.drawRect(upperLeftX, upperLeftY, width, height)
      }
    }
  }

  /**
   *
   *
   * @param upperLeftCornerXInPixels
   * @param upperLeftCornerYInPixels
   * @param widthInPixels
   * @param heightInPixels
   * @param roundingWidthInPixels
   * @param roundingHeightInPixels
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   */
  override
  def drawRoundedRectangle(
      upperLeftCornerXInPixels: Double,
      upperLeftCornerYInPixels: Double,
      widthInPixels: Double,
      heightInPixels: Double,
      roundingWidthInPixels: Double,
      roundingHeightInPixels: Double,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color): Unit = {

    val upperLeftX: Int = upperLeftCornerXInPixels.floor.toInt
    val upperLeftY: Int = upperLeftCornerYInPixels.floor.toInt
    val width: Int = widthInPixels.floor.toInt
    val height: Int = heightInPixels.floor.toInt
    val roundingWidth: Int = roundingWidthInPixels.floor.toInt
    val roundingHeight: Int = roundingHeightInPixels.floor.toInt

    owner.withGraphics2D{g =>
      if (hasFilling) {
        g.setColor(fillColor.toAWTColor)
        g.fillRoundRect(upperLeftX, upperLeftY, width, height, roundingWidth, roundingHeight)
      }

      if (hasBorder) {
        g.setColor(color.toAWTColor)
        g.drawRoundRect(upperLeftX, upperLeftY, width, height, roundingWidth, roundingHeight)
      }
    }
  }

  /**
   *
   *
   * @param xCoordinates
   * @param yCoordinates
   * @param numberOfCoordinatesToDraw
   * @param color
   */
  override
  def drawPolyline(
      xCoordinates: Seq[Double],
      yCoordinates: Seq[Double],
      numberOfCoordinatesToDraw: Int,
      color: Color): Unit = {

    val xs = xCoordinates.map(_.floor.toInt).toArray
    val ys = yCoordinates.map(_.floor.toInt).toArray

    owner.withGraphics2D{g =>
      g.setColor(color.toAWTColor)
      g.drawPolyline(xs, ys, numberOfCoordinatesToDraw)
    }
  }

  /**
   *
   *
   * @param xCoordinates
   * @param yCoordinates
   * @param numberOfCoordinatesToDraw
   * @param hasBorder
   * @param hasFilling
   * @param color
   * @param fillColor
   */
  override
  def drawPolygon(
      xCoordinates: Seq[Double],
      yCoordinates: Seq[Double],
      numberOfCoordinatesToDraw: Int,
      hasBorder: Boolean,
      hasFilling: Boolean,
      color: Color,
      fillColor: Color): Unit = {

    val xs = xCoordinates.map(_.floor.toInt).toArray
    val ys = yCoordinates.map(_.floor.toInt).toArray

    owner.withGraphics2D{g =>
      if (hasFilling) {
        g.setColor(fillColor.toAWTColor)
        g.fillPolygon(xs, ys, numberOfCoordinatesToDraw)
      }

      if (hasBorder) {
        g.setColor(color.toAWTColor)
        g.drawPolygon(xs, ys, numberOfCoordinatesToDraw)
      }
    }
  }

  /**
   *
   *
   * @param fromXInPixels
   * @param fromYInPixels
   * @param toXInPixels
   * @param toYInPixels
   * @param color
   */
  override
  def drawLine(
      fromXInPixels: Double,
      fromYInPixels: Double,
      toXInPixels: Double,
      toYInPixels: Double,
      color: Color): Unit = {

    val startX = fromXInPixels.floor.toInt
    val startY = fromYInPixels.floor.toInt
    val endX = toXInPixels.floor.toInt
    val endY = toYInPixels.floor.toInt

    owner.withGraphics2D{g =>
      g.setColor(color.toAWTColor)
      g.drawLine(startX, startY, endX, endY)
    }
  }

  /**
   *
   *
   * @param workUnit
   * @tparam ResultType
   *
   * @return
   */
  def use[ResultType](workUnit: Graphics2D => ResultType): ResultType = {
    owner.withGraphics2D[ResultType](workUnit)
  }

}
