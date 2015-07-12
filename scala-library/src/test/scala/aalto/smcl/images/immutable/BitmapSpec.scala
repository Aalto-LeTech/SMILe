package aalto.smcl.images.immutable

import java.awt.{Graphics2D => JGraphics2D}
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import aalto.smcl.images._

/**
 *
 *
 * @author Aleksi Lukkarinen 
 */
class BitmapSpec extends ImageSpecBase {

  "Class BitmapImageModel" - {
    "when created for a BitmapImage of size 8 x 9 pixels" - {
      val (width, height) = (8, 9)
      val numOfPixels = width * height
      val b = BitmapImage(Option(width), Option(height))

      "must have a width of 8 pixels" in { assert(b.pixelBuffer.getWidth === 8) }
      "must have a width of 9 pixels" in { assert(b.pixelBuffer.getHeight === 9) }
      "must have a widthRange" - {
        "starting from 0" in { assert(b.widthRange.start === 0) }
        "ending to 7" in { assert(b.widthRange.end === 7) }
      }
      "must have a heightRange" - {
        "starting from 0" in { assert(b.heightRange.start === 0) }
        "ending to 8" in { assert(b.heightRange.end === 8) }
      }
      "must have 72 pixels in total" in { assert(b.pixelCount === numOfPixels) }
    }

    "when created for an image without giving a background color, must have all its pixels of fully opaque black" in {
      val b = newDefaultSmallTestImage

      for (y <- b.heightRange; x <- b.widthRange) { // -- DEBUG -- info(s"(${x},${y})")
        assert(b.pixelIntAt(x, y) === 0xFF000000)
      }
    }

    "when created for an image with a given opaque background color, must have all its pixels of that colour" in {
      val b = BitmapImage(initialBackgroundColorOption = Option[Int](TEST_PIXEL_INT))

      for (y <- b.heightRange; x <- b.widthRange) { // -- DEBUG -- info(s"(${x},${y})")
        assert(b.pixelIntAt(x, y) === TEST_PIXEL_INT)
      }
    }

    "must be able to give a Graphics2D instance" in {
      assert(BitmapImage().graphics2D.isInstanceOf[JGraphics2D])
    }

    "must be able to clear() the image with a given opaque color" in {
      val testColors = Table("c", 0xFF9EADBC, 0xFF000000, 0xFF123456)

      forAll(testColors) { c =>
        info(s"testing pixelInt value: 0x${c.toArgbHexColorString}  (${c})")

        val b = BitmapImage()

        b.clear(Option(c))

        for (y <- b.heightRange; x <- b.widthRange) { // -- DEBUG -- info(s"(${x},${y})")
          b.pixelIntAt(x, y) shouldEqual c
        }
      }
    }

    "must be able to clear() the image with the default (opaque) background color" in {
      val testColors = Table("c", 0xFF9EADBC, 0xFF000000, 0xFF123456)

      forAll(testColors) { c =>
        info(s"testing pixelInt value: 0x${c.toArgbHexColorString}  (${c})")

        val b = BitmapImage(initialBackgroundColorOption = Option(c))

        b.clear()

        for (y <- b.heightRange; x <- b.widthRange) { // -- DEBUG -- info(s"(${x},${y})")
          b.pixelIntAt(x, y) shouldEqual c
        }
      }
    }

  }

}