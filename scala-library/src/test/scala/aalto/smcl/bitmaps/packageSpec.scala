package aalto.smcl.bitmaps


import aalto.smcl.common._
import aalto.smcl.common.ColorOps._




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class packageSpec extends ImageSpecBase {

  "the right color component must be extracted from an Int representing a pixel value by" - {
    "redComponentFrom()" in {
      assert(redComponentFrom(TEST_PIXEL_INT) === TEST_RED_VALUE)
    }

    "greenComponentFrom()" in {
      assert(greenComponentFrom(TEST_PIXEL_INT) === TEST_GREEN_VALUE)
    }

    "blueComponentFrom()" in {
      assert(blueComponentFrom(TEST_PIXEL_INT) === TEST_BLUE_VALUE)
    }

    "opacityComponentFrom()" in {
      assert(opacityComponentFrom(TEST_PIXEL_INT) === TEST_OPACITY_VALUE)
    }
  }

  "withNewRedComponent() must" - {
    "return an Int representing a pixel value with the right color component updated" in {
      assert(withNewRedComponent(TEST_PIXEL_INT, 0) === TEST_PIXEL_INT_WITH_ZEROED_RED)
    }
    "throw an IllegalArgumentException when color component is" - {
      "less than MIN_RED" in {
        intercept[IllegalArgumentException] {withNewRedComponent(TEST_PIXEL_INT, MinimumRed - 1)}
      }
      "greater than MAX_RED" in {
        intercept[IllegalArgumentException] {withNewRedComponent(TEST_PIXEL_INT, MaximumRed + 1)}
      }
    }
  }

  "withNewGreenComponent() must" - {
    "return an Int representing a pixel value with the right color component updated" in {
      assert(withNewGreenComponent(TEST_PIXEL_INT, 0) === TEST_PIXEL_INT_WITH_ZEROED_GREEN)
    }
    "throw an IllegalArgumentException when color component is" - {
      "less than MIN_GREEN" in {
        intercept[IllegalArgumentException] {withNewGreenComponent(TEST_PIXEL_INT, MinimumGreen - 1)}
      }
      "greater than MAX_GREEN" in {
        intercept[IllegalArgumentException] {withNewGreenComponent(TEST_PIXEL_INT, MaximumGreen + 1)}
      }
    }
  }

  "withNewBlueComponent() must" - {
    "return an Int representing a pixel value with the right color component updated" in {
      assert(withNewBlueComponent(TEST_PIXEL_INT, 0) === TEST_PIXEL_INT_WITH_ZEROED_BLUE)
    }
    "throw an IllegalArgumentException when color component is" - {
      "less than MIN_BLUE" in {
        intercept[IllegalArgumentException] {withNewBlueComponent(TEST_PIXEL_INT, MinimumBlue - 1)}
      }
      "greater than MAX_BLUE" in {
        intercept[IllegalArgumentException] {withNewBlueComponent(TEST_PIXEL_INT, MaximumBlue + 1)}
      }
    }
  }

  "withNewOpacityComponent() must" - {
    "return an Int representing a pixel value with the right color component updated" in {
      assert(withNewOpacityComponent(TEST_PIXEL_INT, 0) === TEST_PIXEL_INT_WITH_ZEROED_OPACITY)
    }
    "throw an IllegalArgumentException when color component is" - {
      "less than MinimumOpacity" in {
        intercept[IllegalArgumentException] {withNewOpacityComponent(TEST_PIXEL_INT, MinimumOpacity - 1)}
      }
      "greater than MaximumOpacity" in {
        intercept[IllegalArgumentException] {withNewOpacityComponent(TEST_PIXEL_INT, MaximumOpacity + 1)}
      }
    }
  }

  "colorComponentsFrom() must" - {
    "return a map with the right color components of an Int representing a pixel value" in {
      assert(colorComponentsFrom(TEST_PIXEL_INT) ===
          Map[Symbol, Int](
            'red -> TEST_RED_VALUE,
            'green -> TEST_GREEN_VALUE,
            'blue -> TEST_BLUE_VALUE,
            'opacity -> TEST_OPACITY_VALUE))
    }
  }

  "pixelIntFrom() must" - {
    "compose the right pixel Int value based on given color components" in {
      assert(pixelIntFrom(
        TEST_RED_VALUE,
        TEST_GREEN_VALUE,
        TEST_BLUE_VALUE,
        TEST_OPACITY_VALUE) === TEST_PIXEL_INT)
    }

    "throw an IllegalArgumentException when color component" - {
      "'red' is less than MIN_RED" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(red = MinimumRed - 1)
        }
      }
      "'red' is greater than MAX_RED" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(red = MaximumRed + 1)
        }
      }
      "'green' is less than MIN_GREEN" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(green = MinimumGreen - 1)
        }
      }
      "'green' is greater than MAX_GREEN" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(green = MaximumGreen + 1)
        }
      }
      "'blue' is less than MIN_BLUE" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(blue = MinimumBlue - 1)
        }
      }
      "'blue' is greater than MAX_BLUE" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(blue = MaximumBlue + 1)
        }
      }
      "opacity is less than MinimumOpacity" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(opacity = MinimumOpacity - 1)
        }
      }
      "opacity is greater than MaximumOpacity" in {
        intercept[IllegalArgumentException] {
          pixelIntFrom(opacity = MaximumOpacity + 1)
        }
      }
    }
  }

  "implicit class PixelInt must return the correct" - {
    "color component map by invoking colorComponentInts()" in {
      assert(TEST_PIXEL_INT.colorComponentMap ===
          Map[Symbol, Int](
            'red -> TEST_RED_VALUE,
            'green -> TEST_GREEN_VALUE,
            'blue -> TEST_BLUE_VALUE,
            'opacity -> TEST_OPACITY_VALUE))
    }
    "red component by invoking redComponentInt()" in {
      assert(TEST_PIXEL_INT.redComponentInt === TEST_RED_VALUE)
    }
    "green component by invoking greenComponentInt()" in {
      assert(TEST_PIXEL_INT.greenComponentInt === TEST_GREEN_VALUE)
    }
    "blue component by invoking blueComponentInt()" in {
      assert(TEST_PIXEL_INT.blueComponentInt === TEST_BLUE_VALUE)
    }
    "opacity component by invoking opacityComponentInt()" in {
      assert(TEST_PIXEL_INT.opacityComponentInt === TEST_OPACITY_VALUE)
    }
    "hexadecimal representation by invoking toArgbHexColorString()" in {
      assert(TEST_PIXEL_INT.toArgbHexColorString === "ffdcba98")
    }
    "binary representation by invoking toArgbBinaryColorString()" in {
      assert(TEST_PIXEL_INT.toArgbBinaryColorString === "11111111 11011100 10111010 10011000")
    }
  }

}
