package smile.pictures

import smile.{BuildInfo, RenderSettings}
import smile.colors.*
import smile.modeling.*

import scala.util.Random

object SMILe:
  private def addNoise(pic: Element, noiseStrength: Double): Bitmap =
    val bitmap    = pic.toBitmap()
    val newBitmap = Bitmap(
      bitmap.pixels().width,
      bitmap.pixels().height,
      (x, y) =>
        val redNoise   = Random.nextInt(255)
        val greenNoise = Random.nextInt(255)
        val blueNoise  = Random.nextInt(255)
        Color(redNoise, greenNoise, blueNoise, 255)
    )

    val transformed = bitmap.mergeWith(
      newBitmap,
      (a, b) =>
        // Screen noise (1−(1−A)×(1−B))
        val newColor = Color(
          ((1 - (1 - a.red / 255.0) * (1 - b.red * noiseStrength / 255.0)) * 255).toInt,
          ((1 - (1 - a.green / 255.0) * (1 - b.green * noiseStrength / 255.0)) * 255).toInt,
          ((1 - (1 - a.blue / 255.0) * (1 - b.blue * noiseStrength / 255.0)) * 255).toInt,
          a.opacity
        )
        newColor,
      RenderSettings.Default
    )
    transformed

  def apply(text: String): Element =
    val logoHeight    = 300.0
    val gradientWidth = logoHeight * 2.07
    val radius        = logoHeight / 2.0
    val padding       = 0
    val bgStartColor  = new Color(0xffb6e4fa)
    val bgEndColor    = new Color(0xff7e84c0)
    val bgGradient    = LinearGradient(
      Pos(-gradientWidth / 2.0, -logoHeight / 2.0),
      Pos(gradientWidth / 2.0, logoHeight / 2.0),
      bgStartColor,
      bgEndColor
    )
    val leftEyeColor  = new Color(0xff0071b9)
    val rightEyeColor = new Color(0xff2ab261)
    val mouthColor    = new Color(0xfff15858)

    val backgroundMask = Rectangle(
      gradientWidth - radius,
      logoHeight - radius,
      fillWith(PresetColor.White),
      lineWith(PresetColor.White, radius, true)
    )
    val pic =
      MaskGroup(
        backgroundMask,
        addNoise(
          Rectangle(
            gradientWidth,
            logoHeight,
            fillWith(bgGradient),
            None
          ),
          noiseStrength = 0.6
        )
      )
        .moveBy(gradientWidth / 2, logoHeight / 2)
        .addAt(
          Rectangle(
            logoHeight / 10.0,
            logoHeight / 10.0,
            fillWith(leftEyeColor),
            lineWith(leftEyeColor, logoHeight / 15.0, true)
          ).rotateAround(18, Pos.Origin), // tilted counterclockwise
          logoHeight / 4.25,
          logoHeight / 3.0,
          PositionType.UpperLeftCorner
        )
        .addAt(
          Triangle(
            logoHeight / 8.57,
            fillWith(rightEyeColor),
            lineWith(rightEyeColor, logoHeight / 15.0, true)
          ).rotateAround(40, Pos.Origin), // tilted counterclockwise
          logoHeight / 1.87 + padding,
          logoHeight / 3.53 + padding,
          PositionType.UpperLeftCorner
        )
        .addAt(
          Arc(
            Pos.Origin,
            logoHeight / 1.51,
            logoHeight / 1.54,
            // From the lower left round to the lower right, by way of six o'clock.
            -138,
            130,
            None,
            lineWith(mouthColor, logoHeight / 9.375, true)
          ),
          logoHeight / 7.21 + padding,
          logoHeight / 15.5 + padding,
          PositionType.UpperLeftCorner
        )
        .addAt(
          Text(
            Pos.Origin,
            None,
            text,
            "Hack",
            logoHeight / 3.33,
            800,
            fillWith(PresetColor.White),
            None
          ),
          logoHeight * 1.04,
          logoHeight / 2 - logoHeight / 3.33 / 2,
          PositionType.UpperLeftCorner
        )
        .addAt(
          Text(
            Pos.Origin,
            None,
            s"Version ${BuildInfo.version}",
            "Hack",
            logoHeight / 14,
            600,
            fillWith(Color(255, 255, 255, 200)),
            None
          ),
          logoHeight * 1.04,
          logoHeight - logoHeight / 3,
          PositionType.UpperLeftCorner
        )

    pic
