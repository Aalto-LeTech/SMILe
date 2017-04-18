package aalto.smcl


import org.scalatest.DoNotDiscover

import aalto.smcl.bitmaps.{Bitmap, circle}
import aalto.smcl.colors.{PresetColors, RGBAColor}
import aalto.smcl.infrastructure.GS
import aalto.smcl.infrastructure.tests.SharedIntegrationSpecBase




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
@DoNotDiscover
class SMCLCoreSmokeTests extends SharedIntegrationSpecBase {

  "SMCL must be able to" - {
    "list all settings" in {GS.list()}
    "create a bitmap" in {Bitmap(widthInPixels = 15)}
    "create a circle" in {circle(50)}
    "create a new RGBA color" in {RGBAColor(1, 2, 3, 4)}
    "recall a preset RGBA color" in {PresetColors('lightBlue)}
  }

}
