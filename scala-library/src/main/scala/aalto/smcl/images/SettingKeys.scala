package aalto.smcl.images


import aalto.smcl.common.settings.SettingKeys._




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object SettingKeys {


  // @formatter:off

  /** */
  case object NewBitmapsAreDisplayedAutomatically extends BooleanSettingKey

  /** */
  case object DisplayBitmapsAutomaticallyAfterOperations extends BooleanSettingKey

  /** */
  case object DefaultBitmapWidthInPixels extends IntSettingKey

  /** */
  case object DefaultBitmapHeightInPixels extends IntSettingKey

  /** */
  case object DefaultBackground extends ColorSettingKey

  /** */
  case object DefaultPrimary extends ColorSettingKey

  /** */
  case object DefaultSecondary extends ColorSettingKey

}
