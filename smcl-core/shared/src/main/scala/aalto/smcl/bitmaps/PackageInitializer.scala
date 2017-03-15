package aalto.smcl.bitmaps


import aalto.smcl.colors.{PresetColors, RGBAColor}
import aalto.smcl.infrastructure._




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
private[smcl]
class PackageInitializer {

  /** */
  private val _settingValidatorFactory = new SettingValidatorFactory()


  //
  // Initialize settings
  //
/*
  addInitializer(PackageInitializationPhase.Early) {() =>
    GS += new Setting[Boolean](
      key = NewBitmapsAreDisplayedAutomatically,
      initialValue = false,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[Boolean](
      key = BitmapsAreDisplayedAutomaticallyAfterOperations,
      initialValue = false,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[Boolean](
      key = ShapesHaveBordersByDefault,
      initialValue = true,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[Boolean](
      key = ShapesHaveFillingsByDefault,
      initialValue = false,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[Boolean](
      key = CanvasesAreResizedBasedOnTransformations,
      initialValue = true,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[Int](
      key = DefaultBitmapWidthInPixels,
      initialValue = 50,
      validator = BitmapValidatorFunctionFactory.BitmapWidthValidator())

    GS += new Setting[Int](
      key = DefaultBitmapHeightInPixels,
      initialValue = 50,
      validator = BitmapValidatorFunctionFactory.BitmapHeightValidator())

    GS += new Setting[Int](
      key = BitmapWidthWarningLimitInPixels,
      initialValue = 800,
      validator = BitmapValidatorFunctionFactory.BitmapWidthValidator())

    GS += new Setting[Int](
      key = BitmapHeightWarningLimitInPixels,
      initialValue = 800,
      validator = BitmapValidatorFunctionFactory.BitmapHeightValidator())

    GS += new Setting[Int](
      key = DefaultCircleRadiusInPixels,
      initialValue = 10,
      validator = _settingValidatorFactory.conditionFalseValidator[Int]({
        _ < 1
      }, "Circle radius must be at least 1 pixel"))

    GS += new Setting[Int](
      key = DefaultRoundingWidthInPixels,
      initialValue = 20,
      validator = _settingValidatorFactory.conditionFalseValidator[Int]({
        _ < 1
      }, "Rounding width must be at least 1 pixel"))

    GS += new Setting[Int](
      key = DefaultRoundingHeightInPixels,
      initialValue = 20,
      validator = _settingValidatorFactory.conditionFalseValidator[Int]({
        _ < 1
      }, "Rounding height must be at least 1 pixel"))

    GS += new Setting[Int](
      key = DefaultPaddingInPixels,
      initialValue = 5,
      validator = _settingValidatorFactory.conditionFalseValidator[Int]({
        _ < 0
      }, "Padding cannot be negative"))

    GS += new Setting[Int](
      key = DefaultArcStartAngleInDegrees,
      initialValue = 0,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[Int](
      key = DefaultArcAngleInDegrees,
      initialValue = 180,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[RGBAColor](
      key = DefaultBackground,
      initialValue = PresetColors('white).withAbsoluteOpacity(0),
      validator = _settingValidatorFactory.IsNullValidator("Color cannot be null"))

    GS += new Setting[RGBAColor](
      key = DefaultPrimary,
      initialValue = PresetColors('black),
      validator = _settingValidatorFactory.IsNullValidator("Color cannot be null"))

    GS += new Setting[RGBAColor](
      key = DefaultSecondary,
      initialValue = PresetColors('black),
      validator = _settingValidatorFactory.IsNullValidator("Color cannot be null"))

    GS += new Setting[HorizontalAlignment.Value](
      key = DefaultHorizontalAlignment,
      initialValue = HorizontalAlignment.Left,
      validator = _settingValidatorFactory.EmptyValidator)

    GS += new Setting[VerticalAlignment.Value](
      key = DefaultVerticalAlignment,
      initialValue = VerticalAlignment.Middle,
      validator = _settingValidatorFactory.EmptyValidator)
  }
*/
}
