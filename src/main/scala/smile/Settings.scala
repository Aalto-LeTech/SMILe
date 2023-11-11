package smile

import smile.colors.Color
import smile.colors.PresetColor.{Black, LightBlue, Transparent}
import smile.modeling.{Angle, HorizontalAlignment, Pos, PositionType, VerticalAlignment}

object Settings:
  var DrawingIsAntiAliased: Boolean                            = true
  var NewBitmapsAreDisplayedAutomatically: Boolean             = false
  var BitmapsAreDisplayedAutomaticallyAfterOperations: Boolean = false
  var ShapesHaveBordersByDefault: Boolean                      = true
  var ShapesHaveFillingsByDefault: Boolean                     = true
  var CanvasesAreResizedBasedOnTransformations: Boolean        = true
  var DefaultBitmapWidthInPixels: Int                          = 50
  var DefaultBitmapHeightInPixels: Int                         = 50
  var BitmapWidthWarningLimitInPixels: Int                     = 800
  var BitmapHeightWarningLimitInPixels: Int                    = 800
  var DefaultCircleRadiusInPixels: Double                      = 100
  var DefaultStarCuspRadiusInPixels: Double                    = 100
  var DefaultRoundingWidthInPixels: Double                     = 20
  var DefaultRoundingHeightInPixels: Double                    = 20
  var DefaultPaddingInPixels: Double                           = 1
  var DefaultArcStartAngleInDegrees: Int                       = 0
  var DefaultArcAngleInDegrees: Int                            = 180
  var ColorVisualizationTileSideLengthInPixels: Int            = 80
  var DefaultBackgroundColor: Color                            = Transparent
  var DefaultPrimaryColor: Color                               = Black
  var DefaultSecondaryColor: Color                             = LightBlue
  var DefaultPositionType: PositionType                        = PositionType.Center
  var DefaultHorizontalAlignment: HorizontalAlignment          = HorizontalAlignment.Left
  var DefaultVerticalAlignment: VerticalAlignment              = VerticalAlignment.Middle
  val DefaultPosition: Pos                                     = Pos.Origo
  val DefaultRotationAngle: Angle                              = Angle.Zero
  val DefaultRotationAngleInDegrees: Double                    = DefaultRotationAngle.inDegrees
  val IdentityScalingFactor: Double                            = 1.0
