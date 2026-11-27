package smile.testing

import smile.{RenderSettings, ScalingMethod, TransformMethod}

/** Nearest-neighbor scaling and transforms, so that pixel operations such as flips and rotations
  * are bit-exact.
  */
val NearestNeighborTransforms: RenderSettings = RenderSettings(
  bufferScalingMethod = ScalingMethod.NearestNeighbor,
  bufferTransformMethod = TransformMethod.Nearest
)

/** Nearest-neighbor transforms with antialiasing disabled, for fully deterministic rendering. */
val DeterministicRendering: RenderSettings =
  NearestNeighborTransforms.copy(drawingIsAntiAliased = false)
