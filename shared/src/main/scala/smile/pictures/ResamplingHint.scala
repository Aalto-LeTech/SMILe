package smile.pictures

/** How a bitmap would like to be resampled where an output draws it at another size than its
  * pixels'. Only an output in `Direct` mode reads the hint. A `Stepwise` output ignores it.
  */
enum ResamplingHint derives CanEqual:

  /** As the output's settings say. */
  case Auto

  /** Interpolated, whatever the output's settings say. */
  case Smooth

  /** Each pixel copied as a block of its nearest neighbor, whatever the output's settings say. */
  case Pixelated
end ResamplingHint
