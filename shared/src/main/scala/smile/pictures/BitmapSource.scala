package smile.pictures

import smile.infrastructure.NativeBuffer

/** What a `smile.pictures.PixelSource` was made from, before any of its recorded operations. */
private[smile] enum BitmapSource:

  /** Pixels that exist already, as a loaded image, generated pixels, or the result of an operation
    * on pixels. The buffer is never changed.
    */
  case Pixels(buffer: NativeBuffer)

  /** A picture, rendered with the settings of the output that asks for the pixels. It is rendered
    * at one buffer pixel per logical pixel for the bitmap's pixels, and at the density an output
    * needs when it draws the bitmap directly. A recolored picture's pixels for the recolor's
    * settings are the bitmap's checkpoint (see [[smile.pictures.Recolor]]).
    */
  case Content(picture: Element)
end BitmapSource
