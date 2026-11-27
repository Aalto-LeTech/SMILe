SMILe draws pictures from Scala, on the JVM with Java2D and in a browser with canvas and SVG.

Everything drawn is an [[smile.pictures.Element]], which is a shape, a text, a bitmap, an SVG
picture, or a [[smile.pictures.Picture]] of several. Elements are immutable. Transforming or combining them gives
new elements.

```scala
import smile.colors.Color
import smile.modeling.Pos
import smile.pictures.*

val sun   = Circle(Pos.Origin, 40, fillWith(Color(255, 200, 0)), None)
val house = Rectangle(120, 80, fillWith(Color(200, 80, 60)), None)
val scene = house.addToTop(sun, gap = 10)
```

![A yellow circle above a red rectangle](pictures/scene.png)

Where to look:

  - [[smile.pictures]] has the elements and the operations on them.
  - [[smile.colors]] has colors, the named presets and gradients.
  - [[smile.modeling]] has positions, bounds, directions and velocities.
  - [[smile.animation]] has timing, easing and tweening helpers.
  - [[smile.infrastructure.Renderer]] renders a picture into a bitmap or onto your own graphics, and
    on Scala.js `smile.infrastructure.JSRenderer` draws onto a canvas or gives SVG.
  - [[smile.RenderSettings]] holds the settings an output renders with.
