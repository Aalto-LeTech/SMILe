package smile.modeling

/** Enum for vertical alignment options with a corresponding side-independent alignment.
  */
enum VerticalAlignment(val sideIndependent: SideIndependentAlignment):
  case Top    extends VerticalAlignment(SideIndependentAlignment.TopOrLeft)
  case Middle extends VerticalAlignment(SideIndependentAlignment.MiddleOrCenter)
  case Bottom extends VerticalAlignment(SideIndependentAlignment.BottomOrRight)

/** Enum for horizontal alignment options with a corresponding side-independent alignment.
  */
enum HorizontalAlignment(val sideIndependent: SideIndependentAlignment):
  case Left   extends HorizontalAlignment(SideIndependentAlignment.TopOrLeft)
  case Center extends HorizontalAlignment(SideIndependentAlignment.MiddleOrCenter)
  case Right  extends HorizontalAlignment(SideIndependentAlignment.BottomOrRight)

/** Enum for side-independent alignment, which can be converted to either horizontal or vertical
  * alignment.
  */
enum SideIndependentAlignment:
  inline def toHorizontal: HorizontalAlignment = this match
    case TopOrLeft      => HorizontalAlignment.Left
    case MiddleOrCenter => HorizontalAlignment.Center
    case BottomOrRight  => HorizontalAlignment.Right

  inline def toVertical: VerticalAlignment = this match
    case TopOrLeft      => VerticalAlignment.Top
    case MiddleOrCenter => VerticalAlignment.Middle
    case BottomOrRight  => VerticalAlignment.Bottom

  case TopOrLeft, MiddleOrCenter, BottomOrRight

/** Enum representing the four cardinal sides.
  */
enum Side:
  case Top, Bottom, Left, Right
