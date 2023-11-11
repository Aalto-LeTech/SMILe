package smile.modeling

enum VerticalAlignment(val sideIndependent: SideIndependentAlignment):
  case Top    extends VerticalAlignment(SideIndependentAlignment.TopOrLeft)
  case Middle extends VerticalAlignment(SideIndependentAlignment.MiddleOrCenter)
  case Bottom extends VerticalAlignment(SideIndependentAlignment.BottomOrRight)

enum HorizontalAlignment(val sideIndependent: SideIndependentAlignment):
  case Left   extends HorizontalAlignment(SideIndependentAlignment.TopOrLeft)
  case Center extends HorizontalAlignment(SideIndependentAlignment.MiddleOrCenter)
  case Right  extends HorizontalAlignment(SideIndependentAlignment.BottomOrRight)

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

enum Side:
  case Top, Bottom, Left, Right
