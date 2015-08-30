package aalto.smcl.bitmaps.metadata


import aalto.smcl.SMCL
import aalto.smcl.common.{PresetRGBAColor, RGBAColor}
import aalto.smcl.interfaces.MetadataInterfaceSourceProvider




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class RGBAColorMetadataInterfaceSourceProvider extends MetadataInterfaceSourceProvider {

  SMCL.performInitialization()


  /** */
  private[this] val _rgbaColorClass = RGBAColor(0).getClass

  /** */
  private[this] val _presetRGBAColorClass = PresetRGBAColor(0, Option("<dummy>")).getClass


  /**
   *
   *
   * @param interestingObject
   * @return
   */
  override def querySourceFor(interestingObject: Any): Option[Any] = {
    val c = interestingObject.getClass

    if (c.isAssignableFrom(_rgbaColorClass)
      || c.isAssignableFrom(_presetRGBAColorClass)) {

      return Some(RGBAColorMetadataSource(interestingObject.asInstanceOf[RGBAColor]))
    }

    None
  }

}
