package smile.infrastructure

import smile.pictures.{Bitmap, Picture, PictureElement}

object Renderer:
  def apply(): Renderer = PlatformSpecific.renderer

trait Renderer:
  def createBitmapFrom(elements: PictureElement*): Bitmap
  def createBitmapFrom(picture: Picture): Bitmap
