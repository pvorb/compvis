package de.vorb

import java.awt.Color
import java.awt.image.BufferedImage

package object compvis {
  def colorIntensity(rgb: Color): Int =
    rgb.getRed max rgb.getGreen max rgb.getBlue

  def getWindow(img: BufferedImage, x: Int, y: Int, radius: Int): IndexedSeq[Int] = {
    val xMin = 0 max (x - radius)
    val xMax = (x + radius) min (img.getWidth - 1)
    val yMin = 0 max (y - radius)
    val yMax = (y + radius) min (img.getHeight - 1)

    for {
      x <- xMin until xMax
      y <- yMin until yMax
      intensity = colorIntensity(new Color(img.getRGB(x, y)))
    } yield intensity
  }
}