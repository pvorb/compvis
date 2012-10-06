package de.vorb.compvis.bin

import de.vorb.compvis.{ colorIntensity, getWindow }
import de.vorb.compvis.math.Stochastic.{ mean, variance, standardDerivation }
import java.awt.image.BufferedImage
import java.awt.Color

object Sauvola {
  val black = 0x000000
  val white = 0xFFFFFF
  val R = 128
}

class Sauvola(
  val src: BufferedImage,
  val dest: BufferedImage,
  val radius: Int = 2,
  val k: Double = 0.2) {
  
  import Sauvola._

  require(k >= 0.2 && k <= 0.5, "k must be in interval [0.2, 0.5]")

  private var binarized = false

  protected def binarize(x: Int, y: Int): Int = {

    val xs = getWindow(src, x, y, radius)
    val int = colorIntensity(new Color(src.getRGB(x, y)))

    if (int <= threshold(xs)) black
    else white
  }

  protected def threshold(xs: IndexedSeq[Int]): Double = {
    val m = mean(xs)
    val dev = standardDerivation(xs, variance(xs, m))

    m * (1 + k * (dev / R - 1))
  }

  def binarize(): BufferedImage =
    if (binarized) dest
    else {
      for {
        x <- 0 until src.getWidth
        y <- 0 until src.getHeight
        g = binarize(x, y)
      } {
        dest.setRGB(x, y, g)
      }

      dest
    }
}
