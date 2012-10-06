package de.vorb.compvis.math

import scala.math.ScalaNumber

object Stochastic {

  def mean(xs: Seq[Int]): Double =
    xs.sum.toDouble / xs.length

  def variance(xs: Seq[Int], mean: Double = Double.NaN): Double = {
    val m =
      if (mean.isNaN) this.mean(xs)
      else mean

    var sum = 0.0
    for {
      x <- xs
      diff = m - x
    } sum += diff * diff

    sum / xs.size
  }

  def standardDerivation(xs: Seq[Int], variance: Double = Double.NaN) =
    math.sqrt {
      if (variance.isNaN) this.variance(xs)
      else variance
    }
}