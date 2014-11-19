package com.wangyz.alg

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._

import AlgUtil._

abstract class AbstractLearning(x: Int, y: Int) extends DeepLearningAlg {

  var learningRate = 0.2
  val nx = x
  val ny = y
  val W: Array[Array[Double]] = Array.ofDim[Double](ny, nx)
  val b: Array[Double] = new Array[Double](ny)

  def loss(xs: Array[Array[Double]], ys: Array[Array[Double]]) = {

    val pys = xs.map(x => predict(x))

    val tmpLosses = pys.zipWithIndex.map { case (p, i) =>
      p.zipWithIndex.map { case (e, j) =>
        ys(i)(j) * math.log(e) + (1 - ys(i)(j)) * math.log(1 - e)
      }.sum
    }

    - tmpLosses.sum / tmpLosses.size
  }

  def error(xs: Array[Array[Double]], ys: Array[Array[Double]]) = {

    val pys = xs.map(x => predict(x))

    val errors = pys.zipWithIndex.map { case (p, i) =>
      val maxIndex = p.indexOf(p.max)
      ys(i)(maxIndex) != 1
    }.filter(_ == true)

    errors.size.doubleValue / xs.size
  }

  def predict(x: Array[Double]) = {
    val tmpY = wbx(W, b, x)

    softmax(tmpY)
  }
}