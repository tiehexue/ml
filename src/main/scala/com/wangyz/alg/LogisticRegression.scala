


package com.wangyz.alg

import scala.math

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._

case class LogisticRegression(nx: Int, ny: Int) {

  var learningRate: Double = 0.2

  val W: Array[Array[Double]] = Array.ofDim[Double](ny, nx)
  val b: Array[Double] = new Array[Double](ny)

  def trainOne(x: Array[Double], y: Array[Double]) = {
  
    val tmpPy = wbx(W, b, x)

    val py = softmax(tmpPy)
    val dy = py.zipWithIndex.map{ case (e, i) => y(i) - e }

    W.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        W(i)(j) = e + learningRate * dy(i) * x(j) / nx
      }
    }

    b.zipWithIndex.map{ case (e, i) =>
      b(i) = e + learningRate * dy(i) / nx
    }

    py
  }

  def train(xs: Array[Array[Double]], ys: Array[Array[Double]]) = {
    xs.zipWithIndex.map{ case(e, i) => 
      trainOne(e, ys(i))
    }
  }

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


object LogisticRegression {

  def test = {
    val xs: Array[Array[Double]] = Array(
      Array(1, 1, 1, 0, 0, 0),
      Array(1, 0, 1, 0, 0, 0),
      Array(1, 1, 1, 0, 0, 0),
      Array(0, 0, 1, 1, 1, 0),
      Array(0, 0, 1, 0, 1, 0),
      Array(0, 0, 1, 1, 1, 0)
    )

    val ys: Array[Array[Double]] = Array(
      Array(1, 0),
      Array(1, 0),
      Array(1, 0),
      Array(0, 1),
      Array(0, 1),
      Array(0, 1)
    )

    val classifier = new LogisticRegression(xs.head.size, ys.head.size)

    val epochs: Int = 1000
    for(i <- 0 until epochs) {
      classifier.train(xs, ys)
    }

    for (i <- 0 until xs.size) {
      println(array1D(classifier.predict(xs(i))))
    }
  }
}
