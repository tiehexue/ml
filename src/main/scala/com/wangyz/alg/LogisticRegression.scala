


package com.wangyz.alg

import scala.math

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._
import com.wangyz.util.TimerTrait

import AlgUtil._

case class LogisticRegression(override val nx: Int, override val ny: Int) extends AbstractLearning(nx, ny) {

  def trainOne(x: Array[Double], y: Array[Double]) = {
  
    val tmpPy = wbx(W, b, x)

    val py = softmax(tmpPy)
    val dy = py.zipWithIndex.map{ case (e, i) => y(i) - e }

    updateW(this, dy, x)
    updateb(this, dy)
    
    py
  }

  override def train(xs: Array[Array[Double]], others: Any*) = {
    val ys = get[Array[Array[Double]]](0, others)
    xs.zipWithIndex.map{ case(e, i) => 
      trainOne(e, ys(i))
    }
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

    val classifier = new LogisticRegression(xs.head.size, ys.head.size) with TimerTrait

    val epochs: Int = 1000
    for(i <- 0 until epochs) {
      classifier.train(xs, ys)
    }

    for (i <- 0 until xs.size) {
      println(array1D(classifier.predict(xs(i))))
    }
  }
}
