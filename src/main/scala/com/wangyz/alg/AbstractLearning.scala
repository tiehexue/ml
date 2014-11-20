package com.wangyz.alg

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._

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
    val tmpY = wbx(x)

    softmax(tmpY)
  }

  def wbx(x: Array[Double]): Array[Double] = {
    wbx(W, b, x)
  }

  def wbx(tW: Array[Array[Double]], tb: Array[Double], x: Array[Double]): Array[Double] = {
    tW.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        e * x(j)
      }.sum + tb(i)
    }.toArray
  }

  def updateW(dy: Any, x: Array[Double]): Unit = {
    updateW(this, dy, x)
  }

  def updateW(cls: AbstractLearning, dy: Any, x: Array[Double]): Unit = {
    val delta = dy match {
      case d: Double => Array.fill[Double](cls.W.size)(d)
      case ds: Array[Double] => ds
    }

    cls.W.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        cls.W(i)(j) = e + learningRate * delta(i) * x(j) / nx
      }
    }
  }

  def updateb(dy: Any): Unit = {
    updateb(this, dy)
  }

  def updateb(cls: AbstractLearning, dy: Any): Unit = {
    val delta = dy match {
      case d: Double => Array.fill[Double](cls.W.size)(d)
      case ds: Array[Double] => ds
    }

    cls.b.zipWithIndex.map{ case (e, i) =>
      cls.b(i) = e + learningRate * delta(i) / nx
    }  
  }
}