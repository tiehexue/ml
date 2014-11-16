package com.wangyz.alg

import scala.math

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._

case class dA(nx: Int, nhidden: Int, corruptLevel: Double) {

  var learningRate: Double = 0.2

  val W: Array[Array[Double]] = Array.ofDim[Double](nhidden, nx)
  val b: Array[Double] = new Array[Double](nhidden)
  val bVisible: Array[Double] = new Array[Double](nx)

  def getHidden(x: Array[Double]) = {
    val tmpHidden = W.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map {case (e, j) =>
        e * x(j)
      }.sum + b(i)
    }.toArray   

    sigmoid(tmpHidden)
  }

  def reconstruct(y: Array[Double]) = {
    val tmp = W.transpose.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map {case (e, j) =>
        e * y(j)
      }.sum + bVisible(i)
    }.toArray   

    sigmoid(tmp)
  }

  def corrupt(x: Array[Double]) = {
    val c = binomial(x.size, corruptLevel)
    x.zipWithIndex.map{ case (e, i) => e * c(i) }
  }

  def trainOne(x: Array[Double]) = {
    val c = corrupt(x)
    val y = getHidden(x)
    val z = reconstruct(y)

    val tmpVisibleBias = bVisible.zipWithIndex.map{ case (e, i) =>
      val bias = x(i) - z(i)
      bVisible(i) += learningRate * bias / nx
      bias
    }

    val tmpBias = b.zipWithIndex.map { case (e, i) =>
      val bias = bVisible.zipWithIndex.map { case (v, j) =>
        W(i)(j) * tmpVisibleBias(i)
      }.sum * y(i) * (1 - y(i))
      b(i) += learningRate * bias / nx
      bias
    }

    W.zipWithIndex.map { case (w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        W(i)(j) += learningRate * (tmpBias(i) * c(j) + tmpVisibleBias(j) * y(i)) / nx
      }
    }   
  }

  def train(xs: Array[Array[Double]]) = {
    xs.map(trainOne(_))
  }

  def loss(xs: Array[Array[Double]]) = {
    val corruptXs = xs.map(corrupt(_))
    val ys = corruptXs.map(getHidden(_))
    val zs = ys.map(reconstruct(_))

    val entrophies = xs.zipWithIndex.map { case (e, i) =>
      - e.zipWithIndex.map { case (x, j) =>
        x * math.log(zs(i)(j)) + (1 - x) * math.log(1 - zs(i)(j))
      }.sum
    }

    entrophies.sum / entrophies.size
  }
}

object dA {
  def testdA(epochs: Int) {

    val xs: Array[Array[Double]] = Array(
      Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0)
    )

    val da: dA = new dA(20, 5, 0.1)

    for(i <- 0 until epochs) {
      da.train(xs)
    }

    val testXs: Array[Array[Double]] = Array(
      Array(1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0)
    )

    val reconstructedXs = testXs.map(t => da.reconstruct(da.getHidden(t)))

    println(array2D(reconstructedXs))
  }
}
