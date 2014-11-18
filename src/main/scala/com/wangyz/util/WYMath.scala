package com.wangyz.util

import scala.util.Random

object WYMath {

  val random = new Random(1234)

  def softmax(ys: Array[Double]) = {
    val max = ys.max
    val ys2 = ys.map(i => math.exp(i - max))
    val sum = ys2.sum
    ys2.map(_ / sum)
  }

  def uniform(min: Double, max: Double) = random.nextDouble() * (max - min) + min
  
  def binomial(n: Int, p: Double) = {

    if (p < 0 || p > 1) new Array[Double](n)
    else {
      (0 until n).map(_ => 
        if (random.nextDouble < p) 1.0
        else 0.0
      ).toArray
    }
  }

  def sigmoid(y: Double): Double = 1.0 / (1.0 + math.exp(-y))

  def sigmoid(ys: Array[Double]): Array[Double] = {
    ys.map(sigmoid(_))
  }

  def wbx(W: Array[Array[Double]], b: Array[Double], x: Array[Double]) = {
    W.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        e * x(j)
      }.sum + b(i)
    }.toArray
  }
}