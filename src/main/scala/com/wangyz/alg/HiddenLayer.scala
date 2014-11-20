package com.wangyz.alg

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._

case class HiddenLayer(override val nx: Int, override val ny: Int, activation: (Array[Double]) => Array[Double]) extends AbstractLearning(nx, ny) {

  def outputOne(x: Array[Double]) = {
    activation(wbx(x))
  }

  def output(xs: Array[Array[Double]]) = {
    xs.map(x => outputOne(x))
  }

  def train(xs: Array[Array[Double]], others: Any*) = {}
}