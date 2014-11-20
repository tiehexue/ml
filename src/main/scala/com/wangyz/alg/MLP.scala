package com.wangyz.alg

import com.wangyz.util.Util._
import com.wangyz.util.WYMath._

case class MLP(nin: Int, nHidden: Int, nout: Int) extends AbstractLearning(0, 0) {

  val hiddenLayer = HiddenLayer(nin, nHidden, tanh)
  val logisticRegressionLayer = LogisticRegression(nHidden, nout)

  hiddenLayer.learningRate = learningRate
  logisticRegressionLayer.learningRate = learningRate

  def trainOne(x: Array[Double], y: Array[Double]) = {
    val output = hiddenLayer.outputOne(x)
    val py = logisticRegressionLayer.trainOne(output, y)

    val dy = py.zipWithIndex.map{case (e, i) => y(i) - e}

    updateW(hiddenLayer, dy.sum / nout, x)
    updateb(hiddenLayer, dy.sum / nout)
    
    updateW(logisticRegressionLayer, dy, output)
    updateb(logisticRegressionLayer, dy)
  }

  override def train(xs: Array[Array[Double]], others: Any*) = {
    val ys = get[Array[Array[Double]]](0, others)
    xs.zipWithIndex.map{ case(e, i) => 
      trainOne(e, ys(i))
    }
  }

  override def predict(x: Array[Double]) = {
    val output = wbx(hiddenLayer.W, hiddenLayer.b, x)
    val tmpY = wbx(logisticRegressionLayer.W, logisticRegressionLayer.b, output)

    softmax(tmpY)
  }
}