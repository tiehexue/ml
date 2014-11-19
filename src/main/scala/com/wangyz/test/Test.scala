package com.wangyz.test

import org.apache.commons.collections4.queue.CircularFifoQueue

import com.wangyz.util.Util._
import com.wangyz.util.TimerTrait
import com.wangyz.alg._

object Test {
  
  val ((trainx, trainy), (validationx, validationy), (testx, testy)) = loadData

  private def test(cls: AbstractLearning, learningRate: Double, epochs: Int) = {
        
    cls.learningRate = learningRate
    
    val losses = new CircularFifoQueue[Double](10)

    for(j <- 0 until epochs) {
      val i = j % 1
      cls.train(trainx(i), trainy(i))
      //val l = classifier.loss(trainx(i), trainy(i))
      //val e = classifier.error(trainx(i), trainy(i))
      val l = cls.loss(validationx(i), validationy(i))
      val e = cls.error(testx(i), testy(i))
      losses.add(e)
      val lossArray = losses.toArray.map(x => x.asInstanceOf[Double])
      if (lossArray.min < e) {
        cls.learningRate = cls.learningRate * 0.9
      }

      val lr = cls.learningRate
      println(f"Loss is $l%2.4f, error is $e%2.4f, learning rate is $lr%2.4f")
    }
  }

  def testLogistic(learningRate: Double, epochs: Int) = {
    
    val classifier = new LogisticRegression(trainx.head.head.size, trainy.head.head.size) with TimerTrait
    test(classifier, learningRate, epochs)
  }

  def testMLP(learningRate: Double, epochs: Int) = {
    
    val classifier = new MLP(trainx.head.head.size, 500, trainy.head.head.size) with TimerTrait
    test(classifier, learningRate, epochs)
  }
}