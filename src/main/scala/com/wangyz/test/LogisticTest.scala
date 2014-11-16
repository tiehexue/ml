package com.wangyz.test

import org.apache.commons.collections4.queue.CircularFifoQueue

import com.wangyz.util.Util._
import com.wangyz.alg.LogisticRegression

object LogisticTest {
  
  val ((trainx, trainy), (validationx, validationy), (testx, testy)) = loadData

  def testLogistic(learningRate: Double, epochs: Int) = {
    
    val classifier = new LogisticRegression(trainx.head.head.size, trainy.head.head.size)
    classifier.learningRate = learningRate
    
    val losses = new CircularFifoQueue[Double](10)

    for(j <- 0 until epochs) {
      val i = j % 1
      classifier.train(trainx(i), trainy(i))
      //val l = classifier.loss(trainx(i), trainy(i))
      //val e = classifier.error(trainx(i), trainy(i))
      val l = classifier.loss(validationx(i), validationy(i))
      val e = classifier.error(testx(i), testy(i))
      losses.add(e)
      val lossArray = losses.toArray.map(x => x.asInstanceOf[Double])
      if (lossArray.min < e) {
        classifier.learningRate = classifier.learningRate * 0.9
      }

      val lr = classifier.learningRate
      println(f"Loss is $l%2.4f, error is $e%2.4f, learning rate is $lr%2.4f")
    }
  }
}