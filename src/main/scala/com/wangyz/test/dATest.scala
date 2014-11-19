package com.wangyz.test

// activator -J-Xms2048m -J-Xmx3072m -J-XX:+CMSClassUnloadingEnabled

import com.wangyz.util.Util._
import com.wangyz.util.TimerTrait
import com.wangyz.alg.dA

object dATest {
  
  val ((trainx, trainy), (validationx, validationy), (testx, testy)) = loadData

  def testdA(hiddenNodes: Int, learningRate: Double, epochs: Int, corruptLevel: Double) = {
    
    val classifier = new dA(trainx.head.head.size, hiddenNodes, corruptLevel) with TimerTrait
    classifier.learningRate = learningRate

    (0 until epochs).foreach{ i => 
      classifier.train(trainx(0))
      classifier.loss(trainx(0))
      classifier.learningRate = classifier.learningRate * 0.9
    }
    
    val srcData = trainx(0).take(100)    
    val newData = srcData.map(s => classifier.reconstruct(classifier.getHidden(s))).toArray
    
    (srcData, newData)
  }
}