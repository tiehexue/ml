package com.wangyz.util

import com.wangyz.alg.DeepLearningAlg

trait TimerTrait extends DeepLearningAlg {
  
  private def output(start: Long) = {
    val end = System.nanoTime

    val ms = (end - start) / 1000

    val last = if (ms > 1000 * 1000) {
      ms / 1000 * 1000 + "s"
    } else {
      ms + "ms"
    }

    val className = this.getClass.getName + ".train"

    println(s"Timing of $className : $last")
  }

  def timing(f: => Unit) = {
    val start = System.nanoTime
    val t = f
    output(start)
  }

  abstract override def train(xs: Array[Array[Double]], others: Any*) = {
    timing(super.train(xs, others)) 
  }
}