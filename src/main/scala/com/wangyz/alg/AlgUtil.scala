package com.wangyz.alg

object AlgUtil {
  
  def wbx(W: Array[Array[Double]], b: Array[Double], x: Array[Double]) = {
    W.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        e * x(j)
      }.sum + b(i)
    }.toArray
  }

  def updateW(cls: AbstractLearning, dy: Any, x: Array[Double]) = {
    val delta = dy match {
      case d: Double => Array.fill[Double](cls.W.size)(d)
      case ds: Array[Double] => ds
    }

    cls.W.zipWithIndex.map{ case(w, i) =>
      w.zipWithIndex.map { case (e, j) =>
        cls.W(i)(j) = e + cls.learningRate * delta(i) * x(j) / cls.nx
      }
    }
  }

  def updateb(cls: AbstractLearning, dy: Any) = {
    val delta = dy match {
      case d: Double => Array.fill[Double](cls.W.size)(d)
      case ds: Array[Double] => ds
    }

    cls.b.zipWithIndex.map{ case (e, i) =>
      cls.b(i) = e + cls.learningRate * delta(i) / cls.nx
    }  
  }
}