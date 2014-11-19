package com.wangyz

import test._
import util.Util._

object Main {

  def main(args: Array[String]) {

    if (args.length < 5) {
      println("Aurgments not enough: hiddenNodes learningRate epochs curruptionLevel")
    }

    val (hiddenNodes, learningRate, epochs, corruptionLevel) = (
      args(1).toInt, args(2).toDouble, args(3).toInt, args(4).toDouble
    )

    val (s1, d1) = dATest.testdA(hiddenNodes, learningRate, epochs, corruptionLevel)
    toFile(s1, "src1.txt")
    toFile(d1, "new1.txt")

    val (s2, d2) = dATest.testdA(hiddenNodes * 2, learningRate, epochs, corruptionLevel * 2)
    toFile(d2, "new2.txt")

    val (s3, d3) = dATest.testdA(hiddenNodes * 2, learningRate, epochs, corruptionLevel * 2.5)
    toFile(d3, "new3.txt")

    val (s4, d4) = dATest.testdA(hiddenNodes * 2, learningRate, epochs, corruptionLevel * 3)
    toFile(d4, "new4.txt")
  }
}