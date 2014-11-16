package com.wangyz

import test._
import util.Util._

object Main {

  def main(args: Array[String]) {

    val (s1, d1) = dATest.testdA(0.15, 20, 0.2)
    toFile(s1, "src1.txt")
    toFile(d1, "new1.txt")

    val (s2, d2) = dATest.testdA(0.15, 50, 0.1)
    toFile(s2, "src2.txt")
    toFile(d2, "new2.txt")
  }
}