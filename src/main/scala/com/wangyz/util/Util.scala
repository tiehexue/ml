package com.wangyz.util

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.collection.mutable.WrappedArray

object Util {

  def get[T](index: Int, others: Seq[Any]) = {
    val x = others(index).asInstanceOf[WrappedArray[T]]
    x.head
  }

  def array2D(x: Array[Array[Double]]) = {
    "[\n" + x.map("  " + array1D(_)).mkString("\n") + "\n]"
  }

  def array1D(x: Array[Double]) = {
    "[" + x.mkString(", ") + "]"
  }

  def toFile(x: Array[Array[Double]], fileName: String) = {
    val writer = new PrintWriter(new File(fileName))
    x.map(i => writer.write(i.mkString(", ") + "\n"))
    writer.close()
  }

  def loadData = {

    val train_num = 6000
    val validation_num = 2000
    val test_num = 2000
    val batch_num = 1

    val train_batch_size = train_num / batch_num
    val validation_batch_size = validation_num / batch_num
    val test_batch_size = test_num / batch_num

    val yDim = 10

    val lines = Source.fromFile(new File("10000.txt")).getLines.toArray
    val head = lines.head.split(",")

    val trainx = Array.ofDim[Double](batch_num, train_batch_size, head.size)
    val validationx = Array.ofDim[Double](batch_num, validation_batch_size, head.size)
    val testx = Array.ofDim[Double](batch_num, test_batch_size, head.size)

    val nums = Source.fromFile(new File("10000y.txt")).getLines.toArray.head.split(",")
    val trainy = Array.ofDim[Double](batch_num, train_batch_size, yDim)
    val validationy = Array.ofDim[Double](batch_num, validation_batch_size, yDim)
    val testy = Array.ofDim[Double](batch_num, test_batch_size, yDim)

    lines.zipWithIndex.map{ case (l, i) =>
      l.split(",").zipWithIndex.map{ case (w, j) =>
        if (i < train_num) {
          trainx(i / train_batch_size)(i % train_batch_size)(j) = w.toDouble
        } else if (i < train_num + validation_num) {
          validationx((i - train_num) / validation_batch_size)((i - train_num) % validation_batch_size)(j) = w.toDouble
        } else {
          testx((i - (train_num + validation_num)) / test_batch_size)((i - (train_num + validation_num)) % test_batch_size)(j) = w.toDouble         
        }
      }

      if (i < train_num) {
        trainy(i / train_batch_size)(i % train_batch_size)(nums(i).toInt) = 1
      } else if (i < train_num + validation_num) {   
        validationy((i - train_num) / validation_batch_size)((i - train_num) % validation_batch_size)(nums(i).toInt) = 1
      } else {
        testy((i - (train_num + validation_num)) / test_batch_size)((i - (train_num + validation_num)) % test_batch_size)(nums(i).toInt) = 1
      }
    }

    ((trainx, trainy), (validationx, validationy), (testx, testy))
  }
}