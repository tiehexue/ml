package com.wangyz.alg

object Knapsack {
  case class Item(w: Int, v: Int)

  var recursiveTimes = 0

  val items = List(
    Item(9, 150), Item(13, 35), Item(153, 200), Item(50, 160),
    Item(15, 60), Item(68, 45), Item(27, 60), Item(39, 40), Item(23, 30),
    Item(52, 10), Item(11, 70), Item(32, 30), Item(24, 15),
    Item(48, 10), Item(73, 40), Item(42, 70), Item(43, 75), Item(22, 80),
    Item(7, 20), Item(18, 12), Item(4, 50), Item(30, 10),Item(52, 10), Item(11, 70), Item(32, 30), Item(24, 15),
    Item(48, 10), Item(73, 40), Item(42, 70), Item(43, 75), Item(22, 80),
    Item(7, 20), Item(18, 12), Item(4, 50), Item(30, 10)).sortWith((a, b) => a.w < b.w)
 
  def bad(weight: Int, sumW: Int, sumV: Int, dest: List[Item], src: List[Item]): (Int, Int, Int, List[Item], List[Item]) = {
    recursiveTimes = recursiveTimes + 1;
    // println("doing: " + recursiveTimes)
    val restWeight = weight - sumW
  
    if (src.isEmpty || src.head.w > restWeight) {
      return (weight, sumW, sumV, dest, src)
    } else {
      val best = src.head
      val rests = src.drop(1)
      val x = bad(weight, sumW + best.w, sumV + best.v, dest :+ best, rests)
      val y = bad(weight, sumW, sumV, dest, rests)  

      return if (x._3 > y._3) x else y
    }
  }

  def dp(sum: Int) = {
    val m = Array.ofDim[(Int, Seq[Int])](items.size + 1, sum + 1)

    for (i <- (0 to sum)) 
      m(0)(i) = (0, Seq[Int]())

    for (i <- (1 to items.size)) {
      for (j <- (0 to sum)) {
        if (items(i - 1).w <= j)
        {
          val a = m(i-1)(j)._1
          val b = m(i-1)(j-items(i - 1).w)._1 + items(i - 1).v
          if (a >= b) {
            m(i)(j) = (a, m(i-1)(j)._2)
          } else {  
            m(i)(j) = (b, m(i-1)(j - items(i - 1).w)._2 :+ (i - 1))  
          }
        }
        else {
          m(i)(j) = (m(i-1)(j)._1, m(i-1)(j)._2)
        }
      }
    }

    m(items.size)(sum)
  }

  def doIt(sum: Int) = {
    val x = bad(sum, 0, 0, List[Item](), items)
    println("Recursive Times: " + recursiveTimes + ", total: " + x._4.map(_.v).sum + ", lists: " + x._4.map(y => "(" + y.w + ", " + y.v + ")").mkString(","))

    val m = dp(sum)

    println("Max value is: " + m._1 + ": " + m._2 + m._2.map(x => "(" + items(x).w + ", " + items(x).v + ")").mkString(","))
  }

}