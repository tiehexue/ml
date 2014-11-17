package com.wangyz.alg

object Knapsack {
  case class Item(w: Int, v: Int)

  val items = List(
    Item(9, 150), Item(13, 35), Item(153, 200), Item(50, 160),
    Item(15, 60), Item(68, 45), Item(27, 60), Item(39, 40), Item(23, 30),
    Item(52, 10), Item(11, 70), Item(32, 30), Item(24, 15),
    Item(48, 10), Item(73, 40), Item(42, 70), Item(43, 75), Item(22, 80),
    Item(7, 20), Item(18, 12), Item(4, 50), Item(30, 10)).sortWith((a, b) => a.w < b.w)
 
  def find(recursiveTimes: Int, weight: Int, sumW: Int, sumV: Int, dest: List[Item], src: List[Item]): (Int, Int, Int, Int, List[Item], List[Item]) = {
    val times = recursiveTimes + 1;

    val restWeight = weight - sumW
  
    if (src.isEmpty || src.head.w > restWeight) {
      return (times, weight, sumW, sumV, dest, src)
    } else {
      val best = src.head
      val rests = src.takeRight(src.size - 1)
      val x = find(times, weight, sumW + best.w, sumV + best.v, dest :+ best, rests)
      val y = find(times, weight, sumW, sumV, dest :+ best, rests)  

      return if (x._3 > y._3) x else y
    }
  }

  def doIt(sum: Int) = {
    val x = find(0, sum, 0, 0, List[Item](), items)
    println("Recursive Times: " + x._1 + ", lists: " + x._5.map(y => "(" + y.w + ", " + y.v + ")").mkString(","))
  }
}