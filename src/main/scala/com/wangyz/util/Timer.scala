package com.wangyz.util

object Timer {
  
  def time[T](str: String)(f: => T) = {
    val start = System.nanoTime
    val t = f
    val end = System.nanoTime
    val last = (end - start) / 1000 + "ms"
    println(s"Timing of $str: $last")
    
    t
  }
}