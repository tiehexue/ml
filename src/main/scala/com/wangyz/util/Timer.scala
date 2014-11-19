package com.wangyz.util

object Timer {
  
  def time[T](str: String, appendResult: Boolean = false)(f: => T) = {
    val start = System.nanoTime
    val t = f
    val end = System.nanoTime
    val last = (end - start) / 1000 + "ms"
    val resultStr = if(appendResult) "; and the result is: " + t else ""
    println(s"Timing of $str: $last" + resultStr)
  
    t
  }
}