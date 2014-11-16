package com.wangyz.alg

object KnnKmeans {

  val training = Map((1, 2) -> 1, (2, 3) -> 2, (4, 5) -> 0, (2, 5) -> 1, (3, 5) -> 1, (7, 10) -> 0, (18, 9) -> 2, (10, 12) -> 0, (24, 34) -> 9, (100, 102) -> 1, (7, 9) -> 0)
  val test = (3, 7)
  val training2 = training.keys

  def distance(a: (Int, Int), b: (Int, Int)) = {
    val dx = a._1 - b._1
    val dy = a._2 - b._2 

    dx * dx + dy * dy
  }

  def onenn = {
    val distances = training map { case (k, v) => distance(test, k) -> k }

    training(distances.toSeq.sortBy(_._1).head._2)
  }

  def knn(k: Int = 4) = {
    val distances = training map { case (k, v) => distance(test, k) -> k }

    val t = distances.toSeq.sortBy(_._1).take(k).map(x => training(x._2))
    
    t.map(x => x -> t.count(_ == x)).sortBy(_._2).last._1
  }

  def clustering(centers: Seq[(Int, Int)]): Map[(Int, Int), Iterable[(Int, Int)]] = {
    println("clustering...")

    val distances = training2 map { t => 
      val tmp = centers.map(c => distance(t, c) -> c)
      (tmp.toSeq.sortBy(_._1).head._2, t)
    }

    val result = distances.groupBy(_._1).mapValues(_.map(_._2))
    val newCenters = result.values.map(v => findCenter(v.toSeq)).toSeq
    
    if (centers.toSet != newCenters.toSet) 
      clustering(newCenters)
    else
      result
  }

  def findCenter(p: Seq[(Int, Int)]): (Int, Int) = {
    ((p.map(_._1).sum / p.size), (p.map(_._2).sum / p.size))
    //val distances: Map[Int, (Int, Int)] = p.map(x => p.map(y => distance(x, y)).sum -> x).toMap
    //distances(distances.toSeq.sortBy(_._1).head._1)
  }

  def kmeans(k: Int = 2) = {
    val centers = training.keys.take(k).toSeq
    clustering(centers)
  }

  def testK() = {
    println(onenn)
    println(knn())
    println(kmeans())
  }
}
