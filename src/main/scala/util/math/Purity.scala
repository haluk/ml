package util.math

/**
  * Created by hd on 9/7/16.
  */
object Purity {

  def entropy[T](arr: Array[T]): Double = {
    -1 * freqs(arr).values.map(x => x * log2(x)).sum
  }

  def log2(x: Double): Double = {
    math.log(x) / math.log(2)
  }

  def freqs[T](arr: Array[T]): Map[T, Double] = {
    arr.groupBy(x => x).map(x => (x._1, x._2.length / arr.length.toDouble))
  }

}
