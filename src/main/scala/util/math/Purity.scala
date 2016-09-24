package util.math

/**
  * Created by hd on 9/7/16.
  */
object Purity {

  def impurity(attr: Array[String], classLabel: Array[String]): Double = {
    val grouped = (attr zip classLabel).groupBy(x => x._1)
      .mapValues(v => v.map(i => i._2))
    val entropies = grouped.map { case (k, v) => (k, Purity.entropy(v), v.size) }

    entropies.map { case (k, e, s) => (k, e * (s / classLabel.length.toDouble)) }.map(_._2).sum
  }

  def entropy[T](arr: Iterable[T]): Double = {
    -1 * freqs(arr.toList).values.map(x => x * log2(x)).sum
  }

  def log2(x: Double): Double = {
    math.log(x) / math.log(2)
  }

  def freqs[T](arr: List[T]): Map[T, Double] = {
    arr.groupBy(x => x).map(x => (x._1, x._2.length / arr.length.toDouble))
  }

}
