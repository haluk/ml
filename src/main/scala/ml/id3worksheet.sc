import util.math.Purity

val outlook = List("sunny", "sunny", "sunny", "sunny", "sunny",
  "rainy", "rainy", "rainy", "rainy", "rainy",
  "overcast", "overcast", "overcast", "overcast")

val play = List("no", "no", "no", "yes", "yes", "no", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes")

outlook.size
play.size

(outlook zip play)
val grouped = (outlook zip play).groupBy(x => x._1).mapValues(v => v.map(i => i._2))
val impurities = List()

impurity(outlook.toArray, play.toArray)
val tmp = List("sunny", "rainy", "overcast", "sunny", "overcast")
impurities.::(1)
print(impurities)
val data = Array(Array("a", "b", "c", "d", "true"), Array("a", "e", "f", "g", "true"), Array("ab", "h", "j", "k", "false"))
tmp.toSet[String].map(x => x.charAt(0))
val droptest = Array(9, 5, 2, 0, 4)

data.zipWithIndex.filter(r => r._1(0) == "a").foreach(i => i._1.mkString(","))
val columnIndex = 3

def impurity(attr: Array[String], classLabel: Array[String]): Double = {
  val grouped = (attr zip classLabel).groupBy(x => x._1)
    .mapValues(v => v.map(i => i._2))
  val entropies = grouped.map { case (k, v) => (k, Purity.entropy(v), v.size) }
  entropies.map { case (k, e, s) => (k, e * (s / classLabel.length.toDouble)) }.map(_._2).sum
}
(droptest.take(columnIndex) ++ droptest.drop(columnIndex + 1)).mkString(",")

