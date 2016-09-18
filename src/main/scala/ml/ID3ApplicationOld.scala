package ml

import java.io.File

import com.github.tototoshi.csv.CSVReader
import util.math.Purity

/**
  * Created by hd on 9/6/16.
  */
object ID3ApplicationOld {

  def main(args: Array[String]): Unit = {
    //    val treeA = Node(List(EmptyLeaf, Leaf(5)))
    //    val treeB = Node(List(Node(List(Leaf(2), Leaf(3))), Leaf(5)))
    //    println("Tree A: " + treeA)
    //    println("Tree B: " + treeB)

    // Reading training data
    val reader = CSVReader.open(new File("weather.csv"))
    val training = reader.allWithHeaders()
    val className = "play"
    val features = training(0).keySet - className


    println("Class Name: " + className)
    println("Features: " + features.mkString(", "))

    val featureVals = (features.map(f => (f, training.map(x => (x(f), x(className))))))
    println(featureVals)
    val groupedFeatureVals = featureVals.map(f => (f._1, f._2.groupBy(x => x)))
    println(groupedFeatureVals)
    val groupedFeatureValsSplitted = groupedFeatureVals.map(gf => (gf._1, gf._2.values.flatten.groupBy(_._1)))
    println
    groupedFeatureValsSplitted.foreach(println)
    val impurities = groupedFeatureValsSplitted.map(gfs => (gfs._1, impurity(gfs._2.values)))
    val root = impurities.minBy(_._2)
    print("root: " + root)


  }

  def impurity(labels: Iterable[Iterable[(String, String)]]): Double = {
    val length = labels.flatten.size
    val entropies = labels.map(x => Purity.entropy(x.map(e => e._2)))
    labels.zip(entropies).foldLeft(0.0)((total, cur) => total + cur._1.size.toDouble / length * cur._2)
  }

}
