package ml

import java.io.File
import java.util.Properties
import java.util.logging.Logger

import adt.{Data, Leaf, Node, Tree}
import util.math.Purity

/**
  * Created by hd on 9/16/16.
  */
case class Id3(properties: Properties) {

  val LOG = Logger.getLogger(Id3.getClass.getName)

  val file = new File(properties.getProperty("dataset.path"))
  val hasHeader = properties.getProperty("header").toBoolean
  val delim = properties.getProperty("delimiter")
  val classIndex = properties.getProperty("class.index").toInt

  def summary(): Unit = {
    val data = getData()
    val classLabel = data.header(classIndex)
    val attributes = data.header.filter(p => p != classLabel)
    val summaryMsg = "Number of Rows: %d, Number of Columns: %d, Class Label: %s"
      .format(data.records.size, data.header.size, classLabel)
    LOG.info(summaryMsg)
    val attributesMsg = "Attributes: %s".format(attributes.mkString(", "))
    LOG.info(attributesMsg)
  }

  def getData(): Data = {
    val records = scala.io.Source.fromFile(file).getLines().toArray.map(row => row.split(delim).map(cell => cell.trim))
    hasHeader match {
      case true => new Data(records.head, records.tail.map(r => r.take(classIndex)), records.map(r => r(classIndex)))
      case false => new Data(properties.getProperty("attributes").split(delim), records.map(r => r.take(classIndex)), records.map(r => r(classIndex)))
    }

  }

  def subsetData(data: Data, columnIndex: Int, columnVal: String): Data = {
    val filtered = data.records.zipWithIndex.filter(r => r._1(columnIndex).equals(columnVal))
    new Data(data.header, filtered.map(i => i._1.take(columnIndex) ++ i._1.drop(columnIndex + 1)), filtered.map(i => i._2).map(data.classVal))
  }

  def buildTree(training: Data, pruning: Data, attributes: Array[String]): Tree = {
    println(attributes.mkString(","))
    println(training.records.deep.mkString(","))
    val labelCounts = training.classVal.groupBy(identity).mapValues(_.size)
    if (attributes.size == 0 || training.classVal.toSet.size == 1) // Case 1 [Needs refactoring]
      return Leaf(name = "", labelCounts = labelCounts, decision = "")
    else {
      val impurities = new Array[Double](attributes.size)
      println("\t=======>")
      (0 until attributes.size).foreach(c => impurities(c) = Purity.impurity(training.records.map(r => r(c)), training.classVal))
      val selectedAttribute = impurities.zipWithIndex.min
      val name = attributes(selectedAttribute._2)
      //      val entropy = Purity.entropy(columnValues(training, selectedAttribute._2))
      val entropy = Purity.entropy(training.classVal)
      val impurity = selectedAttribute._1
      println(attributes.zip(impurities).mkString(","))
      println("====")


      return Node(name, entropy, impurity, labelCounts, "",
        columnValues(training, selectedAttribute._2).toSet[String]
          .map(d => (d, buildTree(subsetData(training, selectedAttribute._2, d), pruning, attributes.filter(p => p != name)))))

      //      return Node(name, entropy, impurity, labelCounts, List())


    }
  }

  def columnValues(data: Data, index: Int): Array[String] = {
    data.records.map(r => r(index))
  }

}
