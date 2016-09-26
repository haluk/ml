package ml

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.util.Properties
import java.util.logging.Logger

import adt.{Data, Leaf, Node, Tree}
import util.math.Purity

import scala.util.Random

/**
  * Created by zeynep and haluk on 9/16/16.
  */
case class Id3(properties: Properties) {

  val LOG = Logger.getLogger(Id3.getClass.getName)

  val file = new File(properties.getProperty("dataset.path"))
  val hasHeader = properties.getProperty("header").toBoolean
  val delim = properties.getProperty("delimiter")
  val classIndex = properties.getProperty("class.index").toInt
  val rowIndex = properties.getProperty("row.index").toInt


  def summary(data: Data): Unit = {
    val classLabel = data.header(classIndex)
    val attributes = data.header.filter(p => p != classLabel)
    val summaryMsg = "Number of Rows: %d, Number of Columns: %d, Class Label: %s"
      .format(data.records.size, data.header.size, classLabel)
    LOG.info(summaryMsg)
    val attributesMsg = "Attributes: %s".format(attributes.mkString(", "))
    LOG.info(attributesMsg)
  }

  def getData(): Data = {
    val records = getRecords()
    hasHeader match {
      case true => new Data(records.head, records.tail.map(r => r.patch(classIndex, Nil, 1)), records.map(r => r(classIndex)))
      case false => new Data(properties.getProperty("attributes").split(delim), records.map(r => r.patch(classIndex, Nil, 1)), records.map(r => r(classIndex)))
    }
  }

  def getRecords(): Array[Array[String]] = {
    if (rowIndex != -1)
      return scala.io.Source.fromFile(file).getLines().toArray.map(row => row.trim.split(delim).patch(rowIndex, Nil, 1).map(cell => cell.trim))
    else
      return scala.io.Source.fromFile(file).getLines().toArray.map(row => row.trim.split(delim).map(cell => cell.trim))
  }

  def exportToFile(data: Data, ext: String): Unit = {
    val dataToFile = new Data(data.header, data.records, data.classVal)
    val fileName = file.getName.split("\\.")(0) + "." + ext
    LOG.info(fileName + " is written")
    dataToFile.classVal.zipWithIndex.foreach(i => dataToFile.records.update(i._2, dataToFile.records(i._2).patch(classIndex, List(i._1), 0)))

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))

    if (hasHeader)
      writer.write(dataToFile.header.mkString(delim) + "\n")

    dataToFile.records.foreach(r => writer.write(r.mkString(delim) + "\n"))
    writer.close()
  }

  def splitDataSet(data: Data, size: Int): (Data, Data) = {
    val testIndices = Random.shuffle((0 until data.records.size).toList).take(size)
    val trainingIndices = (0 until data.records.size).filterNot(testIndices.toSet)
    val testData = Data(data.header, testIndices.map(data.records).toArray, testIndices.map(data.classVal).toArray)
    val trainingData = Data(data.header, trainingIndices.map(data.records).toArray, trainingIndices.map(data.classVal).toArray)

    return (trainingData, testData)
  }

  def subsetData(data: Data, columnIndex: Int, columnVal: String): Data = {
    val filtered = data.records.zipWithIndex.filter(r => r._1(columnIndex).equals(columnVal))
    new Data(data.header, filtered.map(i => i._1.take(columnIndex) ++ i._1.drop(columnIndex + 1)), filtered.map(i => i._2).map(data.classVal))
  }

  def buildTree(training: Data, pruning: Data, attributes: Array[String]): Tree = {
    val labelCounts = training.classVal.groupBy(identity).mapValues(_.size)
    if (attributes.size == 0 || training.classVal.toSet.size == 1)
      return Leaf(name = labelCounts.maxBy(_._2)._1, labelCounts = labelCounts)
    else {
      val impurities = new Array[Double](attributes.size)
      (0 until attributes.size).foreach(c => impurities(c) = Purity.impurity(training.records.map(r => r(c)), training.classVal))
      val selectedAttribute = impurities.zipWithIndex.min
      val name = attributes(selectedAttribute._2)
      val entropy = Purity.entropy(training.classVal)
      val impurity = selectedAttribute._1

      return Node(name, entropy, impurity, labelCounts,
        columnValues(training, selectedAttribute._2).toSet[String]
          .map(d => (d, buildTree(subsetData(training, selectedAttribute._2, d), pruning, attributes.filter(p => p != name)))))
    }
  }

  def columnValues(data: Data, index: Int): Array[String] = {
    data.records.map(r => r(index))
  }


}
