package ml

import java.io.{File, FileReader}
import java.util.Properties

import adt.Tree


/**
  * Created by hd on 9/10/16.
  */
object Application {


  /*
    def summary(training: (List[String], CsvData)) = {
      println("Attributes: " + training._1.mkString(", "))
      println("Number of instances: " + training._2.size)
    }

    def printCsv(data: (List[String], CsvData)) = {
      data._2.foreach(x => println(x.mkString(",")))
    }

    val trial = new Id3DecisionTreeoldAttempt
    val training = trial.readCsv("/home/hd/IdeaProjects/ml/test1_yes.csv", null)
  //  val training = trial.readCsv("/home/hd/IdeaProjects/ml/test1_no.csv", null)
  //  val training = trial.readCsv("/home/hd/IdeaProjects/ml/test2.csv", null)
    val label = "play"
    val labelIdx = training._1.indexOf(label)
  //  summary(training)
  //  printCsv(training)

  //  println(training._2(0))
    trial.buildTree(training, training._2.toList.map(x => x(labelIdx)), label)
  */

  def main(args: Array[String]): Unit = {
    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))
    val id3 = Id3(properties)
    id3.summary()

    var attributes = id3.properties.getProperty("attributes").split(",").map(x => x.trim)
    attributes = attributes.filter(p => p != attributes(id3.classIndex))

    val root: Tree = id3.buildTree(id3.getData(), id3.getData(), attributes)
    println(root)
  }


}
