package ml

import java.io.{File, FileReader}
import java.util.Properties
import java.util.logging.Logger

import adt.{Data, Leaf, Node, Tree}

import scala.collection.mutable


/**
  * Created by hd on 9/10/16.
  */
object Application {
  val LOG = Logger.getLogger(Application.getClass.getName)

  def traverse(root: Tree, decision: String = "", depth: Int = 0, parentAttribute: Tree): Unit = root match {
    case root: Node => {
      var msg: String = "Decide-on = %s, H = %f, I = %f, (%s)".format(root.name, root.entropy, root.impurity, root.labelCounts.mkString(", "))
      if (!decision.equals("")) {
        msg = parentAttribute.asInstanceOf[Node].name + " = " + decision + ", " + msg
      }
      print("\t" * depth)
      println(msg)
      if (!root.children.isEmpty)
        root.children.foreach(c => traverse(c._2, c._1, depth + 1, root))
    }
    case root: Leaf => {
      print("\t" * depth)
      val msg: String = parentAttribute.asInstanceOf[Node].name + " = %s, %s, H = %f, (%s)"
        .format(decision, root.name, root.entropy, root.labelCounts.mkString(", "))
      println(msg)
    }
  }

  def exportRules(root: Node, tag: String): String = {
    val ruleSet = new mutable.StringBuilder()

    for (child <- root.children) {
      if (child._2.isInstanceOf[Leaf])
        ruleSet.append(tag + root.name + "=" + child._1 + " and " + child._2.asInstanceOf[Leaf].name + "\n")
      else {
        ruleSet.append(exportRules(child._2.asInstanceOf[Node], tag + root.name + "=" + child._1 + " and "))
      }
    }
    return ruleSet.toString()
  }

  // (Int, Int) ==> (true, false)

  def isMatch(root: Tree, testRec: (Array[String], String), attributes: Array[String], classIndex: Int): (Int, Int) = {
    if (root.isInstanceOf[Leaf]) {
      if (root.asInstanceOf[Leaf].name.equals(testRec._2))
        return (1, 0)
      else
        return (0, 1)
    }
    else {
      val rootNode = root.asInstanceOf[Node]
      val i = attributes.indexOf(rootNode.name)
      val children = rootNode.children.filter(c => c._1.equals(testRec._1(i)))
      if (children.size != 0)
        return isMatch(rootNode.children.filter(c => c._1.equals(testRec._1(i))).toList(0)._2, testRec, attributes, classIndex)
      else {
        LOG.info("Unseen attribute value. Using majority rule to classify")
        val majorityRule = rootNode.labelCounts.maxBy(_._2)._1
        if (majorityRule.equals(testRec._2))
          return (1, 0)
        else
          return (0, 1)

      }
    }
  }

  def testAccuracy(root: Tree, test: Data, attributes: Array[String], classIndex: Int): Array[(Int, Int)] = {
    return test.records.zip(test.classVal).map(r => isMatch(root, r, attributes, classIndex))
  }

  def main(args: Array[String]): Unit = {
    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))
    val id3 = Id3(properties)

    var attributes = id3.properties.getProperty("attributes").split(id3.delim).map(x => x.trim)
    attributes = attributes.filter(p => p != attributes(id3.classIndex))

    val data = id3.getData()
    val (training, test) = id3.splitDataSet(data, id3.properties.getProperty("test.size").toInt)
    val (trainingPrime, validation) = id3.splitDataSet(training, id3.properties.getProperty("validation.size").toInt)
    id3.summary(trainingPrime)

    val root: Tree = id3.buildTree(trainingPrime, trainingPrime, attributes)
    val tfs = testAccuracy(root, test, attributes, id3.classIndex).fold((0, 0))((i, j) => (i._1 + j._1, i._2 + j._2))
    val accuracy = tfs._1 / (tfs._1 + tfs._2).toDouble
    LOG.info("Accuracy: " + accuracy)
    id3.exportToFile(trainingPrime, "training")
    id3.exportToFile(test, "test")
    id3.exportToFile(validation, "validation")
    //    traverse(root, parentAttribute = root)
    //    println(exportRules(root.asInstanceOf[Node], ""))

  }
}
