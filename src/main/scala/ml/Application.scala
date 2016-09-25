package ml

import java.io._
import java.util.Properties
import java.util.logging.Logger

import adt.{Data, Leaf, Node, Tree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


/**
  * Created by hd on 9/10/16.
  */
object Application {
  val LOG = Logger.getLogger(Application.getClass.getName)

  def writeStringToFile(msg: String, fileName: String) = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)))
    writer.write(msg + "\n")
    writer.close()
  }

  def traverse(root: Tree, decision: String = "", depth: Int = 0, parentAttribute: Tree, sb: StringBuilder): Unit = root match {
    case root: Node => {
      var msg: String = "Decide-on = %s, H = %f, I = %f, (%s)".format(root.name, root.entropy, root.impurity, root.labelCounts.mkString(", "))
      if (!decision.equals("")) {
        msg = parentAttribute.asInstanceOf[Node].name + " = " + decision + ", " + msg
      }
      sb.append("\t" * depth)
      sb.append(msg)
      sb.append("\n")
      if (!root.children.isEmpty)
        root.children.foreach(c => traverse(c._2, c._1, depth + 1, root, sb))
    }
    case root: Leaf => {
      sb.append("\t" * depth)
      val msg: String = parentAttribute.asInstanceOf[Node].name + " = %s, %s, H = %f, (%s)"
        .format(decision, root.name, root.entropy, root.labelCounts.mkString(", "))
      sb.append(msg)
      sb.append("\n")
    }
  }

  def exportRules(root: Node, tag: String): String = {
    val ruleSet = new mutable.StringBuilder()

    for (child <- root.children) {
      if (child._2.isInstanceOf[Leaf])
        ruleSet.append(tag + root.name + "=" + child._1 + " -> " + child._2.asInstanceOf[Leaf].name + "\n")
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

  def calcMatchAccuracy(precedents: Array[String], label: String, validation: Data, attributes: Array[String]): Double = {
    val attributeNames = precedents.map(i => i.split("=")(0))
    val attributeValues = precedents.map(i => i.split("=")(1)).map(i => i.trim).toList
    val attributeIndices = attributeNames.map(an => attributes.indexOf(an))
    var validationAttrVals = validation.records.zip(validation.classVal).map(v => (attributeIndices.map(v._1).toList, v._2)).toList
    val matched = validationAttrVals.filter(i => i._1.equals(attributeValues))
    val trueMatch = matched.filter(i => i._2.equals(label))
    if (matched.size == 0)
      return -1
    else
      return trueMatch.size / matched.size.toDouble
  }

  def postPruning(ruleSet: Array[String], validation: Data, attributes: Array[String]): ArrayBuffer[(List[String], String, Double)] = {
    var allPruned = new ArrayBuffer[(List[String], String, Double)]()
    for (rule <- ruleSet.zipWithIndex) {
      val precedents = rule._1.split("->")(0).split(" and ")
      val label = rule._1.split("->")(1).trim
      var prunedRules = new ArrayBuffer[String]()
      var accuracy = 0.0
      for (i <- (0 until precedents.size)) {
        accuracy = precedentsMatch(precedents, i, prunedRules, label, validation, attributes)._2
      }
      allPruned.append((prunedRules.toList.map(x => x.trim), label, accuracy))
    }
    return allPruned
  }

  def precedentsMatch(precedents: Array[String], i: Int, prunedRules: ArrayBuffer[String], label: String, validation: Data, attributes: Array[String]): (ArrayBuffer[String], Double) = {
    if (precedents.size == 1) {
      prunedRules.append(precedents.mkString(""))
      return (prunedRules, calcMatchAccuracy(precedents, label, validation, attributes))
    }
    else {
      if ((prunedRules.size + precedents.drop(i).size) == 1) {
        prunedRules.append(precedents.reverse.head.mkString(""))
        return (prunedRules, calcMatchAccuracy(prunedRules.toArray, label, validation, attributes))
      }
      else {
        val precedentsWithI = (prunedRules ++ precedents.drop(i)).toArray
        val accWithI = calcMatchAccuracy(precedentsWithI, label, validation, attributes)
        val precedentsWithOutI = (prunedRules ++ precedents.drop(i + 1)).toArray
        val accWithOutI = calcMatchAccuracy(precedentsWithOutI, label, validation, attributes)


        if (accWithI >= accWithOutI) {
          prunedRules.append(precedents(i))
          return (prunedRules, accWithI)
        }
        else {
          return (prunedRules, accWithOutI)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))
    val id3 = Id3(properties)
    val baseFileName = id3.file.getName.split("\\.")(0)

    var attributes = id3.properties.getProperty("attributes").split(id3.delim).map(x => x.trim)
    attributes = attributes.filter(p => p != attributes(id3.classIndex))

    val data = id3.getData()
    val (training, test) = id3.splitDataSet(data, id3.properties.getProperty("test.size").toInt)
    val (trainingPrime, validation) = id3.splitDataSet(training, id3.properties.getProperty("validation.size").toInt)
    id3.summary(trainingPrime)

    LOG.info("Building ID3 on training data")
    val root: Tree = id3.buildTree(trainingPrime, trainingPrime, attributes)

    val tfs = testAccuracy(root, test, attributes, id3.classIndex).fold((0, 0))((i, j) => (i._1 + j._1, i._2 + j._2))
    val accuracy = tfs._1 / (tfs._1 + tfs._2).toDouble
    LOG.info("Accuracy of ID3 on test data: " + accuracy)
    LOG.info("Extracting rules from the tree")
    val ruleSet = exportRules(root.asInstanceOf[Node], "").split("\n")

    writeStringToFile(ruleSet.mkString("\n"), baseFileName + ".unprunedRuleSet")
    LOG.info(baseFileName + ".unprunedRuleSet" + " is written")

    val sb = new mutable.StringBuilder()

    val allPruned = postPruning(ruleSet, validation, attributes)
    for (pruned <- allPruned) {
      sb.append(pruned._1.mkString(" and ") + " -> " + pruned._2 + " (Accuracy: " + pruned._3 + ")")
      sb.append("\n")
    }
    writeStringToFile(sb.toString(), baseFileName + ".prunedRuleSetUnsorted")
    LOG.info(baseFileName + ".prunedRuleSetUnsorted" + " is written")
    sb.clear()


    for (unprunedRule <- ruleSet) {
      val precedents = unprunedRule.split("->")(0).trim
      val label = unprunedRule.split("->")(1).trim
      sb.append(unprunedRule + " (Accuracy: " + calcMatchAccuracy(precedents.split(" and "), label, test, attributes) + ")")
      sb.append("\n")
    }
    writeStringToFile(sb.toString(), baseFileName + ".unprunedRuleSetTest")
    LOG.info(baseFileName + ".unprunedRuleSetTest" + " is written")
    sb.clear()

    for (pruned <- allPruned) {
      sb.append(pruned._1.mkString(" and ") + " -> " + pruned._2 + " (Accuracy: " + calcMatchAccuracy(pruned._1.toArray, pruned._2, test, attributes) + ")")
      sb.append("\n")
    }
    writeStringToFile(sb.toString(), baseFileName + ".prunedRuleSetTest")
    LOG.info(baseFileName + ".prunedRuleSetTest" + " is written")
    sb.clear()

    val allPrunedSorted = allPruned.sortWith(_._3 > _._3)
    for (x <- allPrunedSorted) {
      sb.append(x._1.mkString(" and ") + " -> " + x._2 + " (Accuracy: " + x._3 + ")")
      sb.append("\n")
    }
    writeStringToFile(sb.toString(), baseFileName + ".prunedRuleSetSorted")
    LOG.info(baseFileName + ".prunedRuleSetSorted" + " is written")
    sb.clear()

    traverse(root, parentAttribute = root, sb = sb)
    writeStringToFile(sb.toString(), baseFileName + ".tree")
    LOG.info(baseFileName + ".tree" + " is written")
    sb.clear()

    id3.exportToFile(trainingPrime, "training")
    id3.exportToFile(test, "test")
    id3.exportToFile(validation, "validation")

    //    println("Sorted By Accuracy")
    //    val allPrunedSorted = allPruned.sortWith(_._3 > _._3)
    //    for (x <- allPrunedSorted) {
    //      println(x._1 + " Label: " + x._2 + " Pruned Rule Accuracy: " + x._3)
    //    }
    //
    //    println("Testing after pruning")
    //    for(x <- allPrunedSorted) {
    //      println(x._1 + " Label: " + x._2 + " Pruned Rule Accuracy: " + calcMatchAccuracy(x._1.toArray, x._2, test, attributes))
    //    }


  }
}
