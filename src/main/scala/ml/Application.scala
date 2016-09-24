package ml

import java.io.{File, FileReader}
import java.util.Properties

import adt.{Leaf, Node, Tree}

import scala.collection.mutable


/**
  * Created by hd on 9/10/16.
  */
object Application {

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

  def main(args: Array[String]): Unit = {
    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))
    val id3 = Id3(properties)
    id3.summary()

    var attributes = id3.properties.getProperty("attributes").split(",").map(x => x.trim)
    attributes = attributes.filter(p => p != attributes(id3.classIndex))

    val root: Tree = id3.buildTree(id3.getData(), id3.getData(), attributes)
    //    println(root)
    //    val B = new Leaf("B", 0.0, Map("yes"-> 2, "no" -> 3))
    //    val C = new Leaf("C", 0.0, Map("yes"-> 22, "no" -> 33))
    //    val D = new Leaf("D", 0.0, Map("yes"-> 222, "no" -> 333))
    //    val F = new Leaf("F", 0.0, Map("yes" -> 2222, "no" -> 3333))
    //    val E = new Node("E", 0.1, 0.11, Map("yes" -> 0, "no" -> 1), List(("A5", F)))
    //    val A = new Node("A", 0.9940, 0.7872, Map("yes" -> 12, "no" -> 10), List(("A1", B), ("A2", C), ("A3", D), ("A4", E)))
    //    println(A)
    //    println(exportRules(A, ""))
    println(exportRules(root.asInstanceOf[Node], ""))



    //    traverse(root, parentAttribute = root)

    //    val rules = exportRules(root.asInstanceOf[Node], new mutable.StringBuilder(), new mutable.StringBuilder(""))
    //    exportRules(root, parentAttribute = root)
    //    println(root.asInstanceOf[Node].children)
    //    println(rules)
  }


}
