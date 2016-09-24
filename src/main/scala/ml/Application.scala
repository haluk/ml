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

  def exportRules(root: Node, acc: mutable.StringBuilder, prefix: mutable.StringBuilder): String = {
    if (root.isInstanceOf[Node]) {
      for (i <- root.children) {
        var rule = new mutable.StringBuilder()
        rule.append(root.name)
        if (i._2.isInstanceOf[Node]) {
          exportRules(i._2.asInstanceOf[Node], acc, prefix.append(root.name + "=" + i._1 + " and "))
        }
        else {
          rule.append("=" + i._1 + " -> " + i._2.asInstanceOf[Leaf].name + "\n")
          acc.append(prefix.toString + rule.toString)
        }
      }
      prefix.clear()
      acc.toString()
    }

    return acc.toString()
  }

  def main(args: Array[String]): Unit = {
    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))
    val id3 = Id3(properties)
    id3.summary()

    var attributes = id3.properties.getProperty("attributes").split(",").map(x => x.trim)
    attributes = attributes.filter(p => p != attributes(id3.classIndex))

    val root: Tree = id3.buildTree(id3.getData(), id3.getData(), attributes)

    traverse(root, parentAttribute = root)

    val rules = exportRules(root.asInstanceOf[Node], new mutable.StringBuilder(), new mutable.StringBuilder(""))
    //    println(root.asInstanceOf[Node].children)
    println(rules)

  }


}
