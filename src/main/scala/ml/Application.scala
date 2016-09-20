package ml

import java.io.{File, FileReader}
import java.util.Properties

import adt.{Leaf, Node, Tree}


/**
  * Created by hd on 9/10/16.
  */
object Application {

  def traverse(root: Tree, decision: String = ""): Unit = root match {
    case root: Node => {
      var msg: String = "%s, H=%f, I=%f, (%s)".format(root.name, root.entropy, root.impurity, root.labelCounts.mkString(", "))
      if (!decision.equals("")) {
        msg = decision + ", " + msg
      }
      println(msg)
      if (!root.children.isEmpty)
        root.children.foreach(c => traverse(c._2, c._1))
    }
    case root: Leaf => {
      val msg: String = "%s, %s, H=%f, (%s)"
        .format(decision, root.labelCounts.maxBy(_._2)._1, root.entropy, root.labelCounts.mkString(", "))
      println(msg)
    }

  }

  def main(args: Array[String]): Unit = {
    val properties = new Properties
    properties.load(new FileReader(new File(args(0))))
    val id3 = Id3(properties)
    id3.summary()

    var attributes = id3.properties.getProperty("attributes").split(",").map(x => x.trim)
    attributes = attributes.filter(p => p != attributes(id3.classIndex))

    val root: Tree = id3.buildTree(id3.getData(), id3.getData(), attributes)
    println(root)
    //    traverse(root.asInstanceOf[Node])
    //    root.asInstanceOf[Node].children.foreach(c => traverse(c._2, c._1))
  }


}
