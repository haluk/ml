package ml

import adt.{EmptyLeaf, Leaf, Node}

/**
  * Created by hd on 9/6/16.
  */
object ID3Application {
  def main(args: Array[String]): Unit = {
    val treeA = Node(List(EmptyLeaf, Leaf(5)))
    val treeB = Node(List(Node(List(Leaf(2), Leaf(3))), Leaf(5)))
    println("Tree A: " + treeA)
    println("Tree B: " + treeB)
  }

}
