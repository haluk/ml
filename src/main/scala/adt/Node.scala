package adt

/**
  * Created by hd on 9/6/16.
  */
sealed abstract class Tree

case class Node(name: String, entropy: Double, impurity: Double,
                labelCounts: Map[String, Int], children: Iterable[(String, Tree)]) extends Tree

case class Leaf(name: String, entropy: Double = 0.0,
                labelCounts: Map[String, Int], decision: String) extends Tree
