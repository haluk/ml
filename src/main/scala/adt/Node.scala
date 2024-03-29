package adt

/**
  * Created by zeynep and haluk on 9/6/16.
  */
sealed abstract class Tree

case class Node(name: String, entropy: Double, impurity: Double,
                labelCounts: Map[String, Int], children: Iterable[(String, Tree)]) extends Tree

case class Leaf(name: String, entropy: Double = 0.0,
                labelCounts: Map[String, Int]) extends Tree
