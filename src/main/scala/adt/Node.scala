package adt

/**
  * Created by hd on 9/6/16.
  */
sealed abstract class Tree

case class Node(children: List[Tree]) extends Tree

case class Leaf[A](data: A) extends Tree

case object EmptyLeaf extends Tree