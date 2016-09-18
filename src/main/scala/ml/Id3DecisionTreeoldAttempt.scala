package ml

/*Inputs: (D, A, C), where:
D is a dataset with only nominal instance attributes A
C is the class attribute

Output: a decision tree T representing a sequential decision process for
classifying instances (predicting the values of the class attribute C);
each node of T is labeled with a non-class attribute of A

Informal Inductive Bias: minimize the average height of the tree

Procedure:

if the set of remaining non-class attributes is empty
or if all of the instances in D are in the same class

return an empty tree

else {

compute the class entropy of each attribute over the dataset D
let a* be an attribute with minimum class entropy

create a root node for a tree T; label it with a*

for each value b of attribute a* {

let T(a*=b) be the tree computed recursively by ID3 on input (D|a*=b, A-a*, C), where:
D|a*=b contains all instances of D for which a* has the value b
A-a* consists of all attributes of A except a*

attach T(a*=b) to the root of T as a subtree

}

return the resulting decision tree T

http://www.cs.bc.edu/~alvarez/ML/id3

}*/


/**
  * Created by hd on 9/10/16.
  */
class Id3DecisionTreeoldAttempt {

  def readCsv(path: String, header: Array[String]) = {
    val src = io.Source.fromFile(path)
    val rows = new CsvData()
    if (header == null) {
      for (line <- src.getLines)
        rows += line.split(",").map(_.trim)
    }
    else {
      rows += header
      for (line <- src.getLines.drop(1))
        rows += line.split(",").map(_.trim)
    }
    (rows.take(1)(0).toList, rows.drop(1))
  }

  def buildTree(training: (List[String], CsvData), labelVal: List[String], label: String): Any = {
    // all of the instances in training are in the same class [test1.csv]
    // or
    // the set of remaining non-class attributes is empty [test2.csv]
    if (labelVal.toSet.size == 1 || (training._1.toSet - label).size == 0) {
      println("empty tree")
      return null
    }
  }
}
