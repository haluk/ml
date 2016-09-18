package data

import java.io.{File, FileReader}
import java.util.Properties

/**
  * Created by hd on 9/15/16.
  */
object CSVTrial {


  def main(args: Array[String]): Unit = {
    //    val df = Csv.parseFile(new File("weather.csv")).labeled.toFrame
    //    val weather = df.filter(Cols("outlook").as[String])
    //    val outlook = df.get(Cols[String]("outlook"))
    //    val outlook = df.col
    //    println(outlook)
    //    val playNo = df.filter(Cols("play").as[String])(_ == "no")
    //    val playNoWindy = df.filter(Cols("play").as[String])(_ == "no")
    //      .filter(Cols("windy").as[String])(_ == "FALSE")
    //    println(playNoWindy)

    val x = new Properties
    x.load(new FileReader(new File("house-votes-84.properties")))
    println(x.getProperty("dataset.file.name"))
    println(x.getProperty("attribute.names"))
    println(x.getProperty("class.index"))
    println(x.getProperty("missing.value.sign"))
  }


}
