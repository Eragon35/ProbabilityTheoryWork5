import scalax.chart.module.Charting._

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer

object EmpiricalDistributionFunction {
  def show(func: ArrayBuffer[Double]): Unit = {
    // F*20(х) = { 0 при x <= -1.73 ... 1 при x >= 1.7 }
    val tabs = s"\t\t\t"
    println(s"Эмпирическая функция распределения:\n$tabs ___\n$tabs| 0, x <= ${func(0)}")
    for(i <- 1 until 20) {
      if (i == 11) println(s" F* 20(х) = | ${i.toDouble / 20}, ${func(i-1)} < x <= ${func(i)}")
      println(s"$tabs| ${i.toDouble / 20}, ${func(i-1)} < x <= ${func(i)}")
    }
    println(s"$tabs| 1, x > ${func(func.size-1)}\n$tabs ‾‾‾‾")


    var dataset = Seq((0.toString, for (x <- -2.0 to func(0) by 0.01) yield (x, 0.0)))
    for(i <- 1 until 20) dataset = dataset :+ (func(i).toString,
      IndexedSeq((func(i - 1), i.toDouble / 20), (func(i), i.toDouble / 20)))
    dataset = dataset :+ ((1.toString, IndexedSeq((func(func.size-1), 1.0), (2.0, 1.0))))
    XYLineChart(dataset.toXYSeriesCollection()).show("Эмпирическую функция распределения", (1280, 720), scrollable = true)
  }
}
