import scalax.chart.module.Charting._

import scala.collection.mutable.ArrayBuffer

object Frequency {
  def show(func: ArrayBuffer[Double]): Unit = {
    // полигон частот графиком
    val dataset = Seq(
      ("", for (x <- func) yield (x, func.groupBy(identity).mapValues(_.size)(x).toDouble / func.size)),
      ("y = 0", for (x <- func) yield (x, 0.0))
    )
    XYLineChart(dataset.toXYSeriesCollection()).show("Полигон частот", (1280, 720), scrollable = true)
    // полигон частот гистограммой
    val dataset2: Seq[(String, Double)] = for (x <- func) yield (x.toString, func.groupBy(identity).mapValues(_.size)(x).toDouble / func.size)
    BarChart(dataset2).show("Полигон частот", (1280, 720), scrollable = true)


    // высчитываю данные для гистограммы частот
    val k = (Math.log10(func.size)/Math.log10(2) + 1).toInt // k = 1+ log2 (20).roundDown - число интервалов выборки
    val l = (func(func.size - 1) - func(0)) / k // длинна интервала
    var histogram: ArrayBuffer[(Double, Int)] = ArrayBuffer[(Double, Int)]() // (начало промежутка, число значений на нём)
    for (i <- 0 until k) histogram = histogram :+ (func(0) + l * i,
      func.count(_ < func(0) + l * (i+1) + 0.01) - histogram.map(z => z._2).sum)
    histogram = histogram :+ (func(func.size - 1), histogram(k-1)._2)
    // -1.73 to -1.044 = 2 => 0.1
    // -1.044 to -0.358 = 2 => 0.1
    // -0.358 to 0.329 = 8 => 0.4
    // 0.329 to 1.014 = 4 => 0.2
    // 1.014 to 1.7 = 4 => 0.2

    // показываю гистограмму частот графиком
    var seq: Seq[(Double, Double)] = Seq((histogram(0)._1-0.0001, 0.0))
    for (i <- 0 until k) seq = seq ++ Seq((histogram(i)._1, histogram(i)._2.toDouble / func.size),
      (histogram(i+1)._1-0.0001, histogram(i)._2.toDouble / func.size))
    seq = seq :+ (histogram(k)._1 + 0.0001, 0.0)
    var dataset3 = Seq(("y = 0", for (x <- -2.0 to func(0) by 0.01) yield (x, 0.0)))
    dataset3 = dataset3 :+ ("", seq.toIndexedSeq)
    XYLineChart(dataset3.toXYSeriesCollection()).show("Гистограмма частот", (1280, 720), scrollable = true)

    // показываю гистограмму частот гистограммой
    var dataset4 = Seq[(String, Double)]()
    for (i <- 0 until k) dataset4 = dataset4 :+ (
      (s"${BigDecimal(histogram(i)._1).setScale(3, BigDecimal.RoundingMode.HALF_UP).toString}" + // имя столбца
      s" to ${BigDecimal(histogram(i+1)._1).setScale(3, BigDecimal.RoundingMode.HALF_UP).toString}") ->
        histogram(i)._2.toDouble / func.size // значение(высота) столбца
    )
    BarChart(dataset4).show("Гистограмма частот", (1280, 720), scrollable = true)
  }
}