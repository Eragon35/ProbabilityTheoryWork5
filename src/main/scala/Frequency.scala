import scalax.chart.module.Charting._

import scala.collection.mutable.ArrayBuffer

object Frequency {
  def show(func: ArrayBuffer[Double]): Unit = {
    // высчитываю данные для группированной выборки частот
    val k = (Math.log10(func.size)/Math.log10(2) + 1).toInt // k = 1+ log2 (20).round - число интервалов выборки
    val l = (func(func.size - 1) - func(0)) / k // длинна интервала
    var histogram: ArrayBuffer[(Double, Int)] = ArrayBuffer[(Double, Int)]() // (начало промежутка, число значений на нём)
    for (i <- 0 to k) histogram = histogram :+ (func(0) + l * (i - 0.5),
      func.count(_ < func(0) + l * (i+0.5) + 0.01) - histogram.map(z => z._2).sum)
    histogram = histogram :+ (func(func.size - 1) + l / 2, histogram(k-1)._2) // второе значение не играет роли конкретно тут
    println("Интервальный статистический ряд:")
    for (i <- 0 to k) println(f"[${histogram(i)._1}%1.3f, ${histogram(i+1)._1}%1.3f) P(x) = ${histogram(i)._2.toDouble / func.size}")
    var dataset = Seq[(Double, Double)]((histogram(0)._1, histogram(0)._2.toDouble / func.size))
    for (i <- 1 until k) dataset = dataset :+ (histogram(i)._1, histogram(i)._2.toDouble / func.size)
    dataset = dataset :+ (histogram(histogram.size - 1)._1, histogram(histogram.size - 1)._2.toDouble / func.size)
    // полигон группированной выборки частот графиком
    XYLineChart(Seq(("", dataset.toIndexedSeq)).toXYSeriesCollection()).show("Полигон частот", (1280, 720), scrollable = true)


    // показываю гистограмму группированной выборки частот
    var dataset2 = Seq[(String, Double)]()
    for (i <- 0 to k) dataset2 = dataset2 :+ (
      (s"${BigDecimal(histogram(i)._1).setScale(3, BigDecimal.RoundingMode.HALF_UP).toString}" + // имя столбца
      s" to ${BigDecimal(histogram(i+1)._1).setScale(3, BigDecimal.RoundingMode.HALF_UP).toString}") ->
        histogram(i)._2.toDouble / func.size // значение(высота) столбца
    )
    BarChart(dataset2).show("Гистограмма частот", (1280, 720), scrollable = true)
  }
}