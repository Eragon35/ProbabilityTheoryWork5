import scalax.chart.module.Charting._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

object Frequency {
  def show(func: ArrayBuffer[Double]): Unit = {
    val dataset = Seq(
      ("", for (x <- func) yield (x, func.groupBy(identity).mapValues(_.size)(x).toDouble / func.size)),
      ("y = 0", for (x <- func) yield (x, 0.0))
    )
    XYLineChart(dataset.toXYSeriesCollection()).show("Полигон частот", (1280, 720), scrollable = true)

    //todo: гистограмма частот

  }
}