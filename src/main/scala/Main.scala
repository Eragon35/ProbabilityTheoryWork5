import scala.collection.mutable.ArrayBuffer

object Main extends App {
  val array: ArrayBuffer[Double] = ArrayBuffer[Double](0.83, -0.48, -1.35, 0.31, 0.59,
    1.35, -0.3, -0.24, 0.51, 0.26, 0.73, 0.0, 1.59, 0.17, -0.45, 1.6, -0.18, -1.73, 0.03, 1.7)
  val sortedArray: ArrayBuffer[Double] = array.sortWith(_ < _) // вариационный ряд
  val firstOrdinalStatistics: Double = sortedArray(0) // первая порядковая статистика
  val lastOrdinalStatistics: Double = sortedArray(array.size - 1) // последняя порядковая статистика
  val range: Double = lastOrdinalStatistics - firstOrdinalStatistics // размах
  // у меня статистический и вариационный ряды совпадают так как все значения повторяются лишь однажды
  var expectedValue: Double = array.sum / array.size // математическое ожидание
  var standardDeviation: Double = array.map(x => Math.pow(expectedValue - x, 2)).sum // среднеквадратическое отклонения

  // выводим получившиеся значения
  print("Исходный ряд:")
  array.foreach(x => print(s" $x"))
  print("\nВариационный ряд:")
  sortedArray.foreach(x => print(s" $x"))
  println(s"\nПервая порядковая статистика = $firstOrdinalStatistics\nПоследняя порядковая статистика = $lastOrdinalStatistics" +
    f"\nРазмах = $range%1.2f\nМатематическое ожидание = $expectedValue%1.3f\nСреднеквадратическое отклонение = $standardDeviation".
      replace(",", "."))
  EmpiricalDistributionFunction.show(sortedArray) // выводи и строим график эмпирической функции
  Frequency.show(sortedArray) // строим гистограмму и полигон приведенных частот группированной выборки
}