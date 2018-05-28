package homework2

/**
  * Неиспользуя мутабельные коллекции и var, реализовать сортировку слиянием.
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSort extends App {

  def merge(part1: Seq[Int], part2: Seq[Int]): Seq[Int] = {
    (part1, part2) match {
      case (Seq(x), Seq(y)) => if (x > y) Seq(y, x) else Seq(x, y)
      case (Seq(), y) => y
      case (x, Seq()) => x
      case (x, y) => if (x.head < y.head) x.head +: merge(x.tail, y)
      else y.head +: merge(x, y.tail)
    }
  }

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val half = data.size / 2
    if (half == 0) data
    else {
      val divData: (Seq[Int], Seq[Int]) = data.splitAt(half)
      merge(mergeSort(divData._1), mergeSort(divData._2))
    }
  }
}