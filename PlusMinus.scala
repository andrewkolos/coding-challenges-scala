object PlusMinus {

  def main(args: Array[String]): Unit = {
    plusMinus(Array(-4, 3, -9, 0, 4, 1))
  }

  case class Counts(positive: Int, negative: Int, zero: Int)

  case class Ratios(positive: Double, negative: Double, zero: Double)

  def plusMinus(arr: Array[Int]): Unit = {
    def formatDouble(number: Double) = f"$number%.6f"

    val counts = getCounts(arr)
    val ratios = getRatios(counts)

    println(formatDouble(ratios.positive))
    println(formatDouble(ratios.negative))
    println(formatDouble(ratios.zero))
  }

  def getCounts(arr: Array[Int]): Counts = {
    var numPositives = 0
    var numNegatives = 0
    var numZeroes = 0

    for (num <- arr) {
      num match {
        case n if n > 0 => numPositives += 1
        case n if n < 0 => numNegatives += 1
        case n if n == 0 => numZeroes += 1
      }
    }

    Counts(numPositives, numNegatives, numZeroes)
  }

  def getRatios(counts: Counts): Ratios = {
    def computeRatio(count: Int, total: Int): Double = {
      count * 1.0 / total
    }

    val total = counts.positive + counts.negative + counts.zero

    Ratios(computeRatio(counts.positive, total),
      computeRatio(counts.negative, total),
      computeRatio(counts.zero, total))
  }


}
