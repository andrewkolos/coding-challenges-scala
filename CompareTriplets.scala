// https://www.hackerrank.com/challenges/compare-the-triplets/problem

object CompareTriplets {

  def main(args: Array[String]): Unit = {
    val test1 = compareTriplets(Array(17, 28, 30), Array(99, 16, 8))
    println(test1.mkString(" "))
    assert(test1 sameElements Array(2, 1))
  }

  case class Scores(alice: Int, bob: Int)

  def compareTriplets(aliceValues: Array[Int], bobValues: Array[Int]): Array[Int] = {
    val scores: Scores = {
      val initialScores = Scores(0, 0)
      (aliceValues zip bobValues).foldLeft(initialScores) { (s, pair) =>
        pair match {
          case (aliceValue, bobValue) if aliceValue > bobValue => Scores(s.alice + 1, s.bob)
          case (aliceValue, bobValue) if aliceValue < bobValue => Scores(s.alice, s.bob + 1)
          case _ => s
        }
      }
    }
    Array(scores.alice, scores.bob)
  }

  // more procedural
  def compareTriplets2(aliceRatings: Array[Int], bobRatings: Array[Int]): Array[Int] = {
    var aliceScore = 0
    var bobScore = 0

    for (i <- aliceRatings.indices) {
      aliceRatings(i) match {
        case a if a > bobRatings(i) => aliceScore = aliceScore + 1
        case a if a < bobRatings(i) => bobScore = bobScore + 1
        case _ => ()
      }
    }

    Array(aliceScore, bobScore)
  }

  // most procedural
  def compareTriplets3(aliceRatings: Array[Int], bobRatings: Array[Int]): Array[Int] = {
    var aliceScore = 0
    var bobScore = 0

    for (i <- aliceRatings.indices) {
      val currAliceRating = aliceRatings(i)
      val currBobRating = bobRatings(i)

      if (currAliceRating > currBobRating) aliceScore = aliceScore + 1
      else if (currAliceRating < currBobRating) bobScore = bobScore + 1
    }

    Array(aliceScore, bobScore)
  }
}