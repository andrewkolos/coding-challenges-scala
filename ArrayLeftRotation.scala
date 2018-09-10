// https://www.hackerrank.com/challenges/ctci-array-left-rotation/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=arrays

object ArrayLeftRotation {

  def main(args: Array[String]): Unit = {

  }

  // rotate a left d times
  def rotLeft(a: Array[Int], d: Int): Array[Int] = {
    val numElements = a.length
    if (numElements == 0 || d == 0) a
    else {
      val rotationAmount = d match {
        case n if n > numElements => d % numElements + numElements
        case _ => d
      }

      val joj = gcd(numElements, d)
      for (cycle <- 0 until gcd(numElements, d)) {
        var idxToRotateTo = cycle
        val originalIdxToRotateTo = idxToRotateTo
        var idxToRotateFrom = idxToRotateTo + rotationAmount
        var temp = a(idxToRotateTo)

        var prevIdxToRotateFrom = idxToRotateFrom
        while (idxToRotateFrom != originalIdxToRotateTo) {
          prevIdxToRotateFrom = idxToRotateFrom
          a(idxToRotateTo) = a(idxToRotateFrom)
          idxToRotateTo = idxToRotateFrom
          idxToRotateFrom = (idxToRotateFrom + d) % numElements
        }

        a(prevIdxToRotateFrom) = temp
      }
      a
    }
  }

    def gcd(a: Int, b: Int): Int = b match {
      case 0 => a
      case _ => gcd(b, a % b)
    }
  }
