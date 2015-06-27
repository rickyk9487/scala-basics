import math.{abs, exp}

val tol = 1e-4
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tol

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}
fixedPoint(x => math.exp(-x) - x)(1.0) // 2.513716946360203
