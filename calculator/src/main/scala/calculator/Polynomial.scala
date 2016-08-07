package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def getSolution(aa: Double, bb: Double, d: Double): Set[Double] =
      if (d == 0) Set(-bb * 0.5 / aa)
      else if (d < 0) Set.empty
      else Set((-bb + math.sqrt(d))*0.5/aa, (-bb - math.sqrt(d))*0.5/aa)
    Signal(getSolution(a(), b(), delta()))
  }
}
