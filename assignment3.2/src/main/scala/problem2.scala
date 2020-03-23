object problem2 {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).map((p: (A,S)) => p._1 #:: unfold(p._2)(f)).getOrElse(LazyList.empty[A])

  def factors(n:Int):List[Int]={
    unfold(1)(p => Some(p,p+1)).filter(n%_==0).takeWhile(_<n).toList
  }

  def sumFactors(l: List[Int]):Int = {
    l.sum
  }

  def perfInteger(): LazyList[Int] = {
    val liOfInts = unfold(1) {
      case x  => Some(x,x+1)
      case _ => None
    }.filter(x => factors(x).sum == x)
    liOfInts
  }

  def main(args:Array[String]): Unit = {
    println("testing factors of 6: " + factors(6))
    println("testing factors of 5: " + factors(5))
    println("testing factors of 0: " + factors(0))
    println("testing factors of 156: " + factors(156))
    println("testing factors of 328: " + factors(328))
    println("testing perf integers with 0 elements: " + perfInteger().take(0).toList)
    println("testing perf integers with 1 elements: " + perfInteger().take(1).toList)
    println("testing perf integers with 2 elements: " + perfInteger().take(2).toList)
    println("testing perf integers with 3 elements: " + perfInteger().take(3).toList)
    println("testing perf integers with 4 elements: " + perfInteger().take(4).toList)


  }

}
