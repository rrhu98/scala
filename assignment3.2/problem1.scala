object problem1{

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z).map((p: (A,S)) => p._1 #:: unfold(p._2)(f)).getOrElse(LazyList.empty[A])

  def sqr(x:Int) = x*x


  def pythagorean(limit: Int): Option[List[(Int,Int,Int)]] = {
    val liOfInts = unfold(1) {
      case x if x <= limit => Some(x,x+1)
      case _ => None
    }.toList

    val triples = for {
      x <- liOfInts
      y <- liOfInts
      z <- liOfInts
    }yield (x,y,z)

    val pythagTriples =
      triples.filter(x => sqr(x._1)+sqr(x._2) == sqr(x._3))

    Some(pythagTriples)
  }


  def main(args:Array[String]): Unit = {
    println("pyth triples with limit 100: " + pythagorean(100))
    println("pyth triples with limit 10: " + pythagorean(10))
    println("pyth triples with limit 20: " +pythagorean(20))
    println("pyth triples with limit 0: " +pythagorean(0))

  }
}
