import scala.annotation.tailrec

object problem1 extends App {
  val list1 = List(1,3,5,7,8,9,10)
  val list2 = List(2,4,6)
  val deckSim = List("hAce", "h2","h3", "c4", "c5", "c6")
  val deckSim2 = List("hAce", "h3", "c5", "h2", "c4", "c6")
  val deckSim3 = List("h2", "c4", "c6", "hAce", "h3", "c5")

  def shuffle[A] (l:List[A], r:List[A] ): List[A] = (l,r) match {
    case (Nil,Nil) => Nil
    case (x::xs, Nil) => l
    case (Nil, y::ys) => r
    case (x :: xs,y::ys) => x :: shuffle(r, l.tail)
  }

  def split[A] (l:List[A], n:Int): (List[A],List[A])= {
    @tailrec
    def splitHelper(currIndex:Int, l:List[A], hList:List[A]):(List[A],List[A]) = (l, currIndex) match {
      case (Nil, `n`) => (Nil,Nil)
      case n if n == l.length => (l,List())
      case (x::xs, `n`) => (hList:::List(x), xs)
      case (x::y, `currIndex`) => splitHelper(currIndex+1, y, hList:+x)
      case _ => (hList,hList)
    }
    splitHelper(1, l, List[A]())
  }

  def outShuffle[A] (l:List[A]): (List[A]) = {
    val splitList = split(l, l.length/2)
    shuffle(splitList._1,splitList._2)
  }

  def inShuffle[A] (l:List[A]): (List[A]) = {
    val splitList = split(l, l.length/2)
    shuffle(splitList._2,splitList._1)
  }

  @tailrec
  def nShuffle[A] (n: Int, l:List[A]) (f: List[A] => List[A]): List[A] = (f,n,l) match {
    case (_, 0, _) => l
    case (_, _, Nil) => Nil
    case (outShuffle, n, l) =>
      val outShuffled = outShuffle(l)
      nShuffle(n-1, outShuffled) (outShuffle)
    case(inShuffle, n, l) =>
      val inShuffled = inShuffle(l)
      nShuffle(n-1,inShuffled) (inShuffle)
  }

  def howManyShuffles[A] (l1:List[A],l2:List[A]) (f: List[A]=>List[A]): Int = {
    @tailrec
    def shuffleAccum(acc: Int, l1:List[A]): Int = (f,l1,l2) match {
      case (_,Nil, Nil) => acc
      case (_,Nil,_) => acc
      case (_,_,Nil) => acc
      case (outShuffle, l1, l2) =>
        if (l1 == l2) {
          acc
        } else{
          val outShuffled = outShuffle(l1)
          shuffleAccum(acc+1, outShuffled)
        }
      case (inShuffle, l1, l2) =>
        if (l1 == l2) {
          acc
        } else{
          val inShuffled = inShuffle(l1)
          shuffleAccum(acc+1, inShuffled)
        }

    }
    shuffleAccum(0, l1)


  }

  println("Shuffle results: ", shuffle(list1, list2))
  println("Split results: ", split(list1, 6))
  println("outShuffle results: ", outShuffle(deckSim))
  println("inShuffle results: ", inShuffle(deckSim))
  println("nShuffle outShuffle results: ", nShuffle(7, deckSim)(outShuffle))
  println("nShuffle inShuffle results: ", nShuffle(7,deckSim)(inShuffle))
  println("howManyShuffles outShuffle results: ", howManyShuffles(deckSim, deckSim2)(outShuffle))
  println("howManyShuffles inShuffle results: ", howManyShuffles(deckSim, deckSim3)(inShuffle))
  val deckOfCards = List.range(1,53)
  val outCardsOnce = outShuffle(deckOfCards)
  println("howManyShuffles (outShuffles) to return original deck: ", howManyShuffles(outCardsOnce, deckOfCards)(outShuffle)+1)
  val reversed = deckOfCards.reverse
  println("howManyShuffles (inShuffles) to return reversed deck: ", howManyShuffles(deckOfCards,reversed)(inShuffle))

}