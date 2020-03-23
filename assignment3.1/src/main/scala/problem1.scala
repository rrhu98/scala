object problem1{

  val royalParent = Map("George" -> ("m", "William", "Catherine"), "Charlotte" -> ("f", "William", "Catherine"),
    "Louis" -> ("m", "William", "Catherine"), "Archie" -> ("m", "Harry", "Meghan"), "Savannah" -> ("f", "Autumn", "Peter"),
    "Isla" -> ("f", "Autumn", "Peter"), "Mia" -> ("f", "Zara", "Mike"), "Lena" -> ("f", "Zara", "Mike"),
    "Beatrice" -> ("f", "Andrew", "Sarah"), "Eugenie" -> ("f", "Andrew", "Sarah"), "Louise" -> ("f", "Edward", "Sophie"),
    "James" -> ("m", "Edward", "Sophie"), "Peter" -> ("m", "Mark", "Anne"), "Zara" -> ("f", "Mark", "Anne"),
    "William" -> ("m", "Diana", "Charles"), "Harry" -> ("m", "Diana", "Charles"), "Charles" -> ("m", "Elizabeth", "Philip"),
    "Anne" -> ("f", "Elizabeth", "Philip"), "Andrew" -> ("m", "Elizabeth", "Philip"), "Edward" -> ("m", "Elizabeth", "Philip"),
    "Elizabeth" -> ("f", "", ""), "Philip" -> ("m", "", ""), "Diana" -> ("f", "", ""), "Mark" -> ("m", "", ""),
    "Sophie" -> ("f", "", ""), "Sarah" -> ("f", "", ""), "Mike" -> ("m", "", ""), "Autumn" -> ("f", "", ""), "Meghan" -> ("f", "", ""),
    "Catherine" -> ("f", "", ""), "Timothy" -> ("m", "", ""), "Jack" -> ("m", "", ""), "Camilla" -> ("f", "", ""))


  def parents(p: String): Option[(String,String)] = {
    if (royalParent.contains(royalParent(p)._2) && royalParent.contains(royalParent(p)._3)) {
        Some(royalParent(p)._2, royalParent(p)._3)
    } else {
      None
    }
  }

  def grandparents(p: String): Option[List[String]] = {
    parents(p) match {
      case None => None
      case Some(par) =>
        val momOfP = par._1
        val dadOfP = par._2
        (parents(momOfP),parents(dadOfP)) match {
          case (None,None) => None
          case (None,Some(dad)) =>
            val res : Option[List[String]] = Some(dad._1::dad._2::Nil)
            res
          case (Some(mom), None) =>
            val res : Option[List[String]]= Some(mom._1::mom._2::Nil)
            res
          case (Some(mom), Some(dad)) =>
            val res :Option[List[String]] = Some(mom._1:: mom._2:: dad._1:: dad._2::  Nil)
            res

        }
    }

  }

  def siblings(p: String): Option[List[String]] = {
    parents(p) match {
      case None => None
      case Some(par) =>
        val sibs = for {
          x <-royalParent.keys.toList if (parents(p) == parents(x) && p != x)
        }yield x
        Some(sibs)
    }
  }

  def firstCousins(p: String): Option[List[String]] = {
    grandparents(p) match {
      case None => None
      case Some(gpar) =>
        val cous = for {
          x <-royalParent.keys.toList if (grandparents(p) == grandparents(x) && p != x && parents(p)!=parents(x))
        }yield x
        Some(cous)
    }
  }

  def uncles(p: String): Option[List[String]] = {
    parents(p) match {
      case None => None
      case Some(par) =>
        val momOfP = par._1
        val dadOfP = par._2
        (siblings(momOfP),siblings(dadOfP)) match {
          case (None, None) => None
          case (Some(momSibs), None) =>
            Some(momSibs.filter(x => royalParent(x)._1 == "m"))
          case (None, Some(dadSibs)) =>
            Some(dadSibs.filter(x=>royalParent(x)._1 == "m"))
          case (Some(momSibs), Some(dadSibs)) =>
            Some(momSibs.filter(x => royalParent(x)._1 == "m") ::: dadSibs.filter(x=>royalParent(x)._1 == "m"))
    }
    }
  }

  def main(args:Array[String]): Unit = {
    println("parents of William are: " + parents("William"))
    println("parents of Diana are: " + parents("Diana"))
    println("grandparents of William are: " + grandparents("William"))
    println("grandparents of Isla are: " + grandparents("Isla"))
    println("grandparents of Elizabeth are: " + grandparents("Elizabeth"))
    println("siblings of Harry are: " + siblings("Harry"))
    println("siblings of Charles are: " + siblings("Charles"))
    println("siblings of Archie are: " + siblings("Archie"))
    println("first cousins of Charles are: " + firstCousins("Charles"))
    println("first cousins of Peter are: " + firstCousins("Peter"))
    println("first cousins of Archie are: "+ firstCousins("Archie"))
    println("uncles of Archie are: " + uncles("Archie"))
    println("uncles of William are: " + uncles("William"))
    println("uncles of Andrew are: " + uncles("Andrew"))
  }

}
