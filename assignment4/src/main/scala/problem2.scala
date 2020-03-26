import akka.actor._

object problem2 extends App{

  object FibActor {
    case class fibNbr(num: Int, fc: Option[FibMessage])
    case class Receive(num:Int, fc: FibMessage)
  }

  object ClientActor {
    case class result(n: Int, res: Int)
    case class findFib(x: Int)
  }

  class FibMessage(val repTo: FibActor, val fibs: Option[FibMessage], val num: Int){
    var self = -1
    var other = -1
  }


  //client actor class which calls fibactor and provides fib number to find
  class ClientActor extends Actor {
    import FibActor._
    import ClientActor._
    def receive = {
      case result(n, res) =>
        if (res >= 0) {
          println("the " + n + " fibonnaci number is: " + res)
        } else {
          println("Invalid input, please enter integer 0 or above")
        }
      case findFib(num) =>
        println(s"${self.path} received fib number $num")
        val fibActor = context.actorOf(Props[FibActor])
        fibActor ! fibNbr(num, None)

    }
  }

  //fib actor which recursively finds fib number
  class FibActor extends Actor {
    import FibActor._
    def receive: PartialFunction[Any, Unit] = {
      case fibNbr(num, fm) =>
        if (num <= 1 && num >= 0) {
          fm match {
            case Some(x) =>
              sender ! FibActor.Receive(num, x)
            case None =>
              context.parent ! ClientActor.result(num,num)
          }
        }
        else if (num >1) {
          val newFib = context.actorOf(Props[FibActor])
          val newFM = new FibMessage(this, fm, num)
          self ! FibActor.fibNbr(num-1, Some(newFM))
          newFib ! FibActor.fibNbr(num-2, Some(newFM))
        }
        else {
          context.parent ! ClientActor.result(num,-1)
        }

      case Receive(num, fm) =>
        if (sender == fm.repTo.self) {
          fm.self = num
        }
        else {
          fm.other = num
        }
        if (fm.self != -1 && fm.other != -1) {
          if (self != fm.repTo.self) {
            fm.repTo.self ! FibActor.Receive(fm.other, fm)
          }
          else {
            fm.fibs match {
              case Some(value) =>
                fm.repTo.self ! FibActor.Receive (fm.self + fm.other, value)
              case None =>
                fm.repTo.context.parent ! ClientActor.result(fm.num, fm.self + fm.other)
            }
          }
        }
    }

  }
  val system = ActorSystem("fibCalc")
  val clientActor = system.actorOf(Props[ClientActor], "clientActor")

  clientActor ! ClientActor.findFib(-1)
  clientActor ! ClientActor.findFib(0)
  clientActor ! ClientActor.findFib(1)
  clientActor ! ClientActor.findFib(2)
  clientActor ! ClientActor.findFib(3)
  clientActor ! ClientActor.findFib(5)
  clientActor ! ClientActor.findFib(7)
  clientActor ! ClientActor.findFib(15)


}
