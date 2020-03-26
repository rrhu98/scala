import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await

object problem1 extends App{

  case class fibNbr(num: Int)

  //client actor class which calls fibactor and provides fib number to find
  class ClientActor(fibActor: ActorRef) extends Actor {
    def receive = {
      case fibNbr(num) =>
        println(s"${self.path} received fib number $num")
        if (num < 0) {
          println("Invalid input, please enter an integer 0 or above")
        }
        else {
          fibActor ! fibNbr(num)
        }
      case int =>
        println("the fibonacci number is: " + int)
    }
  }

  //fib actor which recursively finds fib number
  class FibActor extends Actor {
    def receive = {
      case fibNbr(num) =>
        if (num == 0) {
          sender ! 0
        }
        else if (num == 1 || num == 2) {
          sender ! 1
        }
        else {
          val fibActor1 = system.actorOf(Props[FibActor])
          val fibActor2 = system.actorOf(Props[FibActor])
          implicit val timeout = Timeout(10.seconds)
          val recurse1 = fibActor1 ? fibNbr(num - 1)
          val result1 = Await.result(recurse1, timeout.duration).asInstanceOf[Int]
          val recurse2 = fibActor2 ? fibNbr(num - 2)
          val result2 = Await.result(recurse2, timeout.duration).asInstanceOf[Int]
          sender ! (result1 + result2)

        }

    }

  }

  val system = ActorSystem("fibCalc")

  val fibTest = fibNbr(-1)
  val fibTest0 = fibNbr(0)
  val fibTest1 = fibNbr(1)
  val fibTest2 = fibNbr(2)
  val fibTest3 = fibNbr(7)
  val fibTest4 = fibNbr(10)
  val fibTest5 = fibNbr(25)
  val fibActor = system.actorOf(Props[FibActor], "fibActor")
  val clientActor = system.actorOf(Props(new ClientActor(fibActor)), "clientActor")

  clientActor ! fibTest
  clientActor ! fibTest0
  clientActor ! fibTest1
  clientActor ! fibTest2
  clientActor ! fibTest3
  clientActor ! fibTest4
  clientActor ! fibTest5


}
