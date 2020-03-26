import akka.actor._

object problem3 extends App{

  object ActorSort {
    case class Check(x: Int)
    case class Terminate()
  }
  object Receiver {
    case class Sorted(x: Int)
  }

  class Receiver extends Actor {
    import Receiver._
    var order = 0
    def receive = {
      case Sorted(x) =>
        order += 1
        println("Sorted in order" + order + " : " + x)
    }

  }

  class ActorSort (actorSort: ActorRef, receiver: ActorRef) extends Actor {
    import ActorSort._
    var smallestSeen = -1
    val newAct = context.actorOf(Props(classOf[ActorSort], sender, receiver))
    def receive: PartialFunction[Any, Unit] = {
      case Check(x) =>
        //check if x sentinel, larger, or smaller than curr
        if (x==0){
          //found sentinel
          if(smallestSeen < 0) {
            sender ! ActorSort.Terminate()
          } else {
            newAct ! ActorSort.Check(0)
          }
        }else if (smallestSeen < 0 ) {
          smallestSeen = x
        }else if (smallestSeen > 0 && x < smallestSeen) {
          //new minimum value, becomes smallest and have to send previous smallest down pipeline
          newAct ! ActorSort.Check(smallestSeen)
          smallestSeen = x
        } else {
          //not new min, send received val down pipeline
          newAct ! ActorSort.Check(x)
        }

      case Terminate() =>
        receiver ! Receiver.Sorted(smallestSeen)
        context.parent ! Terminate()
        context.stop(self)

    }

  }

  val system = ActorSystem("SortInts")

  val receiver1 = system.actorOf(Props[Receiver])
  val sorter1 = system.actorOf(Props(classOf[ActorSort],receiver1, receiver1))
  sorter1 ! ActorSort.Check(20)
  sorter1 ! ActorSort.Check(15)
  sorter1 ! ActorSort.Check(98)
  sorter1 ! ActorSort.Check(346)
  sorter1 ! ActorSort.Check(44)
  sorter1 ! ActorSort.Check(11)
  sorter1 ! ActorSort.Check(13)
  sorter1 ! ActorSort.Check(620)
  sorter1 ! ActorSort.Check(3)
  sorter1 ! ActorSort.Check(0)


}
