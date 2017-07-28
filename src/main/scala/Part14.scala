package neophyte.part14

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Part14 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part14.getClass.getName.split('$').head}")

    val system = ActorSystem("The-Art-of-Coffee")

    val barista: ActorRef = system.actorOf(Props[Barista], "Barista")
    // the barista is making a cup of coffee for himself
    // if we do not set the sender, the billing will end up in the dead-letter-queue
    //barista ! CappuccinoRequest
    //barista ! EspressoRequest
    barista.tell(CappuccinoRequest, barista)
    barista.tell(EspressoRequest, barista)
    println("Barista is making a cappuccino and an espresso (for him/herself)")

    // OMG ... customer is running low on caffein
    val customer: ActorRef = system.actorOf(Props(classOf[Customer], barista), "Roland")
    customer ! CaffeinWithdrawalWarning

    askForCoffee(system, barista, CappuccinoRequest)

    // barista shutsdown the coffee maschine
    barista ! ClosingTime

    val terminated = system.whenTerminated
    Await.result(terminated, 10.seconds)
  }

  def askForCoffee(system: ActorSystem, barista: ActorRef, coffee: CoffeeRequest): Unit = {
    import akka.pattern.ask
    import akka.util.Timeout

    implicit val timeout = Timeout(2.seconds)
    implicit val ec = system.dispatcher

    val f: Future[Any] = barista ? coffee
    f.onComplete {
      case Success(msg) => msg match {
        case Bill(cents) => {
          println(s"Need to pay ${cents} cents")
        }
      }
      case Failure(e) => println(s"Ooopppsss ... ${e.getMessage}")
    }
  }
}

sealed trait CoffeeRequest
case object CappuccinoRequest extends CoffeeRequest
case object EspressoRequest extends CoffeeRequest

case class Bill(cents: Int)
case object ClosingTime

class Barista extends Actor {
  var coffeeSold = 0
  override def receive: Receive = {
    case CappuccinoRequest => {
      coffeeSold += 1
      sender ! Bill(250)
      println("Make Cappucino")
    }
    case EspressoRequest => {
      coffeeSold += 1
      sender ! Bill(200)
      println("Make Espresso")
    }
    case Bill(_) => {
      println("Putting 1 EUR in the coffee jar :)")
    }
    case ClosingTime => {
      println(s"Sold ${coffeeSold} coffees")
      context.system.terminate()
    }
  }
}

case object CaffeinWithdrawalWarning
class Customer(caffeinSource: ActorRef) extends Actor {
  override def receive: Receive = {
    case CaffeinWithdrawalWarning => caffeinSource ! EspressoRequest
    case Bill(cents) => println(s"${self.path.name} has to pay ${cents} cents, or else!")
  }
}