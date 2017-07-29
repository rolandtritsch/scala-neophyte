package neophyte.part15

import akka.actor.SupervisorStrategy.Resume
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, OneForOneStrategy, Props, SupervisorStrategy}

import scala.concurrent.Await
import scala.concurrent.duration._

object Part15 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part15.getClass.getName.split('$').head}")

    val coffeeHouse = ActorSystem("The-Art-of-Coffee")
    val barista = coffeeHouse.actorOf(Props[Barista], "Roland")
    val customer = coffeeHouse.actorOf(Props(classOf[Customer], barista), "Daniel")

    customer ! Customer.CaffeinWithdrawalWarning
    customer ! Customer.CaffeinWithdrawalWarning
    customer ! Customer.CaffeinWithdrawalWarning

    Thread.sleep(2.seconds.toMillis)
    barista ! Barista.ClosingTime

    val terminated = coffeeHouse.whenTerminated
    Await.result(terminated, 10.seconds)
  }
}

object Register {
  type Price = Int

  sealed trait Article
  case object Espresso extends Article
  case object Americano extends Article

  sealed trait Request
  case class Transaction(article: Article) extends Request
  case object Close

  class PaperJamException(msg: String) extends Exception(msg)
}

class Register extends Actor with ActorLogging {
  import Register._

  var paperJam = false

  var revenue = 0
  val prices = Map[Article, Int](
    Espresso -> 200,
    Americano -> 100
  )

  override def receive: Receive = {
    case Transaction(article) => {
      val price = prices(article)
      sender ! createReceipt(price)
      revenue += price
      log.info(s"Increment revenue to ${revenue} cents.")
    }
    case Close => {
      log.info(s"Made ${revenue} cents today.")
    }
  }

  def createReceipt(price: Price): Barista.Receipt = {
    import util.Random

    if(Random.nextBoolean()) {
      paperJam = true
      throw new PaperJamException("OMG, not again.")
    } else Barista.Receipt(price)
  }

  override def postRestart(reason: Throwable): Unit = {
    super.postRestart(reason)
    log.info(s"Restarted. Because of ${reason.getMessage}. And revenue is ${revenue} cents.")
  }
}

object Barista {
  sealed trait CoffeeRequest
  case object EspressoRequest extends CoffeeRequest
  case object AmericanoRequest extends CoffeeRequest

  sealed trait OtherRequest
  case object ClosingTime extends OtherRequest

  case class EspressoCup(state: EspressoCup.State)
  object EspressoCup {
    sealed trait State
    case object Clean extends State
    case object Filled extends State
    case object Dirty extends State
  }

  case class Receipt(amount: Register.Price)
}

class Barista extends Actor {
  import Register._
  import Barista._

  import akka.util.Timeout
  import akka.pattern.ask
  import akka.pattern.pipe
  import context.dispatcher

  implicit val timeout = Timeout(4.seconds)
  val register = context.actorOf(Props[Register], "John")

  override def receive: Receive = {
    case EspressoRequest => {
      val receipt = register ? Transaction(Espresso)
      receipt.map((EspressoCup(EspressoCup.Filled), _)).pipeTo(sender)
    }
    case ClosingTime => {
      register ! Close
      //context.stop(self)
      context.system.terminate()
    }
  }

  import akka.actor.SupervisorStrategy.Directive

  val decider: PartialFunction[Throwable, Directive] = {
    case _: PaperJamException => Resume
  }

  override def supervisorStrategy: SupervisorStrategy = {
    val MaxRestarts = 10
    val WithinTimeframe = 2.minutes
    OneForOneStrategy(MaxRestarts, WithinTimeframe)(decider.orElse(SupervisorStrategy.defaultStrategy.decider))
  }
}

object Customer {
  case object CaffeinWithdrawalWarning
}

class Customer(coffeeSource: ActorRef) extends Actor with ActorLogging {
  import Customer._
  import Barista._
  import Barista.EspressoCup._

  override def receive: Receive = {
    case CaffeinWithdrawalWarning => {
      coffeeSource ! EspressoRequest
    }
    case (EspressoCup(Filled), Receipt(amount)) => {
      log.info(s"Yay, caffeine for ${self}.")
    }
  }
}