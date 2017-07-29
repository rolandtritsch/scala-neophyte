package neophyte.part15

import akka.actor._

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

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
}

class Register extends Actor with ActorLogging {
  import Barista._
  import Register._
  import ReceiptPrinter._

  import akka.pattern.ask
  import akka.pattern.pipe
  import context.dispatcher
  import akka.util.Timeout

  implicit val timeout = Timeout(5.seconds)

  var revenue = 0
  val prices = Map[Article, Int](
    Espresso -> 200,
    Americano -> 100
  )

  val printer = context.actorOf(Props[ReceiptPrinter], "Printer")

  override def receive: Receive = {
    case Transaction(article) => {
      val price = prices(article)
      val requester = sender
      (printer ? PrintJob(price)).map((requester, _)).pipeTo(self)
    }
    case (requester: ActorRef, receipt: Receipt) => {
      revenue += receipt.amount
      log.info(s"Increment revenue to ${revenue} cents.")
      requester ! receipt
    }
    case Close => {
      log.info(s"Made ${revenue} cents today.")
    }
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

  import akka.pattern.ask
  import akka.pattern.pipe
  import context.dispatcher

  import akka.pattern.AskTimeoutException
  import akka.util.Timeout

  implicit val timeout = Timeout(4.seconds)
  val register = context.actorOf(Props[Register], "John")

  override def receive: Receive = {
    case EspressoRequest => {
      val receipt = register ? Transaction(Espresso)
      receipt.map((EspressoCup(EspressoCup.Filled), _)).recover {
        case _: AskTimeoutException => Customer.ComeBackLater
      }.pipeTo(sender)
    }
    case ClosingTime => {
      register ! Close
      //context.system.terminate()
      context.stop(self)
    }
  }
}

object Customer {
  case object CaffeinWithdrawalWarning
  case object ComeBackLater
}

class Customer(coffeeSource: ActorRef) extends Actor with ActorLogging {
  import Customer._
  import Barista._
  import Barista.EspressoCup._

  context.watch(coffeeSource)

  override def receive: Receive = {
    case CaffeinWithdrawalWarning => {
      coffeeSource ! EspressoRequest
    }
    case (EspressoCup(Filled), Receipt(amount)) => {
      log.info(s"Yay, caffeine for ${self}.")
    }
    case ComeBackLater => {
      log.info(s"Grumble, grumble, ...")
      context.system.scheduler.scheduleOnce(300.millis) {
        coffeeSource ! EspressoRequest
      }
    }
    case Terminated => {
      log.info(s"Ok. Let's go to a 24/7 Starbucks!")
    }
  }
}

object ReceiptPrinter {
  case class PrintJob(amount: Register.Price)

  class PaperJamException(msg: String) extends Exception(msg)
}

class ReceiptPrinter extends Actor with ActorLogging {
  import ReceiptPrinter._

  var paperJam = false

  override def postRestart(reason: Throwable): Unit = {
    super.postRestart(reason)
    log.info(s"Restarted. paperJam == ${paperJam}")
  }

  override def receive: Receive = {
    case PrintJob(amount) => sender ! createReceipt(amount)
  }

  def createReceipt(price: Register.Price): Barista.Receipt = {
    import util.Random

    if(Random.nextBoolean()) {
      paperJam = true
      throw new PaperJamException("OMG, not again.")
    } else Barista.Receipt(price)
  }
}