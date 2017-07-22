package neophyte.part09

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration._

object Part09 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: Part09")

    val taxCut = Government.redeemCampaignPledge(25)
    println("Now that they are elected, let's see if they remember their promises ...")
    taxCut.onComplete {
      case Success(TaxCut(reduction)) => println(s"A myrical!!! They really cut the taxes by ${reduction} percent!!!")
      case Failure(e) => println(s"They broke the promise, because of a ${e.getMessage}")
    }
    Await.ready(taxCut, 60.seconds)
  }

  def makeAPromise: Promise[TaxCut] = Promise[TaxCut]()
  def deliverOnAPromise(tc: Promise[TaxCut], reduction: Int): Unit = {
    if(reduction <= 20) tc.success(TaxCut(reduction))
    else tc.failure(CrisisException("global crisis"))
  }
  def kickTheCanDownTheRoad(p: Promise[TaxCut]) = p.future
}

case class TaxCut(reduction: Int)
case class CrisisException(reason: String) extends Exception(reason)

object Government {
  import Part09._

  def redeemCampaignPledge(reduction: Int): Future[TaxCut] = {
    val p = makeAPromise
    val _ = Future {
      println("Starting the new legislative period ...")
      Thread.sleep(2.seconds.toMillis)
      deliverOnAPromise(p, reduction)
      println("... and we go to the polls again!!!")
    }
    kickTheCanDownTheRoad(p)
  }
}
