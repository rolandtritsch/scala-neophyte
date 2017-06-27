package neophyte

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.util.Random

object Part08 {
  private val MAX_WAIT = 60.seconds
  private val RANDOM_WAIT = 2.seconds
  private val WATER_INITIAL_TEMP = 25
  private val WATER_COFFEE_TEMP = 85

  def main(args: Array[String]): Unit = {
    println(s"Brewing: ${prepareCappucino().getOrElse("BOOOM - broke the machine")}")

    val groundedBeans = grindBeansF("baked beans")
    groundedBeans.onComplete {
      case Success(s) => println(s"grounded ${s}")
      case Failure(e) => println(e)
    }
    Await.ready(groundedBeans, MAX_WAIT)

    val temperatureOkay = heatWaterF(Water(20)).map { water => {
        println("we are in the future!")
        (80 to 85).contains(water.temperature)
    }}
    temperatureOkay.onComplete {
      case Success(s) => println(s"water is boiled")
      case Failure(e) => println(e)
    }
    Await.ready(temperatureOkay, MAX_WAIT)
    temperatureOkay.foreach(assert(_))

    val aCupOfCoffee = prepareCappucinoFS()
    aCupOfCoffee.onComplete {
      case Success(s) => println(s"Brewing FS: ${s}")
      case Failure(e) => println(e)
    }
    Await.ready(aCupOfCoffee, MAX_WAIT)

    val anotherCupOfCoffee = prepareCappucinoFP()
    anotherCupOfCoffee.onComplete {
      case Success(s) => println(s"Brewing FP: ${s}")
      case Failure(e) => println(e)
    }
    Await.ready(anotherCupOfCoffee, MAX_WAIT)
  }

  type CoffeeBeans = String
  type GroundCoffee = String
  type Milk = String
  type FrothedMilk = String
  type Espresso = String
  type Cappuccino = String

  case class Water(temperature: Int)

  def grindBeans(beans: CoffeeBeans): GroundCoffee = s"ground coffee of ${beans}"
  def heatWater(water: Water): Water = water.copy(temperature = WATER_INITIAL_TEMP)
  def frothMilk(milk: Milk): FrothedMilk = s"frothed ${milk}"
  def makeEspresso(coffee: GroundCoffee, heatedWater: Water): Espresso = "espresso"
  def makeCappucino(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"

  case class CoffeeGrindingException(msg: String) extends Exception(msg)
  case class MilkFrothingException(msg: String) extends Exception(msg)
  case class WaterBoilingException(msg: String) extends Exception(msg)
  case class CoffeeBrewingException(msg: String) extends Exception(msg)

  def prepareCappucino(): Try[Cappuccino] = for {
    ground <- Try(grindBeans("arabica beans"))
    water <- Try(heatWater(Water(WATER_INITIAL_TEMP)))
    foam <- Try(frothMilk("lactose free milk"))
    espresso <- Try(makeEspresso(ground, water))
  } yield makeCappucino(espresso, foam)

  def grindBeansF(beans: CoffeeBeans): Future[GroundCoffee] = Future {
    println(s"start grinding ${beans} ...")
    Thread.sleep(Random.nextInt(RANDOM_WAIT.toMillis.toInt))
    if(beans == "baked beans") throw CoffeeGrindingException(s"cannot grind ${beans}")
    println(s"... done grinding ${beans}!")
    s"ground coffee of ${beans}"
  }

  def heatWaterF(water: Water): Future[Water] = Future {
    println(s"heating water now ...")
    if(water.temperature < WATER_INITIAL_TEMP) throw WaterBoilingException("water too cold to heat it")
    Thread.sleep(Random.nextInt(RANDOM_WAIT.toMillis.toInt))
    println(s"... and the water is hot, hot, hot!")
    water.copy(temperature = WATER_COFFEE_TEMP)
  }

  def frothMilkF(milk: Milk): Future[FrothedMilk] = Future {
    println(s"start frothing ${milk} now ...")
    Thread.sleep(Random.nextInt(RANDOM_WAIT.toMillis.toInt))
    println(s"... and the milk is foamed!")
    s"frothed ${milk}"
  }

  def makeEspressoF(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
    println(s"start making espresso ...")
    Thread.sleep(Random.nextInt(RANDOM_WAIT.toMillis.toInt))
    println(s"... done making espresso!")
    s"espresso"
  }

  def prepareCappucinoFS(): Future[Cappuccino] = {
    for {
      ground <- grindBeansF("arabica beans")
      water <- heatWaterF(Water(WATER_INITIAL_TEMP))
      foam <- frothMilkF("lactose free milk")
      espresso <- makeEspressoF(ground, water)
    } yield makeCappucino(espresso, foam)
  }

  def prepareCappucinoFP(): Future[Cappuccino] = {
    val groundedBeans = grindBeansF("arabica beans")
    val heatedWater = heatWaterF(Water(WATER_INITIAL_TEMP))
    val foamedMilk = frothMilkF("lactose free milk")

    for {
      ground <- groundedBeans
      water <- heatedWater
      foam <- foamedMilk
      espresso <- makeEspressoF(ground, water)
    } yield makeCappucino(espresso, foam)
  }
}
