package neophyte.part06

import scala.util.Try

object Part06 {
  private final val AGE_LIMIT = 16

  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: neophyte.Part06")

    val child = new Customer(14)
    val teenager = new Customer(16)

    val soldToChild = try {
      buyCigarettes(child, "Marlboro")
      "Here are your cigarettes, but ... it is not good for your health!"
    } catch {
      case UnderAgeException(m) => m
    }
    println(s"Sold Cigarettes: ${soldToChild}")

    val soldToTeenager = buyCigarettes2(teenager, "Marlboro")
    println(s"Sold more Cigarettes: ${soldToTeenager.getOrElse(Cigarettes("""NO"""))}")

    val customers = (10 to 20).map(Customer(_))
    val soldToCustomers = customers.map { c => {
      (c, buyCigarettes2(c, "Marlboro"))
    }}
    val transactions = for {
      (customer, triedToBuy) <- soldToCustomers
      bought <- triedToBuy.recover {
        case e: UnderAgeException => Cigarettes(s"Ooopppsss: ${e.msg}")
      }.toOption
    } yield s"Customer of age ${customer.age} bought a pack of ${bought.brand}"
    transactions.foreach(t => println(t))
  }

  def buyCigarettes(customer: Customer, brand: String): Cigarettes = {
    if(customer.age < AGE_LIMIT) throw UnderAgeException(s"Customer is under the age limit: ${customer.age}/${AGE_LIMIT}")
    else new Cigarettes(brand)
  }

  def buyCigarettes2(customer: Customer, brand: String): Try[Cigarettes] = Try {
    if(customer.age < AGE_LIMIT) throw UnderAgeException(s"Customer is under the age limit: ${customer.age}/${AGE_LIMIT}")
    else new Cigarettes(brand)
  }
}

case class Customer(age: Int)
case class Cigarettes(brand: String)
case class UnderAgeException(msg: String) extends Exception

