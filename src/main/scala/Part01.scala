package neophyte.part01

object Part01 {
  def main(args: Array[String]): Unit = {
    // @todo add/fix all usage messages to reflect new package structure
    require(args.size == 0, "Usage: neophyte.part01.Part01")
    val users = List(
      EMailAddress("Joe", "Doe", 40),
      EMailAddress("Roland", "Tritsch", 2)
    )
    println(s"Score: ${advance(users)}")

    val puser = new PremiumUser("Daniel", 42)
    println(s"Greeting: ${greeting(puser)}")

    val fuser = new FreeUser("Sam", 38, 0.8)
    println(s"Greeting: ${greeting2(fuser)}")
    println(s"Greeting: ${greeting3(fuser)}")
  }

  def advance(users: List[EMailAddress]): Int = {
    require(users.size >= 2)

    users match {
      case EMailAddress(_, _, score0) :: EMailAddress(_, _, score1) :: _ => score0 + score1
      case _ => 0
    }
  }

  def greeting(u: BaseUser): String = u match {
    case FreeUser(name, _, _) => s"Hello ${name}"
    case PremiumUser(name, _) => s"Welcome back ${name}"
  }

  def greeting2(u: BaseUser): String = u match {
    case FreeUser(name, _, p) if (p > FreeUser.upgradeProbablityThreshold) => s"Hello ${name}, what can I do for you today?"
    case FreeUser(name, _, _) => s"Hello ${name}"
    case PremiumUser(name, _) => s"Welcome back ${name}"
  }

  def greeting3(u: BaseUser) = u match {
    case freeUser @ PremiumCandidate() => s"Hello ${freeUser.name}, what can I do for you today? How about an upgrade?"
    case _ => s"Hello you"
  }
}

case class EMailAddress(firstName: String, lastName: String, score: Int)

trait BaseUser {
  val name: String
  val score: Int
}

object FreeUser {
  val upgradeProbablityThreshold = 0.75

  def unapply(u: FreeUser) = Some((u.name, u.score, u.upgradeProbability))
}
class FreeUser(val name: String, val score: Int, val upgradeProbability: Double) extends BaseUser

object PremiumUser {
  def unapply(u: PremiumUser) = Some((u.name, u.score))
}
class PremiumUser(val name: String, val score: Int) extends BaseUser

object PremiumCandidate {
  def unapply(u: FreeUser): Boolean = u.upgradeProbability > FreeUser.upgradeProbablityThreshold
}
