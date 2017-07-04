package neophyte

object Part05 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: neophyte.Part05")

    val greetingSome: Option[String] = Some("Hello World")
    val greetingNone: Option[String] = None

    val absentGreeting: Option[String] = Option(null)
    val presentGreeting = greetingSome

    val user = UsersRepository.findById(1)
    val userName = if (user.isDefined) user.get.first else "NONE"
    println(userName)
    println(user.getOrElse("NONE2"))

    user match {
      case Some(u) => println(u.first)
      case None => println("NONE3")
    }

    user.foreach(u => println(u.first))

    println(user.map(_.age))
    println(user.map(_.sex))
    println(user.flatMap(_.sex))

    val allGenders = for {
      u <- UsersRepository.findAll
      first = u.first
      sex <- u.sex
    } yield (first, sex)
    println(allGenders)

    val allGenders2 = for {
      UserRecord(_, first, _, age, Some(sex)) <- UsersRepository.findAll
    } yield (first, age, sex)
    println(allGenders2)
  }
}

sealed abstract class Gender
case object Male extends Gender
case object Female extends Gender

case class UserRecord(id: Int, first: String, last: String, age: Int, sex: Option[Gender])
object UsersRepository {
  private val all = Map(
    1 -> UserRecord(1, "John", "Doe", 32, Some(Male)),
    2 -> UserRecord(2, "Johanna", "Doe", 30, None)
  )

  def findById(id: Int): Option[UserRecord] = all.get(id)
  def findAll: List[UserRecord] = all.values.toList.sortWith((thiz, thaz) => thiz.id > thaz.id)
}
