package neophyte.part02

object Part02 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: neophyte.part02.Part02")

    val lWithThree = (0 to 2).toList.map(_ * 10)
    val lWithMore = (0 to 9).toList.map(_ * 3)
    println(s"total short: ${total(lWithThree)}")
    println(s"total long: ${total(lWithMore)}")

    val name = "Roland Werner Tritsch"
    println(s"${greetWithFirstName(name)}")
    println(s"${greetWithFirstName("")}")
    println(s"${greetWithFullName(name)}")
    println(s"${greetWithFullName("")}")
    println(s"${greetWithFullName(name.split(" ").drop(1).mkString(" "))}")
    println(s"${greetWithFullName(name.split(" ").drop(2).mkString(" ").drop(2))}")
  }

  def total(l: List[Int]): Int = l match {
    case List(_, b, c) => b * c
    case List(a, b, c, _*) => a + b + c
    case _ => {assert(false, "Ooopppsss"); 0}
  }

  def greetWithFirstName(name: String): String = name match {
    case GivenNames(first, _*) => s"Good morning, ${first}!!!"
    case _ => s"Good morning, sir!!!"
  }

  def greetWithFullName(name: String): String = name match {
    case Names(last, first, _*) => s"Good morning, ${first} ${last}!!!"
    case _ => s"Good morning, SIR!!!"
  }
}

object GivenNames {
  def unapplySeq(name: String): Option[Seq[String]] = {
    val names = name.trim.split(" ").filter(_.nonEmpty)
    if(names.isEmpty) None else Some(names)
  }
}

object Names {
  def unapplySeq(name: String): Option[(String, String, Seq[String])] = {
    val names = name.trim.split(" ").filter(_.nonEmpty)
    if(names.size < 2) None
    else Some((names.last, names.head, names.drop(1).dropRight(1)))
  }
}