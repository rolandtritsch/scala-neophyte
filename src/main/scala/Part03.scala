package neophyte.part03

object Part03 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part03.getClass.getName.split('$').head}")

    /*
    You can pattern match on ...

    - case classes
    - types (Int, Long, Short, ...)
    - lists/seqs/colls (head :: tail, first :: second :: rest, ...)
    - values ("Roland", 42, ...)
    */

    println(welcome(Player("Werner", 100001)))

    val player = currentPlayer()
    val Player(name, _) = player
    println(s"Welcome back, ${name}!")

    val scores = (1 to 10).map(_ * 10).toList.reverse
    println(highestScore(scores).getOrElse(-1234))
    println(highestScore(List()).getOrElse(-1234))

    println(currentScore(0))
    println(currentScore(0L))
    println(currentScore(0.0))
    println(currentScore(0.0F))

    val p = Player("Roland", 1)
    val p2 = Player("Daniel", 100)
    val (winner, score) = gameResult(p, p2)
    println(s"${winner} did win the game with a score of ${score}")

    println(s"Hall of Fame: ${hallOfFame().mkString(""", """)}")

    val lists = List(1, 2, 3) :: List.empty :: List(5, 3) :: Nil
    println(sizeOfLists(lists))
  }

  def welcome(p: Player): String = p match {
    case Player(_, score) if (score > 100000) => s"Wow!!! ${score}!!! That's a high score! Welcome back!"
    case Player(name, _) => s"Welcome back, ${name}! Wanna play chess?"
    case _ => {
      assert(false, "Ooopppsss"); ""
    }
  }

  def currentPlayer(): Player = Player("Roland", 1)

  def highestScore(scores: List[Int]): Option[Int] = {
    try {
      val highScore :: allOtherScores = scores
      Some(highScore)
    } catch {
      case ex: MatchError => {
        println("Ooopppsss")
        None
      }
    }
  }

  def currentScore(s: AnyVal): String = s match {
    case x: Int => "Int score"
    case x: Long => "Long score"
    case x: Double => "Double score"
    case x: Float => "Float score"
    case _ => {
      assert(false, "Ooopppsss"); ""
    }
  }

  def gameResult(p: Player, p2: Player): (String, Int) = {
    if (p.score >= p2.score) (p.name, p.score - p2.score)
    else (p2.name, p2.score - p.score)
  }

  def gameResults(): Seq[(String, Int)] =
    ("Daniel", 3500) :: ("Melissa", 13000) :: ("John", 7000) :: Nil

  def hallOfFame(): Seq[String] = for {
    (name, score) <- gameResults()
    if (score > 5000)
  } yield name

  def sizeOfLists(lists: List[List[Int]]): List[Int] = for {
    list@head :: _ <- lists
  } yield list.size
}

case class Player(name: String, score: Int)
