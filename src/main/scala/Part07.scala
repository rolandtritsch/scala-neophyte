package neophyte

object Part07 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, "Usage: neophyte.Part07")

    val joe = Voter("joe", 10, Right(new Republican))
    val jane = Voter("jane", 20, Left(new Democrat))

    val election = (11 to 20).map {i => {
      Voter(s"Joe_${i}", i, if(i % 2 == 0) Left(new Democrat) else Right(new Republican))
    }}

    val republicans = election.filter(v => v.currentPoliticalViews.isRight)
    val democrats = election.filter(v => v.currentPoliticalViews.isLeft)
    assert(republicans.size == democrats.size)

    val moreDemocrats = election.map { case Voter(name, _, views) => {
        views.left.map(v => (name, views, "democrat"))
    }}
    println(moreDemocrats.toList)

    val sizeOfARepublican = joe.currentPoliticalViews.fold(d => d.toString.size, r => r.toString.size)
    println(sizeOfARepublican)

    val sizeOfADemocrat = jane.currentPoliticalViews.fold(d => d.toString.size, r => r.toString.size)
    println(sizeOfADemocrat)

    println(joe.castTheVote.fold(e => s"Of age ${e.current} cannot vote!!!", p => p.toString))
    println(jane.castTheVote.fold(e => s"Of age ${e.current} cannot vote!!!", p => p.toString))
  }
}

abstract class Party

class Republican extends Party {
  override def toString: String = "Repulican"
}

class Democrat extends Party {
  override def toString: String = "Democrat"
}

case class Voter(name: String, age: Int, currentPoliticalViews: Either[Democrat, Republican]) {
  private val AGE_LIMIT = 18
  def switch = Voter(name, age, if(currentPoliticalViews.isLeft) Right(new Republican) else Left(new Democrat))
  def castTheVote: Either[UnderAgeVoterException, Party] = {
    if(age <= AGE_LIMIT) Left(UnderAgeVoterException(age, AGE_LIMIT))
    else Right(currentPoliticalViews.fold(d => d, r => r))
  }
}

case class UnderAgeVoterException(current: Int, required: Int) extends Exception