package neophyte.part13

object Part13 {
  import Franchise.Character

  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part13.getClass.getName.split('$').head}")

    val dc = new Franchise("DC")
    val superman = Character("Superman")
    val batman = Character("Batman")
    val (love, desire) = dc.createFanFictionWith(superman, batman)
    println(s"${love} makes out with ${desire}")

    val starTrek = new Franchise("Star Trek")
    val quark = Franchise.Character("Quark")
    val jadzia = Franchise.Character("Jadzia Dax")

    val starWars = new Franchise("Star Wars")
    val luke = Franchise.Character("Luke Skywalker")
    val yoda = Franchise.Character("Yoda")

    val (thiz, thaz) = starTrek.createFanFictionWith(jadzia, luke)
    println(s"${thiz} makes out with ${thaz}")

    val starTrek2 = new BetterFranchise("Star Trek")
    val quark2 = starTrek2.Character("Quark")
    val jadzia2 = starTrek2.Character("Jadzia Dax")

    val starWars2 = new BetterFranchise("Star Wars")
    val luke2 = starWars2.Character("Luke Skywalker")
    val yoda2 = starWars2.Character("Yoda")

    /*
    34. Waiting for source changes... (press enter to interrupt)
    [info] Compiling 1 Scala source to /Users/rtritsch/Development/Home/scala-neophyte/target/scala-2.12/classes...
    [error] /Users/rtritsch/Development/Home/scala-neophyte/src/main/scala/Part13.scala:34: type mismatch;
    [error]  found   : starWars2.Character
    [error]  required: starTrek2.Character
    [error]     val (thiz2, thaz2) = starTrek2.createFanFictionWith(jadzia2, luke2)
    [error]                                                                  ^
    [error] one error found
    [error] (compile:compileIncremental) Compilation failed
    [error] Total time: 0 s, completed 26-Jul-2017 12:35:17
    */
    //val (thiz2, thaz2) = starTrek2.createFanFictionWith(jadzia2, luke2) // won't compile (see above)
    val (thiz2, thaz2) = starTrek2.createFanFictionWith(jadzia2, quark2)
    println(s"${thiz2} makes out with ${thaz2}")

    // Part II of Part 13 :) - AwesomeDB ...

    val datastore = new AwesomeDB
    datastore.set(Keys.foo)(42)
    val i = datastore.get(Keys.foo)
    println(s"AwesomeDB (Key/Value): ${Keys.foo}/${i}")

    //datastore.set(Keys.foo)("42") // won't compile
  }
}

object Franchise {
  case class Character(name: String)
}

class Franchise(name: String) {
  import Franchise.Character

  def createFanFictionWith(lovestruck: Character, objectOfDesire: Character): (Character, Character) = {
    (lovestruck, objectOfDesire)
  }
}

class BetterFranchise(name: String) {
  case class Character(name: String)

  def createFanFictionWith(lovestruck: Character, objectOfDesire: Character): (Character, Character) = {
    (lovestruck, objectOfDesire)
  }
}

object AwesomeDB {
  abstract class Key(name: String) {
    type Value

    override def toString: String = name
  }
}

import AwesomeDB.Key

class AwesomeDB {
  import collection.mutable.Map

  val data = Map.empty[Key, Any]
  def get(key: Key): Option[key.Value] = data.get(key).asInstanceOf[Option[key.Value]]
  def set(key: Key)(value: key.Value): Unit = data.update(key, value)
}

trait IntValued extends Key {
  override type Value = Int
}

trait StringValued extends Key {
  override type Value = String
}

object Keys {
  val foo = new Key("foo") with IntValued
  val bar = new Key("bar") with StringValued
}