package neophyte.part12

object Part12 {
  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part12.getClass.getName.split('$').head}")

    val xs = (1 to 10).map(_.toDouble).toVector
    println(s"Mean: ${Statistics.mean(xs)}")
    println(s"Median: ${Statistics.median(xs)}")
    println(s"Quartiles: ${Statistics.quartiles(xs)}")
    println(s"Iqr: ${Statistics.iqr(xs)}")

    println(s"Mean2: ${Statistics.mean2[Double](xs)}")
    assert(Statistics.mean(xs) == Statistics.mean2(xs))
    println(s"Median2: ${Statistics.median2(xs)}")
    assert(Statistics.median(xs) == Statistics.median2(xs))
    println(s"Quartiles2: ${Statistics.quartiles2(xs)}")
    assert(Statistics.quartiles(xs) == Statistics.quartiles2(xs))
    println(s"Iqr2: ${Statistics.iqr2(xs)}")
    assert(Statistics.iqr(xs) == Statistics.iqr2(xs))
  }
}

object Statistics {
  def mean(xs: Vector[Double]): Double = {
    xs.reduce(_ + _) / xs.size
  }
  def median(xs: Vector[Double]): Double = xs(xs.size/2)
  def quartiles(xs: Vector[Double]): (Double, Double, Double) = {
    (xs(xs.size/4), median(xs), xs(xs.size/4*3))
  }
  def iqr(xs: Vector[Double]): Double = quartiles(xs) match {
    case(upper, _, lower) => upper - lower
  }

  import Math.NumberLike

  def mean2[T](xs: Vector[T])(implicit ev: NumberLike[T]): T = {
    ev.devide(xs.reduce(ev.plus(_, _)), xs.size)
  }
  def median2[T: NumberLike](xs: Vector[T]): T = xs(xs.size/2)
  def quartiles2[T: NumberLike](xs: Vector[T]): (T, T, T) = {
    (xs(xs.size/4), median2(xs), xs(xs.size/4*3))
  }
  def iqr2[T: NumberLike](xs: Vector[T]): T = quartiles2(xs) match {
    case(upper, _, lower) => implicitly[NumberLike[T]].minus(upper, lower)
  }
}

object Math {
  //import annotation.implicitNotFound
  //@implicitNotFound("No member of type class NumberLike in scope for ${T}")
  trait NumberLike[T] {
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T
    def devide(x: T, y: Int): T
  }

  object NumberImplicits {
    implicit object NumberLikeInt extends NumberLike[Int] {
      override def plus(x: Int, y: Int): Int = x + y
      override def minus(x: Int, y: Int): Int = x - y
      override def devide(x: Int, y: Int): Int = x / y
    }

    implicit object NumberLikeDouble extends NumberLike[Double] {
      override def plus(x: Double, y: Double): Double = x + y
      override def minus(x: Double, y: Double): Double = x - y
      override def devide(x: Double, y: Int): Double = x / y
    }
  }

  object JodaImplicits {
    import org.joda.time.Duration

    implicit object NumberLikeDuration extends NumberLike[Duration] {
      override def plus(x: Duration, y: Duration): Duration = x.plus(y)
      override def minus(x: Duration, y: Duration): Duration = x.minus(y)
      override def devide(x: Duration, y: Int): Duration = Duration.millis(x.getMillis/y)
    }
  }
}