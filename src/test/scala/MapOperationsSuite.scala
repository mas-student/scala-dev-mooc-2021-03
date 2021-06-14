
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import module1.opt


class MapOperationsSuite extends FlatSpec with Matchers with PropertyChecks {

  "option orElse" should "produce map containing all the keys" in
    forAll { (o1: Option[Int], b1: Option[Int]) =>

      println(o1)
      Option(null).orElse(b1) shouldBe b1
    }

  case class Age(amount: Long)
  implicit val genAge: Gen[Age] = for {age <- Gen.oneOf(1 to 5)} yield Age(age)
  implicit val arbitraryEntityType: Arbitrary[Age] = Arbitrary(genAge)

  "option orElse" should "produce map containing all the keys 2" in
    forAll { (o1: Age) =>
      println(o1)

    }

  object opt {
    sealed trait Option[+A]{
      def isEmpty: Boolean = this match {
        case Option.Some(_) => false
        case Option.None => true
      }

      def get: A = this match {
        case Option.Some(v) => v
        case Option.None => throw new Exception("Get on empty list")
      }

      def getOrElse[B >: A](b: B): B = this match {
        case Option.Some(v) => v
        case Option.None => b
      }

      def orElse[B >: A](b: Option[B]): Option[B] = this match {
        case Option.Some(v) => Option.Some(v)
        case Option.None    => b
      }

      def map[B](f: A => B): Option[B] = this match {
        case Option.Some(v) => Option.Some(f(v))
        case Option.None => Option.None
      }


      def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case Option.Some(v) => f(v)
        case Option.None    => Option.None
      }

      // val i : Option[Int]  i.map(v => v + 1)

      def filter(predicate: A => Boolean): Option[A] = this match {
        case Option.Some(v) if predicate(v) => Option.Some(v)
        case Option.Some(_)                   => Option.None
        case Option.None                      => Option.None
      }


      def f(x: Int, y: Int): Option[Int] =
        if(y == 0) Option.None
        else Option.Some(x / y)

      def zip[B](b: Option[B]): Option[(A, B)] = {
        if (b.isEmpty) Option.None
        else
          this match {
            case Option.Some(v)  => Option.Some((v, b.get))
            case Option.None     => Option.None
          }
      }


    }

    object Option{
      case class Some[A](v: A) extends Option[A]
      case object None extends Option[Nothing]

      def apply[A](x: A): Option[A] = if (x == null) None else Some(x)
    }
  }

  implicit val genOption: Gen[opt.Option[Int]] =
    for {age <- arbitrary[Int]}
      yield opt.Option.Some(age)

  implicit val arbitraryOption: Arbitrary[opt.Option[Int]] = Arbitrary(genOption)


  "option orElse" should "produce option" in
    forAll { (o1: opt.Option[Int], o2: opt.Option[Int]) =>
      println(o1)
      println(o2)
      opt.Option(null).orElse(o1) shouldBe o1
      opt.Option.None.orElse(o1) shouldBe o1
      o2.orElse(o1) shouldBe o2
    }

  "option orElse 2" should "produce option 2" in
    forAll { (o1: opt.Option[Int], o2: opt.Option[Int]) =>
      println(o1)
      println(o2)
      opt.Option(null).orElse(o1) shouldBe o1
      opt.Option.None.orElse(o1) shouldBe o1
      o2.orElse(o1) shouldBe o2
    }

  "option zip" should "produce zip option" in
    forAll { (o1: Int, o2: Int) =>
      opt.Option.Some("foo") zip opt.Option.Some("bar") shouldBe opt.Option.Some(("foo", "bar"))
      opt.Option.Some("foo") zip opt.Option.None shouldBe opt.Option.None
      opt.Option.None zip opt.Option.Some("bar") shouldBe opt.Option.None
      opt.Option.None zip opt.Option.None shouldBe opt.Option.None
    }

  "option" should "right identity" in
    forAll { (o1: Int, o2: Int) =>
      Option(o1).flatMap{ x => Option(x)} shouldBe Option(o1)
      val f: (Int => Option[Int]) = x => Some(x * 2)
      val g: (Int => Option[Int]) = x => Some(x + x)
      Option(o1).flatMap(f) shouldBe f(o1)

      Option(o1).flatMap(f).flatMap(g) shouldBe Option(o1).flatMap(f(_).flatMap(g))
    }

  "option" should "custom monad" in
    forAll { (o1: Int, c: Int => Option[Int], f: Int => Option[Int]) =>
      Option(o1).flatMap(f) shouldBe f(o1)
      Option(o1).flatMap(Option(_)) shouldBe Option(o1)

      Option(o1).flatMap(f).flatMap(c) shouldBe Option(o1).flatMap(f(_).flatMap(c))
    }

  "option" should "custom monad 2" in
    forAll { (o1: Int) =>
      val f: Int => opt.Option[Int] = x => opt.Option.Some(x * 2)
      val g: Int => opt.Option[Int] = x => opt.Option.Some(x + x)

      opt.Option.Some(o1).flatMap(f) shouldBe f(o1)
      opt.Option.Some(o1).flatMap(opt.Option.Some(_)) shouldBe opt.Option.Some(o1)

      opt.Option.Some(o1).flatMap(f).flatMap(g) shouldBe opt.Option.Some(o1).flatMap(f(_).flatMap(g))
    }

  "option" should "should corr filter" in
    forAll { (o1: Int, p1: Int => Boolean) =>

      if(p1(o1))
        opt.Option.Some(o1).filter(p1) shouldBe opt.Option.Some(o1)
      else
        opt.Option.Some(o1).filter(p1) shouldBe opt.Option.None

    }


}