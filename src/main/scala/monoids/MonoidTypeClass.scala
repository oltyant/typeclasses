package monoids

import examples.printable.Person

trait SemiGroup[A] {
  def combine(a: A, b: A): A
}

trait Monoid[A] extends SemiGroup[A] {
  val identity: A
}

object MonoidInstances {
  implicit val intToMonoid = new Monoid[Int] {
    def combine(a: Int, b: Int) = a + b
    val identity = 0
  }
  implicit val stringToMonoid = new Monoid[String] {
    def combine(a: String, b: String) = a + b
    val identity = ""
  }
  implicit val personToSemiGroup = new SemiGroup[Person] {
    def combine(a: Person, b: Person) = Person(a.id + b.id, a.name + b.name, a.address + b.address)
  }
  implicit def optionToMonoid[A : Monoid] = new Monoid[Option[A]] {
    def combine(a: Option[A], b: Option[A]) = (a, b) match {
      case (None, None)          => Option.empty[A]
      case (left@Some(l), None)  => left
      case (None, right@Some(r)) => right
      case (Some(l), Some(r))    => Some(implicitly[Monoid[A]].combine(l, r))
    }
    val identity = None
  }
}

object Monoid {
  def apply[A : Monoid] = implicitly[Monoid[A]]
  def combine[A : Monoid](a: A, b: A) = Monoid[A].combine(a, b)
  def identity[A: Monoid] = Monoid[A].identity 
}

object MonoidOps {
  implicit class MonoidSyntax[A : Monoid](a: A) {
    def combine(b: A) = Monoid[A].combine(a, b)
    def |+|(b: A) = combine(b)
    def identity = Monoid[A].identity
  }
}

object MonoidMain extends App {
  import MonoidInstances._
  import MonoidOps._

  val x: Int = 1
  val y: Int = 2
  println(1 |+| 2)

  //wo explicit result type it thinks that noneString is None while someString is Some
  //and we have only monoid for Option, not for Some and None
  val noneString: Option[String] = None
  val someString: Option[String] = Some("Something")

  println(noneString |+| someString)
  println(someString |+| someString)
  println(someString.identity)
}
