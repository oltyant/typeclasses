package examples.printable

//the type class itself
trait Printable[A] {
  def format(a: A): String

  //strange function, conteramap which seems to be the reverse function
  def contramap[B](f: B => A): Printable[B] = {
    val self = this
    new Printable[B] {
      def format(b: B): String = self.format(f(b)) 
    }
  }
}

final case class Person(id: Int, name: String, address: String)
final case class Box[A](value: A)

//typeclass instances
object PrintableInstances {
  implicit val stringToPrintable = new Printable[String] {
    def format(a: String) = a
  }
  implicit val personToPrintable = new Printable[Person] {
    def format(p: Person) = s"The person ${p.name} has an id: ${p.id} and lives at ${p.address}"
  }
  implicit def optionToPrintable[A : Printable] = new Printable[Option[A]] {
    def format(a: Option[A]) = a match {
      case Some(v) => implicitly[Printable[A]].format(v)
      case None    => "None"
    }
  }
  implicit def boxPrintable[A](implicit p: Printable[A]) = p.contramap[Box[A]](_.value)
}

//typeclass interface object
object Printable {
  def apply[A : Printable] = implicitly[Printable[A]]
  def format[A : Printable](a: A) = Printable[A].format(a)
  def print[A : Printable](a: A) = println(format(a))
}

//typeclass interface syntax
object PrintableSyntax {
  implicit class PrintableOps[A : Printable](a: A) {
    def format: String = Printable[A].format(a)
    def print: Unit = println(format)
  }
}

object PrintableMain extends App {
  import PrintableInstances._
  import PrintableSyntax._

  val frodo = Person(1, "Frodo Baggins", "Bag End")
  Printable.print(frodo)
  //with syntax - monkey patch the person
  frodo.print

  //contramap usage: Advanced
  //1. box which has Printable instances
  val box = Box("Ipiapacs")
  val box2 = Box(frodo)
  //2. call format on them gives back the wrapped and formatted value
  box.print
  box2.print
}
