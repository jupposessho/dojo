package jupposessho

import shapeless._

trait Adder[A] {

	def add(value: A, field: String): A
}

object Adder {

	def apply[A](implicit adder: Adder[A]): Adder[A] = adder

	def pure[A](func: A => A): Adder[A] = new Adder[A] {
		def add(value: A, field: String): A = func(value)
	}

	implicit val intAdder: Adder[Int] = Adder.pure(num => num + 1)

	implicit val hNilAdder: Adder[HNil] = Adder.pure(hnil => hnil)

	implicit def hListAdder[H, T <: HList](
		implicit
		hAdder: Lazy[Adder[H]],
		tAdder: Adder[T]
		): Adder[H :: T] =
	Adder.pure(pair => hAdder.value.add(pair.head, "") :: tAdder.add(pair.tail, ""))

	implicit def genericAdder[A, R <: HList](
		implicit
		gen: Generic.Aux[A, R],
		rAdder: Lazy[Adder[R]]
		): Adder[A] =
	Adder.pure { a => 
		gen.from(rAdder.value.add(gen.to(a), ""))
	}
}

object Main {
	def main(args: Array[String]): Unit = {}

  def addOne[A](value: A)(implicit adder: Adder[A]): A = adder.add(value, "")

	case class Foo(a: Int, b: Int)
	case object BAR
	val foo = Foo(1, 2)

	println(addOne(2))
	println(addOne(foo))
	println(addOne(BAR))
}
