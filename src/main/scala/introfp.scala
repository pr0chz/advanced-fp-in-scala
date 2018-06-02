package lambdaconf.introfp

import scala.io.Source
import scala.util.Random

object functions {

  // function: domain -> codomain
  object totality {
    // total vs not-total - has to associate all items in domain set to codomain set item
    // what for: no exceptions, rely on the types and compiler
    def notTotal1(s: String): Int = s.toInt

    def notTotal2(s: String): String = null // depends on whether we include null into codomain, in FP we usually do not
    def total1(s: String): Unit = ()

    def total2(s: Int): String = s.toString

    def notTotal3(s: String): Nothing = ??? // Nothing is a type with no values, so there is no way how to map anything from domain to codomain (no implementation that can return value)
  }

  object determinism {
    // f(a1) = b1; f(a2) = b2; a1 == a2 => b1 == b2
    // what for: testability

    var i: Int = 0
    // () => Long
    def notDeterministic1(): Int = { i += 1; i }
    def notDeterministic2(): Long = System.currentTimeMillis()
    def notDeterministic3(): Double = Random.nextDouble()

    def deterministic1(x: Int) = x * x

    // not-pure deterministic function
    def deterministic2(x: Int): Int = {
      try println("Hello world") catch { case _: Throwable => }
      x*x
    }
  }

  object purity {
    // computation has to be only 'effect'
    // what for: testability, reasoning by type (for impure you always have to look into implementation), equational reasoning (substitution model)
    def impure1(x: Int): Int = {
      println("Hello world")
      x * x
    }
    def impure2(): String = Source.fromInputStream(System.in).mkString

    def pure = "hello"

    // TODO: does pure not-deterministic function exist?
  }

  object functions {
    // Int => Int
    def abs(i: Int): Int = if (i<0) -i else i
    // List[Int] => Option[Int]
    def headOption(l: List[Int]): Option[Int] = l.headOption
    // Int => Boolean
    def odd(i: Int): Boolean = abs(i) % 2 == 1
  }

  object higher_order {
    // higher order function is a function that takes or returns function or both
    // combinator: takes a function and returns a function (e.g. compose)

    type Error = String
    type Parser[A] = String => Either[Error, (String, A)]

    def or[A](l: Parser[A], r: Parser[A]): Parser[A] = {
      x: String => l(x) match {
        case Left(_) => r(x)
        case v => v
      }
      // TODO rewrite using some Either combinators
    }

  }

  object polymorphism {
    // functions for multiple types
    // scala does not support parametric polymorphic functions (introduced with val), just methods
    // note: other type of polymorphism - subtype polymorphism (inheritance)

    val f: Int => String = ??? // function, can be passed as a value
    def f2[A](x: A): A = x // method, cannot be passed, but can be parameter polymorphic

    // val identity: ??? = ??? // cannot be done

    // trick to make polymorphic functions passable around
    trait Identity {
      // type-driven development
      // def apply[A](a: A): A = ??? // what possible implementations there are?
      def apply[A](a: A): A = a

      // Parametrical polymorphism interpretation (one possibility)
      // (A: Type) => (A => A) - given a type, returns a monomorphic function
      // foo[A, B, C](a, b, c)
      // List[Int] ...
    }
    object identity extends Identity

    identity(1)
    identity("foo")

    trait First {
      def apply[A, B](t: (A, B)): A = t._1
    }
    object first extends First

    trait Second {
      def apply[A, B](t: (A, B)): B = t._2
    }
    object second extends Second

    // "nice" subset of scala https://github.com/scalaz/scalazzi

    // TODO what are these traits for? passing around? we should probably pass these as a general function not a First type, so what is it for?

    object secondAsObject {
      def apply[A, B](t: (A, B)): A = t._1
    }

    def x[A,B,C](v: (A, B))(f: ((A, B)) => C): C = f(v)

    x((1,7))(secondAsObject.apply)

  }

  object types {
    // type is a set of possible values

    object products {
      // isomorphic representations of product type
      case class Person(name: String, age: Int)
      type Person2 = (String, Int)
    }
    object sums {
      sealed trait Lunch
      // sum terms
      final case class Vegetarian(description: String) extends Lunch
      final case class Paleo(description: String) extends Lunch
    }
  }

  object excercies {
    // degenerate sum type, just one term
    // degenerate product type, no term
    case class Foo()

    // identity for product is unit
    type String2 = (String, Unit)
    def sToS2(s: String): String2 = (s, ())
    def s2Tos(s: String2): String = s._1

    // A * 1 = A

    // Either[Unit, A] // 1 + A
    // Option[A] // 1 + A

    // identity for sum is Nothing
    type String3 = Either[Nothing, String]

    def sToS3(s: String): String3 = Right(s)
    def s3Tos(s: String3): String = s match {
      case Left(n) => n // dead code
      case Right(s) => s
    }

    // Nothing re-implemented

    final abstract class Void {
      def absurd[A]: A // can't be implemented (how to create A from thin air?)
    }

    type String4 = Either[Void, String]

    def s4ToS(s: String4): String = s match {
      case Left(v) => v.absurd[String] // compiler will not complain
      case Right(v) => v
    }

  }

  object type_constructor {

    // data constructor
    case class Person(name: String)
    object Person {
      def apply(name: String): Person = new Person(name)
    }
    Person.apply("name")

    // type constructor construct types
    // List is not a type, but a type constructor (type => type)
    // type == set of values

    // good note: more polymorphic the function is, the less ways to implement it

    // kinds
    // List : * => *
    // Map  : [*, *] => *
    //

    // StackLike : (* => *) => *
    // StackLike : S => * where S = (* => *)
    trait StackLike[Stack[_]] { // this type parameter is not used for anything, so we can use _ here // TODO can we create for example Stack[B] and use B below??
      def push[A](s: Stack[A], a: A): Stack[A]
      def pop[A](s: Stack[A]): Option[(A, Stack[A])]
    }

    // * => (* => *) is invalid in Scala, each kind has to return a type not a type constructor!
    // TODO * => * => * invalid as well?
    // from Scala3 it would be possible to use Map[Int, _] to "partially apply" a type

    // StackLike[List]
    // StackLike[Deque]
    // StackLike[Vector]
    // StackLike[String] // wrong
    // StackLike[Map] // wrong
    // StackLike[Function0] // ok - trait Function0[+R] extends AnyRef

    val StackLikeList = new StackLike[List] {
      override def push[A](s: List[A], a: A): List[A] = ???
      override def pop[A](s: List[A]): Option[(A, List[A])] = ???
    }

    // A[_, _, _] =>
    // A[_[_]] : (* => *) => * (A[S] where S = _[_])
    // A[_[_,_], _[_]] : [ [*, *] => *, * => *] => *
    //   A[K, V] where K = _[_,_] and V = _[_]
    // A[_[_[_]], _, _[_,_,_]] : [(* => *) => *, *, [*, *, *] => *] => *
    //   - example: A[StackLike, String, Tuple3]

    // (* => *) => * // A[_[_]]
    // [[*, *] => *, * => *] // A[_[_, _], _[_]]
    // [((* => *) => *) => *, * => *] => * // A[_[_[_[_]]],_[_]]

    // types as propositions
    // curry howard
    // types pose a proposition and proposition is true when function can be implemented

    // false
    def prop1[A, B]: A => B = ???

    // true
    def prop2[A, B <: A]: B => A = (b: B) => b


    def repeat0(n: Int, s: String): String =
      // many ways how to implement this, types do not give us a clue
      "john"

    def repeat1[A](n: Int, s: A, f: (A, A) => A): A = {
      s // less possibilities but still not optimal
    }

    // TODO homework
    def concat(x: String, y: String): String = x + y
    def repeat2[A](n: Int, f: A => A): A => A = {
      x => if (n == 0) x else repeat2(n - 1, f)(x)
    }
    def repeatString(n: Int, s: String) = repeat2[String](n, str => concat(str, s))

    // TYPECLASSES

    trait CollectionLike[F[_]] {
      def foldLeft[A, Z](fa: F[A])(z: Z)(f: (Z, A) => Z): Z
    }

    val ListCollectionLike = new CollectionLike[List] {
      override def foldLeft[A, Z](fa: List[A])(z: Z)(f: (Z, A) => Z): Z = fa.foldLeft(z)(f)
    }

    // Map[K, V] - we have to use kind-projection until Scala3
    def MapValueCollectionLike[K] = new CollectionLike[Map[K, ?]] {
      override def foldLeft[A, Z](fa: Map[K, A])(z: Z)(f: (Z, A) => Z): Z = fa.values.foldLeft(z)(f)
    }

    // other approaches how to partially apply the type
    def MapValueCollectionLike2[K] = {
      type MapK[A] = Map[K, A]
      new CollectionLike[MapK] {
        override def foldLeft[A, Z](fa: Map[K, A])(z: Z)(f: (Z, A) => Z): Z = fa.values.foldLeft(z)(f)
      }
    }

    def MapValueCollectionLike3[K] = new CollectionLike[({ type L[V] = Map[K, V]})#L] { // existential type, something kind projection uses under the hood
        override def foldLeft[A, Z](fa: Map[K, A])(z: Z)(f: (Z, A) => Z): Z = fa.values.foldLeft(z)(f)
    }

    // collection like for Tuple2 - we have to pick one - first or the other tuple item

    def Tuple2CollectionLikeFirst[B] = new CollectionLike[Tuple2[?, B]] {
      override def foldLeft[A, Z](fa: (A, B))(z: Z)(f: (Z, A) => Z): Z = f(z, fa._1)
    }

    def Tuple2CollectionLikeFirst2[B] = {
      type Tuple2Applied[A] = Tuple2[A, B] // TODO what does type X[A] mean?
      new CollectionLike[Tuple2Applied] {
        override def foldLeft[A, Z](fa: Tuple2Applied[A])(z: Z)(f: (Z, A) => Z): Z = f(z, fa._1)
      }
    }

    val t = Tuple2CollectionLikeFirst.foldLeft(("aaa", 1))("ccc")((z: String, a:String) => z + a)


    // SIZE

    trait Sized[F[_]] {
      def size[A](fa: F[A]): Int
    }

    val ListSized = new Sized[List] {
      def size[A](fa: List[A]): Int = fa.size
    }

    def MapSized[K] = new Sized[Map[K, ?]] {
      override def size[A](fa: Map[K, A]): Int = fa.keys.size
    }

    // we still have to use kind projection or type lambda to suggest return type, otherwise it will not typecheck when calling
    def MapSized2[K]: Sized[Map[K, ?]] = {
      type MapK[V] = Map[K, V]
      new Sized[MapK] {
        override def size[A](fa: MapK[A]): Int = fa.keys.size
      }
    }

    // implementation via type lambda
    def MapSized3[K] = new Sized[({ type MapK[V] = Map[K, V] })#MapK ] {
      override def size[A](fa: Map[K, A]): Int = fa.keys.size
    }

    val x = MapSized3.size(Map(4 -> "ads", 2 -> "dsad"))

  }

  object existensials {

    // existential type is a dual to universal type

    // list that composes all map operations to a single one (to prevent recreation of list on each map operation)
    // there is no way how to implement this without existencial type (TODO ?)
    trait FastMap[A] { self =>
      type A0  // existential type, proof of the existence of the type, but we cannot really say anything about what type it is
      val original: Seq[A0]
      val mapping: A0 => A

      def map[B](f: A => B): FastMap[B] = new FastMap[B] {
        type A0 = self.A0
        val original = self.original
        val mapping = self.mapping.andThen(f)
      }

      def run: Seq[A] = original.map(mapping)
    }

    object FastMap {
      def apply[A](as: A*): FastMap[A] = new FastMap[A] {
        type A0 = A
        val original = Seq(as: _*)
        val mapping = identity
      }
    }

    // equivalent codes
    def size1(l: List[_]): Int = l.size // _ is existential type here
    def size2[A](l: List[A]): Int = l.size
  }

  object type_classes {
    // parametric polymorphism removes structure from code
    // type classes add it - minimum structure necessary to solve a problem

    //def repeat[A](n: Int, a: A): A = ???

    // type class - bundle of operations, types and laws for operations
    // enables us to extend not our types

    // append: (A, A) => A
    trait SemiGroup[A] {
        // Must satisfy: Associativity Law - append(append(a1, a2), a3) == append(a1, append(a2, a3))
        def append(l: A, r: A): A

        // Enforcing the check: each type class should go togehter with scalacheck functions to test the laws
    }

    // OO aproach, not a good idea :)
    trait Appendable { self =>
      def append(r: self.type): self.type
    }

    object SemiGroup {
      // instance of type class for Int
      implicit val IntAdditionSemiGroup = new SemiGroup[Int] { // there could be multiple interpretations, this one is addition
        override def append(l: Int, r: Int): Int = l + r
      }

      implicit def SemiGroupTuple2[A, B](implicit A: SemiGroup[A], B: SemiGroup[B]): SemiGroup[(A, B)] = new SemiGroup[(A, B)] {
        override def append(l: (A, B), r: (A, B)): (A, B) = (A.append(l._1, r._1), B.append(l._2, r._2))
      }

      implicit val SemiGroupString = new SemiGroup[String] {
        override def append(l: String, r: String): String = l + r
      }

      implicit def SemiGroupList[A] = new SemiGroup[List[A]] {
        override def append(l: List[A], r: List[A]): List[A] = l ++ r
      }

//      implicit def SemiGroupMap[K, V](implicit A: SemiGroup[V]) = new SemiGroup[Map[K, V]] {
//        override def append(l: Map[K, V], r: Map[K, V]): Map[K, V] = {
//          def values(key: K) = List(l.get(key), r.get(key)).flatten
//          val x = for (key <- l.keySet ++ r.keySet) yield (key, values(key).reduce(A.append))
//          x.toMap
//        }
//      }

      implicit def SemiGroupMap2[K, V: SemiGroup] = new SemiGroup[Map[K, V]] {
        override def append(l: Map[K, V], r: Map[K, V]): Map[K, V] = {
          def values(key: K) = List(l.get(key), r.get(key)).flatten
          val x = for (key <- l.keySet ++ r.keySet) yield (key, values(key).reduce(_ <> _))
          x.toMap
        }
      }

      // Allows us to create a semigroup like this: SemiGroup[Int]()
      def apply[A](implicit S: SemiGroup[A]): SemiGroup[A] = S
    }


    def repeat[A](n: Int, a: A)(implicit S: SemiGroup[A]): A =
      if (n <= 1) a
      else S.append(a, repeat(n - 1, a)(S))

    repeat(5, 5)
    repeat(5, (3, 2))

    // context-bound
    def repeat2[A: SemiGroup](n: Int, a: A): A =
      if (n <= 1) a
      else implicitly[SemiGroup[A]].append(a, repeat(n - 1, a))

    // context-bound & using apply
    def repeat3[A: SemiGroup](n: Int, a: A): A =
      if (n <= 1) a
      else SemiGroup[A].append(a, repeat(n - 1, a))


    // where to put the definitions:
    //   - if we control the type - put it into the companion object of type
    //   - if we control the type class - put it into the type class companion object
    //
    // we should have just a single instance for a given type (TODO ?)

    implicit class SemigroupSyntax[A](l: A) {
      def <> (r: A)(implicit S: SemiGroup[A]): A = S.append(l, r)
    }

    // context-bound & using apply & using implicit class operator
    // this method signature is much better in terms of describing what this function does
    def repeat4[A: SemiGroup](n: Int, a: A): A =
      if (n <= 1) a
      else a <> repeat(n - 1, a)

    (("Hello", "Hello"), "Goodbye") <> (("Goodbye", "Hello"), "Hello")

    Map(1 -> "Bla bla") <> Map(2 -> "Ce ce", 1 -> "Ha ha")



    trait Monoid[A] extends SemiGroup[A] {
      // Must satisfy:
      // Left Zero: append(zero, a) == a
      // Right Zero: append(a, zero) == a
      def zero: A
    }

    object Monoid {
      def apply[A](implicit M: Monoid[A]): Monoid[A] = M

      implicit val MonoidInt: Monoid[Int] = new Monoid[Int] {
        override val zero = 0

        override def append(l: Int, r: Int) = l <> r
      }
    }

    def zero[A: Monoid]: A = Monoid[A].zero // implicitly instantiate Monoid, thanks to apply() and implicit vals

    zero[Int]


    // transitive law: eq(a, b) && eq(b, c) => eq(a, c)
    // 1 / 10 = 1 / (2 * 5) = (1 / 2) * (1 / 5)
    // better than equals (that has AnyRef type)
    trait Eq[A] {
      def eq(l: A, r: A): Boolean
    }
    object Eq {
      def apply[A](implicit A: Eq[A]): Eq[A] = A
      implicit val EqInt = new Eq[Int] {
        def eq(l: Int, r: Int): Boolean = l == r
      }
    }
    implicit class EqSyntax[A](l: A) {
      def === (r: A)(implicit A: Eq[A]): Boolean = A.eq(l, r)
    }

    // compiler plugin for typeclasses
    // https://github.com/mpilquist/simulacrum

  }

  object adts {
    // idea: design types so they do not allow for any kind of error

    case class Email private (value: String)
    object Email {
      def apply(value: String): Option[Email] = ???

    }

    // class for representing CORRECT credit card number
    final case class CreditCardNumber private ()
    object CreditCardNumber {
      def apply(number: String): Option[CreditCardNumber] = ???
    }

    sealed trait PaymentMethod
    final case class CreditCard(number: String, code: String, name: String) extends PaymentMethod
    final case object Cash extends PaymentMethod
    final case class Bank(routingNumber:String, swiftCode: String, accountNumber: String) extends PaymentMethod
    final case class Bitcoin(sig: String, address: String) extends PaymentMethod

    CreditCard("", "abaabbabab", "432423423423$#@$@#$@#$#@$")


    // wrong
//    class PaymentMethod {
//      static final int CREDIT_CARD = 1
//      String getCardNumber();
//    }

    final case class Employee(
                               name: String,
                               start: java.time.Instant,
                               position: Position
                             )

    sealed trait Position
    final case object CEO extends Position
    final case object CTO extends Position
    final case class Engineer(level: Int) extends Position

  }

  // https://leanpub.com/fpmortals/read


  object functor {
    // type-class
    // Identity Law    : fmap(identity)(fa) === fa
    //     - How to break it? By adding / removing / reshuffling list items
    //     - We do not change structure by mapping
    // Composition Law : fmap(f)(fmap(g)(fa)) == fmap(f.compose(g))(fa)
    //     - Set is not a Functor
    //

    trait Functor[F[_]] {
      def fmap[A, B](f: A => B): F[A] => F[B]
    }

    object Functor {
      // Functor[List].fmap[Int, String](_.toString)(List(1, 2, 3))
      def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

      implicit val OptionFunctor = new Functor[Option] {
        override def fmap[A, B](f: A => B): Option[A] => Option[B] = _.map(f)
      }
      implicit val ListFunctor = new Functor[List] {
        override def fmap[A, B](f: A => B): List[A] => List[B] = _.map(f)
      }

      // FunctionFunctor is function composition
      implicit def FunctionFunctor[A] = new Functor[Function1[A, ?]] {
        override def fmap[B, C](f: B => C): (A => B) => A => C = x => f.compose(x)
      }
    }

    implicit class FunctorSyntax[F[_], A](fa: F[A]) {
      def fmap[B](f: A => B)(implicit F: Functor[F]): F[B] = F.fmap(f)(fa)
    }

    List(1, 2, 3).fmap(_.toString)

    // nested mapping!
    case class Compose[F[_], G[_], A](fga: F[G[A]])
    // functors compose very well
    case class Product[F[_], G[_], A](l: F[A], r: G[A])
    case class Coproduct[F[_], G[_], A](e: Either[F[A], G[A]])


    trait Apply[F[_]] extends Functor[F] {
      def ap[A, B](f: F[A => B], fa: F[A]): F[B]
      // def zip[A, B](l: F[A], r: F[B]): F[(A, B)]
    }

    // Example for Apply: Option((i: Int) => String).ap(Option(1))

    object Apply {
      implicit val OptionApply = new Apply[Option] {
        override def ap[A, B](f: Option[A => B], fa: Option[A]): Option[B] = (f, fa) match {
          case (Some(f), Some(a)) => Some(f(a))
          case _ => None
        }
        override def fmap[A, B](f: A => B): Option[A] => Option[B] = _.fmap(f)
      }
    }

    trait Applicative[F[_]] extends Apply[F] {
      def point[A](a: A): F[A] // Example: Some(a), List(a), Future.successful(a), no point for Map (Map is just Apply)
    }

    object Applicative {
      implicit val ListApplicative = new Applicative[List] {
        override def point[A](a: A): List[A] = List(a)

        override def ap[A, B](f: List[A => B], fa: List[A]): List[B] = // lossy implementation: f.zip(fa).map { case (f, x) => f(x) }
          for {
            f <- f
            a <- fa
          } yield (f(a)) // cartesian product

        override def fmap[A, B](f: A => B): List[A] => List[B] = _.fmap(f)
      }
    }

    // Dual types - example point A -> F[A], Coapplicative F[A] -> A
    // Identity is Monad and Comonad at once
    final case class Identity[A](run: A)

    trait Monad[F[_]] extends Applicative[F] {
      // programmable semicolon
      // intuition with recipes: fa is a recipe for A, F[B] is a recipe for B, A => F[B] is a function for creating a recipe from A
      //   next recipe depends on value(s) produced by previous recipe
      // flatMap is very hard to optimize (paralellize), for example independent api calls via Future - Future as instance of Apply
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] // flatMap
    }

    object Monad {
      implicit val IdentityMonad = new Monad[Identity] {
        override def bind[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa.run)
        override def point[A](a: A): Identity[A] = Identity(a)
        override def ap[A, B](f: Identity[A => B], fa: Identity[A]): Identity[B] = Identity(f.run(fa.run))
        override def fmap[A, B](f: A => B): Identity[A] => Identity[B] = x => Identity(f(x.run))
      }
    }

    implicit class MonadSyntax[F[_], A](fa: F[A]) {
      def map[B](f: A => B)(implicit F: Monad[F]): F[B] = F.fmap(f)(fa)
      def flatMap[B](f: A => F[B])(implicit F: Monad[F]): F[B] = F.bind(fa)(f)
    }

  }

  object applied {
    import functor._
    //def println(s: String): Unit = Predef.println(s) // pure version = ()
    //def readLine(): String = scala.io.StdIn.readLine() // pure version = ""

    // description of an effectful program that will produce an A
    final case class IO[A](unsafePerformIO: () => A) {
      def map[B](f: A => B): IO[B] = IO(() => f(unsafePerformIO()))
      def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafePerformIO()).unsafePerformIO()) // NOT f(unsafePerformIO())
    }

    object IO {
      def point[A](a: => A): IO[A] = IO(() => a)

      implicit val MonadIO: Monad[IO] = new Monad[IO] {
        override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

        override def point[A](a: A): IO[A] = point(a)

        override def ap[A, B](f: IO[A => B], fa: IO[A]): IO[B] = ???

        override def fmap[A, B](f: A => B): IO[A] => IO[B] = x => x.map(f)
      }
    }

    // deterministic, pure and free of side effects
//    def println(s: String): IO[Unit] = IO(() => Predef.println(s))
//    def readLine(): IO[String] = IO(() => scala.io.StdIn.readLine())

    case class Location()
    case class World(description: String, what: List[Item], options: Map[Location, World])


    sealed trait Item
    case class Food(health: Int, description: String)
    case class Weapon(damage: Int, description: String)

    case class Player(name: String, inventory: List[Item], location: Location)
    case class State(player: Player, world: World)

    sealed trait Command
    case object Exit extends Command
    case object Look extends Command
    case class Pickup(what: String) extends Command

    def parseInput(input: String): Either[String, Command] = {
      val l = input.split(" ").map(_.toLowerCase).toList
      l match {
        case "exit" :: Nil => Right(Exit)
        case "look" :: Nil => Right(Look)
        case "pickup" :: what :: Nil => Right(Pickup(what))
        case _ => Left("Invalid command.")
      }
    }

    def update(command: Command, state: State): (String, Option[State]) = command match {
      case Exit => ("Bye!", None)
      case Look => ("You are standing somwhere, look at something", Some(state))
    }

    def mainLoop[F[_]: Monad: Console](state: State): F[Unit] = {
      val maybeContinue: Command => F[Unit] = { (command: Command) =>
        val (message, next) = update(command, state)
        for {
          _ <- Console[F].println(message)
          _ <- next.fold(implicitly[Monad[F]].point(()))(mainLoop(_))
        } yield ()
      }

      def printContinue(line: String): F[Unit] = {
        for {
          _ <- Console[F].println(line)
          _ <- mainLoop(state)
        } yield ()
      }

      for {
        either <- Console[F].readLine.map(parseInput)
        _ <- either.fold(printContinue, maybeContinue)
      } yield ()
    }


    def program[F[_]: Monad: Console]: F[Unit] =
      for {
        _ <- Console[F].println("What is your name")
        name <- Console[F].readLine
        _ <- Console[F].println(s"Hello $name, it's good to meet you!")
        player = Player(name, Nil, Location())
        state = State(player, World("Room 204 in Wolf Law Building", Nil, Map()))
        _ <- mainLoop(state)
      } yield ()

    trait Console[F[_]] {
      def println(line: String): F[Unit]
      def readLine: F[String]
    }

    object Console {
      def apply[F[_]](implicit F: Console[F]): Console[F] = F
      implicit val ConsoleIO: Console[IO] = new Console[IO] {
        def println(s: String): IO[Unit] = IO(() => Predef.println(s))
        def readLine(): IO[String] = IO(() => scala.io.StdIn.readLine())
      }
    }

    def println[F[_]: Console](line: String): F[Unit] = Console[F].println(line)
    def readLine[F[_]: Console]: F[String] = Console[F].readLine

    case class Buffer(output: List[String], input: List[String])
    case class FakeConsole[A](run: Buffer => (Buffer, A))
    object FakeConsole {
      implicit val ConsoleFakeConsole = new Console[FakeConsole] {
        override def println(line: String): FakeConsole[Unit] = FakeConsole(buffer => (buffer.copy(output = line :: buffer.output), ()))
        override def readLine: FakeConsole[String] = FakeConsole(buffer => buffer.input match {
          case Nil => throw new Error("Broken test!")
          case line :: rest => (buffer.copy(input = rest), line)
        })
      }

      implicit val MonadFakeConsole = new Monad[FakeConsole] {
        override def point[A](a: A): FakeConsole[A] = FakeConsole(buffer => (buffer, a))
        override def ap[A, B](f: FakeConsole[A => B], fa: FakeConsole[A]): FakeConsole[B] = FakeConsole { buffer1 =>
          val (buffer2, ab) = f.run(buffer1)
          val (buffer3, a) = fa.run(buffer2)
          (buffer3, ab(a))
        }
        override def fmap[A, B](f: A => B): FakeConsole[A] => FakeConsole[B] =
          (fa: FakeConsole[A]) => FakeConsole { buffer1 =>
            val (buffer2, a) = fa.run(buffer1)
            (buffer2, f(a))
          }
        override def bind[A, B](fa: FakeConsole[A])(f: A => FakeConsole[B]): FakeConsole[B] = FakeConsole { buffer1 =>
          val (buffer2, a) = fa.run(buffer1)
          f(a).run(buffer2)
        }
      }
    }

    def test: List[String] = program[FakeConsole].run(Buffer(Nil, List("John", "look", "exit")))._1.output
  }

  // finally tagless


  // Optics (Lens)
  // handling nested structures in FP
  // Optic[S, T, A, B] - S src set, T target set, A src item, B dest item
  // Optic[S, A] - S == T, A == B

  // case class Person(name: String)
  // Lens - focus on a term inside a product - Person.name
  // Prism - focusing on a term of sum type
  // Traversal -

  object optics {
    case class Person(name: String, age: Int)

    object Person {
      val _Name = Lens[Person, String](_.name, (name: String) => _.copy(name = name))
      val _Age  = Lens[Person, Int](_.age, (age: Int) => _.copy(age = age))
    }

    case class Car(driver: Person)
    object Car {
      val _Person = Lens[Car, Person](_.driver, person => _.copy(driver = person))
    }

    case class Lens[S, A](
                         get: S => A,
                         set: A => S => S
                         ) {
      def >>>[B](that: Lens[A, B]): Lens[S, B] = Lens[S, B](
        get = (s: S) => (that.get(get(s)) : B),
        set = (b: B) => (s: S) => set(that.set(b)(get(s)))(s)
      )

      def modify(f: A => A): S => S =
        (s: S) => set(f(get(s)))(s)
    }

    val TestCar = Car(Person("Name", 10))

    import Car._
    import Person._

    val lens = (_Person >>> _Age): Lens[Car, Int]

    lens.modify(_ + 10)(TestCar)
  }


}

