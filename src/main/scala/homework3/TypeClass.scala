package homework3

/*
В scala есть такая особенность, что оператор сравнения позволяет сравнивать разные типы.

Например вы можете написать такой код:
Option(1) == "1"
И максимум, что мы получим - это предупреждение от компилятора.
Вам нужно написать свой type class под название Eq[A], для которого будет определена операция `def eq(a1: A, a2: A): Boolean`
А также синтаксис, который позволит писать так:
Option(1) === Option(1)
"str" !== "str1"
true == false
И при этом не давать компилироваться следующему коду:

Option(1) === Option("1")
"str" !== false

Должны быть определены экземпляры для вывода:
Option[A], Map[String, A], Seq[A]
Сравнение значений в контейнерах должны осуществляться через экземпляр класса типов Eq[A]
А также дефолтный Eq для любого типа, реализованный через оператор `==`
*/
trait Eq[A] {
  def eq(a1: A, a2: A): Boolean
}

object Eq {

  implicit def defaultEq[A]: Eq[A] = new Eq[A] {
    override def eq(a1: A, a2: A): Boolean = if (a1 == a2) true else false
  }

  implicit def optionEq[A](implicit s: Eq[A]): Eq[Option[A]] = new Eq[Option[A]] {
    override def eq(a1: Option[A], a2: Option[A]): Boolean =
      (a1, a2) match {
        case (Some(x), Some(y)) => s.eq(x, y)
        case (None, None) => true
        case _ => false
      }
  }

  implicit def seqEq[A](implicit s: Eq[A]): Eq[Seq[A]] = new Eq[Seq[A]] {
    override def eq(a1: Seq[A], a2: Seq[A]): Boolean =
      (a1, a2) match {
        case (x, y) if (x.size == y.size) => !x.zip(y).exists(z => !s.eq(z._1, z._2))
        case _ => false
      }
  }

  implicit def mapEq[A](implicit s: Eq[Seq[A]], b: Eq[Seq[String]]): Eq[Map[String, A]] = new Eq[Map[String, A]] {
    override def eq(a1: Map[String, A], a2: Map[String, A]): Boolean =
      (a1, a2) match {
        case (x, y) => b.eq(x.keys.toSeq, y.keys.toSeq) && s.eq(x.values.toSeq, y.values.toSeq)
        case _ => false
      }
  }

  object Syntax {

    implicit class EqOpt[A](val a1: Option[A]) {
      def ===(a2: Option[A])(implicit s: Eq[Option[A]]): Boolean = s.eq(a1, a2)
    }

    implicit class EqAny[A](val a1: A) {
      def !==(a2: A)(implicit s: Eq[A]): Boolean = !s.eq(a1, a2)
    }

    implicit class EqSeq[A](val a1: Seq[A]) {
      def ===(a2: Seq[A])(implicit s: Eq[Seq[A]]): Boolean = s.eq(a1, a2)
    }

    implicit class EqMap[A](val a1: Map[String, A]) {
      def ===(a2: Map[String, A])(implicit s: Eq[Map[String, A]]): Boolean = s.eq(a1, a2)
    }

  }

}

object TypeClass extends App {

  import Eq.Syntax._

  println(Option(4) === Option(2))
  println("str" !== "str1")
  println(Seq(1, 2, 3) === Seq(1, 2, 3))
  println(Seq(1, 2, 3) === Seq(1, 4, 3))
  println(Map("1" -> 1, "2" -> 2) === Map("1" -> 1, "2" -> 2))
  println(Map("1" -> 1, "3" -> 3) === Map("1" -> 1, "2" -> 2))
  println(Seq[Any]() === Seq[Any]())
  println(Map[String, Any]() === Map[String, Any]())
  //println(Option(4) === Option("2"))
}