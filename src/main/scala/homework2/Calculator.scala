package homework2

/**
  * В этом домашнем задании вам нужно реализовать калькулятор.
  * Что он должен делать:
  * функция computeValues принимает Map, ключами являются имена переменных, а значением некоторое выражение.
  * На выходе функция возвращает Map, в которой все выражения вычислены.
  * Функция eval вычисляет одно заданное выражение, а getReferenceExpr возвращает значение переменной по имени.
  * Функция eval возвращает Double.NaN в случаях:
  * - переменная не была задана
  * - циклическая ссылка, например: a = c - b; b = a - 2
  */

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends App {

  def computeValues(namedExpressions: Map[String, Expr]): Map[String, Double] = {
    namedExpressions.map { case (key, value) => (key, eval(value, namedExpressions)) }
  }

  def eval(expr: Expr, references: Map[String, Expr]): Double = {

    def evalWithMem(expr: Expr, references: Map[String, Expr], initValue: Expr): Double = {
      expr match {
        case Literal(x) => x
        case Plus(x, y) => evalWithMem(x, references, initValue) + evalWithMem(y, references, initValue)
        case Ref(x) =>
          if (getReferenceExpr(x, references) == initValue) Double.NaN
          else evalWithMem(getReferenceExpr(x, references), references, initValue)
        case Minus(x, y) => evalWithMem(x, references, initValue) - evalWithMem(y, references, initValue)
        case Times(x, y) => evalWithMem(x, references, initValue) * evalWithMem(y, references, initValue)
        case Divide(x, y) =>
          if (evalWithMem(y, references, initValue) == 0) throw new IllegalArgumentException("divided by zero")
          else evalWithMem(x, references, initValue) / evalWithMem(y, references, initValue)
      }
    }
    evalWithMem(expr, references, expr)
  }

  // Данная функция должна вернуть значение переменной по имени
  def getReferenceExpr(name: String, references: Map[String, Expr]): Expr = references(name)
}