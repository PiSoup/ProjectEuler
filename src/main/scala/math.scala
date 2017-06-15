import scala.annotation.tailrec

object Math {
  @tailrec
  def factorial(value: BigInt, currentValue: BigInt = BigInt(1)): BigInt = {
    value match {
      case n if n == BigInt(1) => currentValue //BigInt doesn't get along with pattern matching
      case n => factorial(n - 1, n * currentValue)
    }
  }
}
