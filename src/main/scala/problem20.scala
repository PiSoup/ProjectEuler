import Math._

/**
  * Created by lilyd on 5/9/2017.
  */
object Problem20{
  def main(args: Array[String]): Unit = {
    val bigFactorial = factorial(100)
    println(s"Factorial=$bigFactorial")

    val digitSum = bigFactorial.toString.toCharArray.foldLeft(0)((acc: Int, digit: Char) => acc + digit.asDigit)
    println(s"Sum of digits=$digitSum")
  }

}