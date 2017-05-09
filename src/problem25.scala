import Math._

/**
  * Created by lilyd on 5/9/2017.
  */
object problem25 {
  def main(args: Array[String]): Unit = {
    val fib = new FibonacciIterator()

    // Need to offset the fold start point to make up for the missing initial pair
    println(s"Fib: ${fib.takeWhile(_.toString.length < 1000).foldLeft(3)((acc: Int, _: BigInt)=> acc + 1)}")
  }
}

class FibonacciIterator extends Iterator[BigInt]{
  private var currentPair = (BigInt(1), BigInt(1))

  override def hasNext: Boolean = true //There's always another fibonacci number
  override def next(): BigInt = {
    val nextFib = currentPair._1 + currentPair._2
    currentPair = (currentPair._2, nextFib)
    nextFib
  }
}