import scala.util.Try

/**
  * Created by lilyd on 6/15/2017.
  */
object problem54 {
  def main(args: Array[String]): Unit = {
  }

  sealed trait Suit
  case object Diamond extends Suit
  case object Club extends Suit
  case object Spade extends Suit
  case object Heart extends Suit
  implicit def stringToSuit(text: String): Suit = text.toCharArray.last match {
    case 'D' => Diamond
    case 'C' => Club
    case 'S' => Spade
    case 'H' => Heart
  }

  case class Card(value: Int, suit: Suit)
  object Card {
    def apply(text: String): Card = {
      val reg = "(\\w)(\\w)".r
      text match{
        case reg(value, suit) =>
          val parsedValue = value match {
            case "T" => 10
            case "J" => 11
            case "Q" => 12
            case "K" => 13
            case "A" => 14
            case n if Try(n.toInt).isSuccess => n.toInt
            case n => throw new Exception("Invalid value: " + n)
          }
          Card(parsedValue, suit)
        case n => throw new Exception("File not as well formatted as claimed: " + n)
      }
    }
  }

  sealed trait Hand
  case class HighCard(cards: Seq[Int]) extends Hand
  case class OnePair(pairValue: Int, rest: Seq[Int]) extends Hand
  case class TwoPair(highPair: Int, lowPair: Int, rest: Int) extends Hand
  case class ThreeOfAKind(value: Int, rest: Seq[Int]) extends Hand
  case class Straight(highCard: Int) extends Hand
  case class Flush(cards: Seq[Int]) extends Hand
  case class FullHouse(highSet: Int, lowSet: Int) extends Hand
  case class FourOfAKind(highSet: Int, lowSet: Int) extends Hand
  case class StraightFlush(highValue: Int) extends Hand
  implicit def cardsToHand(cards: Seq[Card]): Hand = {
    def isFlush = cards.forall(_.suit == cards.head.suit)
    def isStraight = cards.sortWith{_.value < _.value}.sliding(2).foldLeft(true){case (acc, Seq(Card(v1, _), Card(v2, _))) => acc & ((v2 - v1) == 1)}
    lazy val groupedCards = cards.map(_.value).groupBy(identity).mapValues(_.length).toSeq.sortWith(_._2 > _._2)  //This is of type [value -> count] sorted descending by count
    cards.sortWith{_.value > _.value} match {
      case n if isFlush && isStraight => StraightFlush(n.head.value)
      case n if groupedCards.map{ case (_, v) => v}.max == 4 => FourOfAKind(groupedCards.head._1, groupedCards.last._1)
      case n if groupedCards(0)._2 == 3 && groupedCards(1)._2 == 2 => FullHouse(groupedCards(0)._1, groupedCards(1)._1)
      case n if isFlush => Flush(n.map(_.value))
      case n if isStraight => Straight(n.head.value)
      case n if groupedCards(0)._2 == 3 => ThreeOfAKind(groupedCards.head._1, groupedCards.tail.map(_._1))
      case n if groupedCards(0)._2 == 2 && groupedCards(1)._2 == 2 => TwoPair(groupedCards(0)._1, groupedCards(1)._1, groupedCards(2)._1)
      case n if groupedCards(0)._2 == 2 => OnePair(groupedCards.head._1, groupedCards.tail.map(_._1))
      case n => HighCard(n.map(_.value))
    }
  }
}

