import org.scalatest._
import problem54._

class Problem54Test extends FlatSpec with Matchers {
  "Card parser" should "parse face values" in {
    assert(Card("1H") == Card(1, Heart))
    assert(Card("TH") == Card(10, Heart))
    assert(Card("JH") == Card(11, Heart))
    assert(Card("QH") == Card(12, Heart))
    assert(Card("KH") == Card(13, Heart))
    assert(Card("AH") == Card(14, Heart))
  }
  it should "parse suits" in {
    assert(Card("1H") == Card(1, Heart))
    assert(Card("1S") == Card(1, Spade))
    assert(Card("1C") == Card(1, Club))
    assert(Card("1D") == Card(1, Diamond))
  }

  "Hand parser" should "parse OnePair" in {
    assert(cardsToHand(Seq("5H", "5C", "6S", "7S", "KD").map(Card(_))) == OnePair(5, Seq(13,7,6)))
  }
}