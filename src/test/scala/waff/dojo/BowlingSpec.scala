package waff.dojo

import org.scalatest.{ Matchers, WordSpecLike }

class BowlingSpec extends WordSpecLike with Matchers {

  val game = Bowling(Vector.empty[Int])

  "bowling game" should {
    "return with the score of the game" when {

      "all 0" in {
        game
          .rollMany(20, 0)
          .score shouldBe 0
      }

      "all 1" in {
        game
          .rollMany(20, 1)
          .score shouldBe 20
      }

      "spare" in {
        game.rollSpare
          .roll(3)
          .rollMany(17, 0)
          .score shouldBe 16
      }

      "strike" in {
        game.rollStrike
          .roll(3)
          .roll(4)
          .rollMany(16, 0)
          .score shouldBe 24
      }

      "perfect" in {
        game.rollStrike
          .rollMany(12, 10)
          .score shouldBe 300
      }
    }
  }

  implicit class BowlingOps(game: Bowling) {
    def rollMany(limit: Int, pins: Int): Bowling = {
      (1 to limit).foldLeft(game)((g, _) => g roll pins)
    }

    def rollSpare = game.roll(5).roll(5)

    def rollStrike = game.roll(10)
  }
}
