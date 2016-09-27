package com.emarsys.dojo

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestFSMRef, TestKit, TestProbe}
import akka.util.Timeout
import com.emarsys.dojo.Bowling._
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._

class BowlingSpec
    extends TestKit(ActorSystem("BowlingGameSpec"))
    with ImplicitSender
    with Matchers
    with WordSpecLike
    with BeforeAndAfter
    with ScalaFutures {

  //  override def after = TestKit.shutdownActorSystem(system)
  implicit val timeout = Timeout(5 seconds)

  "A bowling game" when {

    "starting a game" should {
      "be Idle" in {
        val bowlingGame = createGame
        bowlingGame.stateName shouldEqual Rolling
        bowlingGame.stateData shouldEqual Rolls(Nil)
      }
      "return 0" in {
        val bowlingGame = createGame

        bowlingGame ! Roll(0)
        (bowlingGame ? Score).futureValue shouldEqual ActualScore(0)
      }
    }

    "some open frame rolled" should {
      "be the sum of the rolls" in {
        val bowlingGame = createGame

        rollMany(4, 1, bowlingGame)
        bowlingGame.stateName shouldEqual Rolling
        (bowlingGame ? Score).futureValue shouldEqual ActualScore(4)
      }
    }

    "one spare in the house" should {
      "the next roll added to the score of the last frame" in {
        val bowlingGame = createGame

        bowlingGame ! Roll(6)
        bowlingGame ! Roll(4)
        bowlingGame ! Roll(3)
        rollMany(17, 0, bowlingGame)
        bowlingGame.stateName shouldEqual Rolling
        (bowlingGame ? Score).futureValue shouldEqual ActualScore(16)
      }

      "the bonus only applies for in a frame" in {
        val bowlingGame = createGame

        bowlingGame ! Roll(3)
        bowlingGame ! Roll(6)
        bowlingGame ! Roll(4)
        rollMany(17, 0, bowlingGame)
        bowlingGame.stateName shouldEqual Rolling
        (bowlingGame ? Score).futureValue shouldEqual ActualScore(13)
      }
    }

    "one strike in the house" should {
      "the next two roll added to the score of the last frame" in {
        val bowlingGame = createGame

        bowlingGame ! Roll(10)
        bowlingGame ! Roll(4)
        bowlingGame ! Roll(3)
        rollMany(16, 0, bowlingGame)
        bowlingGame.stateName shouldEqual Rolling
        (bowlingGame ? Score).futureValue shouldEqual ActualScore(24)
      }
    }

    "all strike" should {
      "maximum score" in {
        val bowlingGame = createGame

        rollMany(12, 10, bowlingGame)
        bowlingGame.stateName shouldEqual Rolling
        (bowlingGame ? Score).futureValue shouldEqual ActualScore(300)
      }
    }
  }

  def createGame = {
    val p = TestProbe()
    val r = TestFSMRef(new Bowling, p.ref, java.util.UUID.randomUUID.toString)
    r
  }

  def rollMany(n: Int, p: Int, game: ActorRef) = {
    List.fill(n)(p).foreach(game ! Roll(_))
  }
}
