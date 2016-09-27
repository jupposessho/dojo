package com.emarsys.dojo

import akka.actor.{FSM, Props}
import Bowling._

class Bowling extends FSM[State, Data] {

  startWith(Rolling, Rolls(Nil))

  when(Rolling) {

    case Event(Roll(r), Rolls(rolls)) =>
      goto(Rolling) using Rolls(addRoll(r, rolls))

    case Event(Score, Rolls(l)) =>
      sender ! ActualScore(score(l))
      stay()
  }

  initialize()

  def score(l: List[Frame]): Int = {
    l.reverse.take(10).map(f => f.score).sum
  }

  def addRoll(r: Int, l: List[Frame]): List[Frame] = l match {

    case Nil =>
      Frame.create(r) :: Nil
    
    case Strike :: StrikeWithBonus(b) :: xs =>
      Frame.create(r) :: StrikeWithBonus(r) :: StrikeWithBonus(b + r) :: xs

    case Strike :: xs =>
      Frame.create(r) :: StrikeWithBonus(r) :: xs

    case InComplete(h) :: StrikeWithBonus(b) :: xs =>
      Complete(h + r) :: StrikeWithBonus(b + r) :: xs

    case InComplete(h) :: xs =>
      if (h + r == 10) Spare :: xs
      else Complete(h + r) :: xs

    case Complete(h) :: xs =>
      Frame.create(r) :: Complete(h) :: xs

    case Spare :: xs =>
      InComplete(r) :: SpearWithBonus(r) :: xs

    case _ => l
  }
}

object Bowling {

  sealed trait State
  case object Idle extends State
  case object Rolling extends State
  case object Finished extends State

  sealed trait Data
  case class Rolls(rolls: List[Frame]) extends Data

  case class Roll(hit: Int)
  case object Score
  case class ActualScore(score: Int)

  sealed trait Frame {
    def score: Int
  }
  case class InComplete(hit: Int) extends Frame {
    override def score: Int = hit
  }
  case class Complete(hit: Int) extends Frame {
      override def score: Int = hit
    }
  case class SpearWithBonus(bonus: Int) extends Frame {
      override def score: Int = 10 + bonus
    }
  case object Spare extends Frame {
      override def score: Int = 10
    }
  case object Strike extends Frame {
      override def score: Int = 10
    }
  case class StrikeWithBonus(bonus: Int) extends Frame {
      override def score: Int = 10 + bonus
    }

  object Frame {
    def create(r: Int) = {
      if (r == 10) Strike
      else InComplete(r)
    }
  }

  def props() = Props(new Bowling())
}
