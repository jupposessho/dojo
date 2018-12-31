package waff.dojo

final case class Bowling(rolls: Vector[Int]) {

  val numberOfFrames = 10

  def roll(pins: Int): Bowling = Bowling(rolls :+ pins)

  def score: Int = {
    def acc(actScore: Int, frameIndex: Int, remainingSteps: Int): Int = {
      if (remainingSteps == 0) actScore
      else if (strike(frameIndex)) acc(actScore + 10 + strikeBonus(frameIndex), frameIndex + 1, remainingSteps - 1)
      else if (spare(frameIndex)) acc(actScore + 10 + spareBonus(frameIndex), frameIndex + 2, remainingSteps - 1)
      else acc(actScore + frameScore(frameIndex), frameIndex + 2, remainingSteps - 1)
    }

    acc(0, 0, 10)
  }

  private def frameScore(frameIndex: Int) = {
    rolls(frameIndex) + rolls(frameIndex + 1)
  }

  private def spareBonus(frameIndex: Int) = {
    rolls(frameIndex + 2)
  }

  private def strikeBonus(frameIndex: Int) = {
    rolls(frameIndex + 2) + rolls(frameIndex + 1)
  }

  private def spare(frameIndex: Int) = {
    frameScore(frameIndex) == 10
  }

  private def strike(frameIndex: Int) = {
    rolls(frameIndex) == 10
  }
}
