import Algebra._

sealed trait Previous[S, S1] {
  def getPrevious: S1
}

object Previous{

  def apply[S, S1](implicit INST: Previous[S, S1]): Previous[S, S1] = INST

  implicit lazy val previous1: Previous[OneMove, NoMoves] = new Previous[OneMove, NoMoves] {
    def getPrevious = NoMoves()
  }

  implicit lazy val previous2: Previous[TwoMoves, OneMove] = new Previous[TwoMoves, OneMove] {
    def getPrevious = OneMove()
  }

  implicit lazy val previous3: Previous[ThreeMoves, TwoMoves] = new Previous[ThreeMoves, TwoMoves] {
    def getPrevious = TwoMoves()
  }

  implicit lazy val previous4: Previous[FourMoves, ThreeMoves] = new Previous[FourMoves, ThreeMoves] {
    def getPrevious = ThreeMoves()
  }

  implicit lazy val previous5: Previous[FiveMoves, FourMoves] = new Previous[FiveMoves, FourMoves] {
    def getPrevious = FourMoves()
  }

  implicit lazy val previous6: Previous[SixMoves, FiveMoves] = new Previous[SixMoves, FiveMoves] {
    def getPrevious = FiveMoves()
  }

  implicit lazy val previous7: Previous[SevenMoves, SixMoves] = new Previous[SevenMoves, SixMoves] {
    def getPrevious = SixMoves()
  }

  implicit lazy val previous8: Previous[EightMoves, SevenMoves] = new Previous[EightMoves, SevenMoves] {
    def getPrevious = SevenMoves()
  }

  implicit lazy val previous9: Previous[Full, EightMoves] = new Previous[Full, EightMoves] {
    def getPrevious = EightMoves()
  }
}
