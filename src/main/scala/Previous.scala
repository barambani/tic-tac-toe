import Algebra._
import scala.language.higherKinds

sealed trait Previous[S[_], S1[_], M, M1] {
  def getPrevious: S1[M1]
}

object Previous{

  def apply[S[_], S1[_], M, M1](implicit INST: Previous[S, S1, M, M1]): Previous[S, S1, M, M1] = INST

  implicit lazy val previous1: Previous[InPlay, NotStarted, OneMove, NoMoves] = new Previous[InPlay, NotStarted, OneMove, NoMoves] {
    def getPrevious = NotStarted(NoMoves())
  }

  implicit lazy val previous2: Previous[InPlay, InPlay, TwoMoves, OneMove] = new Previous[InPlay, InPlay, TwoMoves, OneMove] {
    def getPrevious = InPlay(OneMove())
  }

  implicit lazy val previous3: Previous[InPlay, InPlay, ThreeMoves, TwoMoves] = new Previous[InPlay, InPlay, ThreeMoves, TwoMoves] {
    def getPrevious = InPlay(TwoMoves())
  }

  implicit lazy val previous4: Previous[InPlay, InPlay, FourMoves, ThreeMoves] = new Previous[InPlay, InPlay, FourMoves, ThreeMoves] {
    def getPrevious = InPlay(ThreeMoves())
  }

  implicit lazy val previous5: Previous[MayBeFinished, InPlay, FiveMoves, FourMoves] = new Previous[MayBeFinished, InPlay, FiveMoves, FourMoves] {
    def getPrevious = InPlay(FourMoves())
  }

  implicit lazy val previous6: Previous[MayBeFinished, MayBeFinished, SixMoves, FiveMoves] = new Previous[MayBeFinished, MayBeFinished, SixMoves, FiveMoves] {
    def getPrevious = MayBeFinished(FiveMoves())
  }

  implicit lazy val previous7: Previous[MayBeFinished, MayBeFinished, SevenMoves, SixMoves] = new Previous[MayBeFinished, MayBeFinished, SevenMoves, SixMoves] {
    def getPrevious = MayBeFinished(SixMoves())
  }

  implicit lazy val previous8: Previous[MayBeFinished, MayBeFinished, EightMoves, SevenMoves] = new Previous[MayBeFinished, MayBeFinished, EightMoves, SevenMoves] {
    def getPrevious = MayBeFinished(SevenMoves())
  }

  implicit lazy val previous9: Previous[Finished, MayBeFinished, Full, EightMoves] = new Previous[Finished, MayBeFinished, Full, EightMoves] {
    def getPrevious = MayBeFinished(EightMoves())
  }
}
