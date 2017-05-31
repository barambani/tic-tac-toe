import Algebra._

sealed trait Previous[S <: Status, M <: Move] { 
  type NewS <: Status
  type NewM <: Move
}

object Previous {

  type Aux[S <: Status, M <: Move , S1, M1] = Previous[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Move](implicit INST: Previous[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST

  implicit lazy val previous1: Aux[InPlay, OneMove, NotStarted, NoMoves] = 
    new Previous[InPlay, OneMove] {
      type NewS = NotStarted
      type NewM = NoMoves
    }

  implicit lazy val previous2: Aux[InPlay, TwoMoves, InPlay, OneMove] = 
    new Previous[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = OneMove
    }
 
  implicit lazy val previous3: Aux[InPlay, ThreeMoves, InPlay, TwoMoves] = 
    new Previous[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = TwoMoves
    }

  implicit lazy val previous4: Aux[InPlay, FourMoves, InPlay, ThreeMoves] = 
    new Previous[InPlay, FourMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves
    }

  implicit lazy val previous5: Aux[MayBeFinished, FiveMoves, InPlay, FourMoves] = 
    new Previous[MayBeFinished, FiveMoves] {
      type NewS = InPlay
      type NewM = FourMoves
    }

  implicit lazy val previous6: Aux[MayBeFinished, SixMoves, MayBeFinished, FiveMoves] = 
    new Previous[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves
    }

  implicit lazy val previous7: Aux[MayBeFinished, SevenMoves, MayBeFinished, SixMoves] = 
    new Previous[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves
    }

  implicit lazy val previous8: Aux[MayBeFinished, EightMoves, MayBeFinished, SevenMoves] = 
    new Previous[MayBeFinished, EightMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves
    }

  implicit lazy val previous9: Aux[Finished, Full, MayBeFinished, EightMoves] = 
    new Previous[Finished, Full] {
      type NewS = MayBeFinished
      type NewM = EightMoves
    }
}
