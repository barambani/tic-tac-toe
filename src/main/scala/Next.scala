import Algebra._
import scalaz.{\/, -\/, \/-}

sealed trait Next[S <: Status, M <: Move] {
  type NewS <: Status
  type NewM <: Move
}

object Next {

  type Aux[S <: Status, M <: Move, S1, M1] = Next[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Move](implicit INST: Next[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST
  
  implicit lazy val noMoves: Aux[NotStarted, NoMoves, InPlay, OneMove] = 
    new Next[NotStarted, NoMoves] {
      type NewS = InPlay
      type NewM = OneMove
    }

  implicit lazy val oneMove: Aux[InPlay, OneMove, InPlay, TwoMoves] = 
    new Next[InPlay, OneMove] {
      type NewS = InPlay
      type NewM = TwoMoves
    }

  implicit lazy val twoMoves: Aux[InPlay, TwoMoves, InPlay, ThreeMoves] =
    new Next[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves
    }

  implicit lazy val threeMoves: Aux[InPlay, ThreeMoves, InPlay, FourMoves] = 
    new Next[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = FourMoves
    }

  implicit lazy val fourMoves: Aux[InPlay, FourMoves, MayBeFinished, FiveMoves] = 
    new Next[InPlay, FourMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves
    }

  implicit lazy val fiveMoves: Aux[MayBeFinished, FiveMoves, MayBeFinished, SixMoves] = 
    new Next[MayBeFinished, FiveMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves
    }

  implicit lazy val sixMoves: Aux[MayBeFinished, SixMoves, MayBeFinished, SevenMoves] = 
    new Next[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves
    }

  implicit lazy val sevenMoves: Aux[MayBeFinished, SevenMoves, MayBeFinished, EightMoves] = 
    new Next[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = EightMoves
    }

  implicit lazy val eightMoves: Aux[MayBeFinished, EightMoves, Finished, Full] = 
    new Next[MayBeFinished, EightMoves] {
      type NewS = Finished
      type NewM = Full
    }
}
