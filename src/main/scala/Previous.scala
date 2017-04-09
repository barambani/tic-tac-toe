import Algebra._
import scala.language.higherKinds

sealed trait Previous[S <: Status, M <: Moves] { 
  type NewS <: Status
  type NewM <: Moves

  val s: NewS
  val m: NewM
}

object Previous {

  type Aux[S <: Status, M <: Moves , S1, M1] = Previous[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Moves](implicit INST: Previous[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST

  implicit lazy val previous1: Aux[InPlay, OneMove, NotStarted, NoMoves] = 
    new Previous[InPlay, OneMove] {
      type NewS = NotStarted
      type NewM = NoMoves
      
      val s = NotStarted
      val m = NoMoves
    }

  implicit lazy val previous2: Aux[InPlay, TwoMoves, InPlay, OneMove] = 
    new Previous[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = OneMove
      
      val s = InPlay
      val m = OneMove
    }
 
  implicit lazy val previous3: Aux[InPlay, ThreeMoves, InPlay, TwoMoves] = 
    new Previous[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = TwoMoves
      
      val s = InPlay
      val m = TwoMoves
    }

  implicit lazy val previous4: Aux[InPlay, FourMoves, InPlay, ThreeMoves] = 
    new Previous[InPlay, FourMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves
      
      val s = InPlay
      val m = ThreeMoves
    }

  implicit lazy val previous5: Aux[MayBeFinished, FiveMoves, InPlay, FourMoves] = 
    new Previous[MayBeFinished, FiveMoves] {
      type NewS = InPlay
      type NewM = FourMoves
      
      val s = InPlay
      val m = FourMoves
    }

  implicit lazy val previous6: Aux[MayBeFinished, SixMoves, MayBeFinished, FiveMoves] = 
    new Previous[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves
      
      val s = MayBeFinished
      val m = FiveMoves
    }

  implicit lazy val previous7: Aux[MayBeFinished, SevenMoves, MayBeFinished, SixMoves] = 
    new Previous[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves
      
      val s = MayBeFinished
      val m = SixMoves
    }

  implicit lazy val previous8: Aux[MayBeFinished, EightMoves, MayBeFinished, SevenMoves] = 
    new Previous[MayBeFinished, EightMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves
      
      val s = MayBeFinished
      val m = SevenMoves
    }

  implicit lazy val previous9: Aux[Finished, Full, MayBeFinished, EightMoves] = 
    new Previous[Finished, Full] {
      type NewS = MayBeFinished
      type NewM = EightMoves
      
      val s = MayBeFinished
      val m = EightMoves
    }
}
