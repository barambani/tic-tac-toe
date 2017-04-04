import Algebra._
import scala.language.higherKinds

sealed trait Previous[S <: Status, M <: Moves] { 
  type NewS
  type NewM

  val s: NewS
  val m: NewM
}

object Previous {

  type Aux[S <: Status, M <: Moves , S1, M1] = Previous[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Moves](implicit INST: Previous[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST

  implicit lazy val previous1: Previous[InPlay, OneMove] = 
    new Previous[InPlay, OneMove] {
      type NewS = NotStarted
      type NewM = NoMoves
      
      val s = NotStarted
      val m = NoMoves()
    }

  implicit lazy val previous2: Previous[InPlay, TwoMoves] = 
    new Previous[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = OneMove
      
      val s = InPlay
      val m = OneMove()
    }
 
  implicit lazy val previous3: Previous[InPlay, ThreeMoves] = 
    new Previous[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = TwoMoves
      
      val s = InPlay
      val m = TwoMoves()
    }

  implicit lazy val previous4: Previous[InPlay, FourMoves] = 
    new Previous[InPlay, FourMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves
      
      val s = InPlay
      val m = ThreeMoves()
    }

  implicit lazy val previous5: Previous[MayBeFinished, FiveMoves] = 
    new Previous[MayBeFinished, FiveMoves] {
      type NewS = InPlay
      type NewM = FourMoves
      
      val s = InPlay
      val m = FourMoves()
    }

  implicit lazy val previous6: Previous[MayBeFinished, SixMoves] = 
    new Previous[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves
      
      val s = MayBeFinished
      val m = FiveMoves()
    }

  implicit lazy val previous7: Previous[MayBeFinished, SevenMoves] = 
    new Previous[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves
      
      val s = MayBeFinished
      val m = SixMoves()
    }

  implicit lazy val previous8: Previous[MayBeFinished, EightMoves] = 
    new Previous[MayBeFinished, EightMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves
      
      val s = MayBeFinished
      val m = SevenMoves()
    }

  implicit lazy val previous9: Previous[Finished, Full] = 
    new Previous[Finished, Full] {
      type NewS = MayBeFinished
      type NewM = EightMoves
      
      val s = MayBeFinished
      val m = EightMoves()
    }
}
