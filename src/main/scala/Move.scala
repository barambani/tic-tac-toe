import Algebra._
import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

sealed trait Move[S <: Status, M <: Moves] {
  type NewS <: Status
  type NewM <: Moves

  val s: NewS
  val m: NewM
}

object Move {

  type Aux[S <: Status, M <: Moves, S1, M1] = Move[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Moves](implicit INST: Move[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST
  
  implicit lazy val move0: Aux[NotStarted, NoMoves, InPlay, OneMove] = 
    new Move[NotStarted, NoMoves] {
      type NewS = InPlay
      type NewM = OneMove

      val s = InPlay
      val m = OneMove
    }

  implicit lazy val move1: Aux[InPlay, OneMove, InPlay, TwoMoves] = 
    new Move[InPlay, OneMove] {
      type NewS = InPlay
      type NewM = TwoMoves

      val s = InPlay
      val m = TwoMoves
    }

  implicit lazy val move2: Aux[InPlay, TwoMoves, InPlay, ThreeMoves] =
    new Move[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves

      val s = InPlay
      val m = ThreeMoves
    }

  implicit lazy val move3: Aux[InPlay, ThreeMoves, InPlay, FourMoves] = 
    new Move[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = FourMoves

      val s = InPlay
      val m = FourMoves
    }

  implicit lazy val move4: Aux[InPlay, FourMoves, MayBeFinished, FiveMoves] = 
    new Move[InPlay, FourMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves

      val s = MayBeFinished
      val m = FiveMoves
    }

  implicit lazy val move5: Aux[MayBeFinished, FiveMoves, MayBeFinished, SixMoves] = 
    new Move[MayBeFinished, FiveMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves

      val s = MayBeFinished
      val m = SixMoves
    }

  implicit lazy val move6: Aux[MayBeFinished, SixMoves, MayBeFinished, SevenMoves] = 
    new Move[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves

      val s = MayBeFinished
      val m = SevenMoves
    }

  implicit lazy val move7: Aux[MayBeFinished, SevenMoves, MayBeFinished, EightMoves] = 
    new Move[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = EightMoves

      val s = MayBeFinished
      val m = EightMoves
    }

  implicit lazy val movea8: Aux[MayBeFinished, EightMoves, Finished, Full] = 
    new Move[MayBeFinished, EightMoves] {
      type NewS = Finished
      type NewM = Full

      val s = Finished
      val m = Full
    }
}
