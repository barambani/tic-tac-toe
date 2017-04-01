import Algebra._
import scala.language.higherKinds

sealed trait TakeBack[S <: Status, M <: Moves] { 
  type NewS
  type NewM

  def takeBack(b: Board[S, M]): Board[NewS, NewM]
}

object TakeBack {

  type Aux[S <: Status, M <: Moves , S1, M1] = TakeBack[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Moves](implicit INST: TakeBack[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST

  implicit lazy val takeBack1: TakeBack[InPlay, OneMove] = 
    new TakeBack[InPlay, OneMove] {
      type NewS = NotStarted
      type NewM = NoMoves
      
      def takeBack(b: Board[InPlay, OneMove]): Board[NewS, NewM] =
        Board.createPrev(b)(NotStarted(), NoMoves())
    }

  implicit lazy val takeBack2: TakeBack[InPlay, TwoMoves] = 
    new TakeBack[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = OneMove
      
      def takeBack(b: Board[InPlay, TwoMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(InPlay(), OneMove())
    }
 
  implicit lazy val takeBack3: TakeBack[InPlay, ThreeMoves] = 
    new TakeBack[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = TwoMoves
      
      def takeBack(b: Board[InPlay, ThreeMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(InPlay(), TwoMoves())
    }

  implicit lazy val takeBack4: TakeBack[InPlay, FourMoves] = 
    new TakeBack[InPlay, FourMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves
      
      def takeBack(b: Board[InPlay, FourMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(InPlay(), ThreeMoves())
    }

  implicit lazy val takeBack5: TakeBack[MayBeFinished, FiveMoves] = 
    new TakeBack[MayBeFinished, FiveMoves] {
      type NewS = InPlay
      type NewM = FourMoves
      
      def takeBack(b: Board[MayBeFinished, FiveMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(InPlay(), FourMoves())
    }

  implicit lazy val takeBack6: TakeBack[MayBeFinished, SixMoves] = 
    new TakeBack[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves
      
      def takeBack(b: Board[MayBeFinished, SixMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(MayBeFinished(), FiveMoves())
    }

  implicit lazy val takeBack7: TakeBack[MayBeFinished, SevenMoves] = 
    new TakeBack[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves
      
      def takeBack(b: Board[MayBeFinished, SevenMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(MayBeFinished(), SixMoves())
    }

  implicit lazy val takeBack8: TakeBack[MayBeFinished, EightMoves] = 
    new TakeBack[MayBeFinished, EightMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves
      
      def takeBack(b: Board[MayBeFinished, EightMoves]): Board[NewS, NewM] =
        Board.createPrev(b)(MayBeFinished(), SevenMoves())
    }

  implicit lazy val takeBack9: TakeBack[Finished, Full] = 
    new TakeBack[Finished, Full] {
      type NewS = MayBeFinished
      type NewM = EightMoves
      
      def takeBack(b: Board[Finished, Full]): Board[NewS, NewM] =
        Board.createPrev(b)(MayBeFinished(), EightMoves())
    }
}
