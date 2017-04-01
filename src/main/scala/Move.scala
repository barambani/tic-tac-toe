import Algebra._
import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

sealed trait Move[S <: Status, M <: Moves] {
  type NewS
  type NewM

  def move: Board[S, M] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]]
}

object Move {

  type Aux[S <: Status, M <: Moves, S1, M1] = Move[S, M] { type NewS = S1; type NewM = M1 }

  def apply[S <: Status, M <: Moves](implicit INST: Move[S, M]): Aux[S, M, INST.NewS, INST.NewM] = INST
  
  implicit lazy val move0: Move[NotStarted, NoMoves] = 
    new Move[NotStarted, NoMoves] {
      type NewS = InPlay
      type NewM = OneMove

      def move: Board[NotStarted, NoMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] =
        b => t => p => newBoard(b)(InPlay(), OneMove())(t, p)
    }

  implicit lazy val move1: Move[InPlay, OneMove] = 
    new Move[InPlay, OneMove] {
      type NewS = InPlay
      type NewM = TwoMoves

      def move: Board[InPlay, OneMove] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] =
        b => t => p => newBoard(b)(InPlay(), TwoMoves())(t, p)
    }

  implicit lazy val move2: Move[InPlay, TwoMoves] =
    new Move[InPlay, TwoMoves] {
      type NewS = InPlay
      type NewM = ThreeMoves

      def move: Board[InPlay, TwoMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(InPlay(), ThreeMoves())(t, p)
    }

  implicit lazy val move3: Move[InPlay, ThreeMoves] = 
    new Move[InPlay, ThreeMoves] {
      type NewS = InPlay
      type NewM = FourMoves

      def move: Board[InPlay, ThreeMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(InPlay(), FourMoves())(t, p)
    }

  implicit lazy val move4: Move[InPlay, FourMoves] = 
    new Move[InPlay, FourMoves] {
      type NewS = MayBeFinished
      type NewM = FiveMoves

      def move: Board[InPlay, FourMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(MayBeFinished(), FiveMoves())(t, p)
    }

  implicit lazy val move5: Move[MayBeFinished, FiveMoves] = 
    new Move[MayBeFinished, FiveMoves] {
      type NewS = MayBeFinished
      type NewM = SixMoves

      def move: Board[MayBeFinished, FiveMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(MayBeFinished(), SixMoves())(t, p)
    }

  implicit lazy val move6: Move[MayBeFinished, SixMoves] = 
    new Move[MayBeFinished, SixMoves] {
      type NewS = MayBeFinished
      type NewM = SevenMoves

      def move: Board[MayBeFinished, SixMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(MayBeFinished(), SevenMoves())(t, p)
    }

  implicit lazy val move7: Move[MayBeFinished, SevenMoves] = 
    new Move[MayBeFinished, SevenMoves] {
      type NewS = MayBeFinished
      type NewM = EightMoves

      def move: Board[MayBeFinished, SevenMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(MayBeFinished(), EightMoves())(t, p)
    }

  implicit lazy val movea8: Move[MayBeFinished, EightMoves] = 
    new Move[MayBeFinished, EightMoves] {
      type NewS = Finished
      type NewM = Full

      def move: Board[MayBeFinished, EightMoves] => Empty[Tile] => Player => \/[String, Board[NewS, NewM]] = 
        b => t => p => newBoard(b)(Finished(), Full())(t, p)
    }

  private def newBoard[S <: Status, S1 <: Status, M <: Moves, M1 <: Moves](b: Board[S, M])(nS: S1, nM: M1)(e: Empty[Tile], p: Player): \/[String, Board[S1, M1]] = 
    takeIfAvailable(b.es, e)((a, as) => as filterNot (_ == a)) flatMap {
      Board.createNew[S1, M1](nS, nM)(_, Taken(e.t, p) :: b.h)
    }

  private def takeIfAvailable[A](xs: Set[Empty[A]], a: Empty[A])(f: (Empty[A], Set[Empty[A]]) => Set[Empty[A]]): \/[String, Set[Empty[A]]] =
    xs contains a match {
      case true   => \/-(f(a, xs))
      case false  => -\/(s"${ a.t } already taken")
    }
}
