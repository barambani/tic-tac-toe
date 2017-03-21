import Algebra._
import scalaz.{\/, -\/, \/-}

sealed trait Move[S, S1] {
  def move: Board[S] => Empty[Tile] => Player => \/[String, Board[S1]]
}

object Move {

  def apply[S, S1](implicit INST: Move[S, S1]): Move[S, S1] = INST
  
  implicit lazy val move0: Move[NoMoves, OneMove] = new Move[NoMoves, OneMove] {
    def move: Board[NoMoves] => Empty[Tile] => Player => \/[String, Board[OneMove]] =
      b => t => p => newBoard(b, OneMove())(t, p)
  }

  implicit lazy val move1: Move[OneMove, TwoMoves] = new Move[OneMove, TwoMoves] {
    def move: Board[OneMove] => Empty[Tile] => Player => \/[String, Board[TwoMoves]] =
      b => t => p => newBoard(b, TwoMoves())(t, p)
  }

  implicit lazy val move2: Move[TwoMoves, ThreeMoves] = new Move[TwoMoves, ThreeMoves] {
    def move: Board[TwoMoves] => Empty[Tile] => Player => \/[String, Board[ThreeMoves]] = 
      b => t => p => newBoard(b, ThreeMoves())(t, p)
  }

  implicit lazy val move3: Move[ThreeMoves, FourMoves] = new Move[ThreeMoves, FourMoves]{
    def move: Board[ThreeMoves] => Empty[Tile] => Player => \/[String, Board[FourMoves]] = 
      b => t => p => newBoard(b, FourMoves())(t, p)
  }

  implicit lazy val move4: Move[FourMoves, FiveMoves] = new Move[FourMoves, FiveMoves]{
    def move: Board[FourMoves] => Empty[Tile] => Player => \/[String, Board[FiveMoves]] = 
      b => t => p => newBoard(b, FiveMoves())(t, p)
  }

  implicit lazy val move5: Move[FiveMoves, SixMoves] = new Move[FiveMoves, SixMoves]{
    def move: Board[FiveMoves] => Empty[Tile] => Player => \/[String, Board[SixMoves]] = 
      b => t => p => newBoard(b, SixMoves())(t, p)
  }

  implicit lazy val move6: Move[SixMoves, SevenMoves] = new Move[SixMoves, SevenMoves]{
    def move: Board[SixMoves] => Empty[Tile] => Player => \/[String, Board[SevenMoves]] = 
      b => t => p => newBoard(b, SevenMoves())(t, p)
  }

  implicit lazy val move7: Move[SevenMoves, EightMoves] = new Move[SevenMoves, EightMoves]{
    def move: Board[SevenMoves] => Empty[Tile] => Player => \/[String, Board[EightMoves]] = 
      b => t => p => newBoard(b, EightMoves())(t, p)
  }

  implicit lazy val movea8: Move[EightMoves, Full] = new Move[EightMoves, Full]{
    def move: Board[EightMoves] => Empty[Tile] => Player => \/[String, Board[Full]] = 
      b => t => p => newBoard(b, Full())(t, p)
  }

  private def newBoard[S <: Status, S1 <: Status](b: Board[S], s1: S1)(e: Empty[Tile], p: Player): \/[String, Board[S1]] = 
    takeIfAvailable(b.es, e)((a, as) => as filterNot (_ == a)) flatMap {
      Board.createNew(s1, b.ts + Taken(e.t, p), _, e.t :: b.h)
    }

  private def takeIfAvailable[A](xs: Set[A], a: A)(f: (A, Set[A]) => Set[A]): \/[String, Set[A]] =
    xs contains a match {
      case true   => \/-(f(a, xs))
      case false  => -\/(s"Tile $a already taken")
    }
}
