import Algebra._
import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

sealed trait Move[S[_], S1[_], M, M1] {
  def move: Board[S, M] => Empty[Tile] => Player => \/[String, Board[S1, M1]]
}

object Move {

  def apply[S[_], S1[_], M, M1](implicit INST: Move[S, S1, M, M1]): Move[S, S1, M, M1] = INST
  
  implicit lazy val move0: Move[NotStarted, InPlay, NoMoves, OneMove] = new Move[NotStarted, InPlay, NoMoves, OneMove] {
    def move: Board[NotStarted, NoMoves] => Empty[Tile] => Player => \/[String, Board[InPlay, OneMove]] =
      b => t => p => newBoard(b, InPlay(OneMove()))(t, p)
  }

  implicit lazy val move1: Move[InPlay, InPlay, OneMove, TwoMoves] = new Move[InPlay, InPlay, OneMove, TwoMoves] {
    def move: Board[InPlay, OneMove] => Empty[Tile] => Player => \/[String, Board[InPlay, TwoMoves]] =
      b => t => p => newBoard(b, InPlay(TwoMoves()))(t, p)
  }

  implicit lazy val move2: Move[InPlay, InPlay, TwoMoves, ThreeMoves] = new Move[InPlay, InPlay, TwoMoves, ThreeMoves] {
    def move: Board[InPlay, TwoMoves] => Empty[Tile] => Player => \/[String, Board[InPlay, ThreeMoves]] = 
      b => t => p => newBoard(b, InPlay(ThreeMoves()))(t, p)
  }

  implicit lazy val move3: Move[InPlay, InPlay, ThreeMoves, FourMoves] = new Move[InPlay, InPlay, ThreeMoves, FourMoves]{
    def move: Board[InPlay, ThreeMoves] => Empty[Tile] => Player => \/[String, Board[InPlay, FourMoves]] = 
      b => t => p => newBoard(b, InPlay(FourMoves()))(t, p)
  }

  implicit lazy val move4: Move[InPlay, MayBeFinished, FourMoves, FiveMoves] = new Move[InPlay, MayBeFinished, FourMoves, FiveMoves]{
    def move: Board[InPlay, FourMoves] => Empty[Tile] => Player => \/[String, Board[MayBeFinished, FiveMoves]] = 
      b => t => p => newBoard(b, MayBeFinished(FiveMoves()))(t, p)
  }

  implicit lazy val move5: Move[MayBeFinished, MayBeFinished, FiveMoves, SixMoves] = new Move[MayBeFinished, MayBeFinished, FiveMoves, SixMoves]{
    def move: Board[MayBeFinished, FiveMoves] => Empty[Tile] => Player => \/[String, Board[MayBeFinished, SixMoves]] = 
      b => t => p => newBoard(b, MayBeFinished(SixMoves()))(t, p)
  }

  implicit lazy val move6: Move[MayBeFinished, MayBeFinished, SixMoves, SevenMoves] = new Move[MayBeFinished, MayBeFinished, SixMoves, SevenMoves]{
    def move: Board[MayBeFinished, SixMoves] => Empty[Tile] => Player => \/[String, Board[MayBeFinished, SevenMoves]] = 
      b => t => p => newBoard(b, MayBeFinished(SevenMoves()))(t, p)
  }

  implicit lazy val move7: Move[MayBeFinished, MayBeFinished, SevenMoves, EightMoves] = new Move[MayBeFinished, MayBeFinished, SevenMoves, EightMoves]{
    def move: Board[MayBeFinished, SevenMoves] => Empty[Tile] => Player => \/[String, Board[MayBeFinished, EightMoves]] = 
      b => t => p => newBoard(b, MayBeFinished(EightMoves()))(t, p)
  }

  implicit lazy val movea8: Move[MayBeFinished, Finished, EightMoves, Full] = new Move[MayBeFinished, Finished, EightMoves, Full]{
    def move: Board[MayBeFinished, EightMoves] => Empty[Tile] => Player => \/[String, Board[Finished, Full]] = 
      b => t => p => newBoard(b, Finished(Full()))(t, p)
  }

  private def newBoard[S[_] <: Status[_], S1[_] <: Status[_], M <: Moves, M1 <: Moves](b: Board[S, M], s1: S1[M1])(e: Empty[Tile], p: Player): \/[String, Board[S1, M1]] = 
    takeIfAvailable(b.es, e)((a, as) => as filterNot (_ == a)) flatMap {
      Board.createNew[S1, M1](s1)(_, Taken(e.t, p) :: b.h)
    }

  private def takeIfAvailable[A](xs: Set[A], a: A)(f: (A, Set[A]) => Set[A]): \/[String, Set[A]] =
    xs contains a match {
      case true   => \/-(f(a, xs))
      case false  => -\/(s"Tile $a already taken")
    }
}
