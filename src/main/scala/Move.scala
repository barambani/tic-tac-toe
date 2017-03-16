import Algebra._
import scalaz.{\/, -\/, \/-}

sealed trait Move[S, S1] {
  def move: Board[S] => Empty[Tile] => \/[String, Board[S1]]
}

object Move {

  def apply[S, S1](implicit INST: Move[S, S1]): Move[S, S1] = INST
  
  implicit lazy val move0: Move[NoMoves, OneMove] = new Move[NoMoves, OneMove] {
    def move: Board[NoMoves] => Empty[Tile] => \/[String, Board[OneMove]] =
      b => t => newBoard(OneMove(), b.ts, t)
  }

  implicit lazy val move1: Move[OneMove, TwoMoves] = new Move[OneMove, TwoMoves] {
    def move: Board[OneMove] => Empty[Tile] => \/[String, Board[TwoMoves]] =
      b => t => newBoard(TwoMoves(), b.ts, t)
  }

  implicit lazy val move2: Move[TwoMoves, ThreeMoves] = new Move[TwoMoves, ThreeMoves] {
    def move: Board[TwoMoves] => Empty[Tile] => \/[String, Board[ThreeMoves]] = 
      b => t => newBoard(ThreeMoves(), b.ts, t)
  }

  implicit lazy val move3: Move[ThreeMoves, FourMoves] = new Move[ThreeMoves, FourMoves]{
    def move: Board[ThreeMoves] => Empty[Tile] => \/[String, Board[FourMoves]] = 
      b => t => newBoard(FourMoves(), b.ts, t)
  }

  implicit lazy val move4: Move[FourMoves, FiveMoves] = new Move[FourMoves, FiveMoves]{
    def move: Board[FourMoves] => Empty[Tile] => \/[String, Board[FiveMoves]] = 
      b => t => newBoard(FiveMoves(), b.ts, t)
  }

  implicit lazy val move5: Move[FiveMoves, SixMoves] = new Move[FiveMoves, SixMoves]{
    def move: Board[FiveMoves] => Empty[Tile] => \/[String, Board[SixMoves]] = 
      b => t => newBoard(SixMoves(), b.ts, t)
  }

  implicit lazy val move6: Move[SixMoves, SevenMoves] = new Move[SixMoves, SevenMoves]{
    def move: Board[SixMoves] => Empty[Tile] => \/[String, Board[SevenMoves]] = 
      b => t => newBoard(SevenMoves(), b.ts, t)
  }

  implicit lazy val move7: Move[SevenMoves, EightMoves] = new Move[SevenMoves, EightMoves]{
    def move: Board[SevenMoves] => Empty[Tile] => \/[String, Board[EightMoves]] = 
      b => t => newBoard(EightMoves(), b.ts, t)
  }

  implicit lazy val movea8: Move[EightMoves, Full] = new Move[EightMoves, Full]{
    def move: Board[EightMoves] => Empty[Tile] => \/[String, Board[Full]] = 
      b => t => newBoard(Full(), b.ts, t)
  }

  private def newBoard[S1](s1: S1, es: Set[Empty[Tile]], e: Empty[Tile]): \/[String, Board[S1]] =
    es contains e match {
      case true   => \/-(Board(s1, es filterNot (_ == e)))
      case false  => -\/(s"Tile ${e.t} already taken")
    }
}
