import Algebra._
import scalaz.{\/, -\/, \/-}

object TicTacToeApi {

  def startGame: Board[NotStarted, NoMoves] =
    Board.empty

  def nextMoveFor[S <: Status, M <: Move, S1 <: Status, M1 <: Move](b: Board[S, M])(t: Tile, p: Player)(
    implicit 
      NXT: Next.Aux[S, M, S1, M1],
      SS: Show[S1],
      SM: Show[M1]): \/[String, Board[S1, M1]] =
        move(b, Empty(t), p)

  def nextMoveFor[S <: Status, M <: Move, S1 <: Status, M1 <: Move](b: \/[String, Board[S, M]])(t: Tile, p: Player)(
    implicit 
      NXT: Next.Aux[S, M, S1, M1],
      SS: Show[S1],
      SM: Show[M1]): \/[String, Board[S1, M1]] =
        b flatMap (move(_, Empty(t), p))

  def move[S <: Status, M <: Move, S1 <: Status, M1 <: Move](b: Board[S, M], e: Empty[Tile], p: Player)(
    implicit 
      NXT: Next.Aux[S, M, S1, M1],
      SS: Show[S1],
      SM: Show[M1]): \/[String, Board[S1, M1]] =
        Board.tryMoveAt(b)(e, p)

  def whoWon[S <: Status, M <: Move](b: Board[S, M])(implicit W: WhoWon[S]): W.R =
    W.whoWon(b)

  def playerAt[S <: Status, M <: Move](b: Board[S, M], t: Tile): Option[Player] =
    b.playerAt(t)

  def takeBack[S <: Status, M <: Move, S1 <: Status, M1 <: Move](b: Board[S, M])(
    implicit 
      PRV: Previous.Aux[S, M, S1, M1],
      SS: Show[S1],
      SM: Show[M1]): Board[S1, M1] =
        Board.takeBack(b)

  def isDraw(b: Board[Finished, Full]): Boolean = true

  implicit class TicTacToeApiOps[S, M](b: \/[String, Board[S, M]]) {
    def asString: String = b.fold(identity, _.toString)
  }
}
