import Algebra._
import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

object TicTacToeApi {

  def startGame: Board[NotStarted, NoMoves] =
    Board.empty

  def nextMoveFor[S <: Status, M <: Moves](b: Board[S, M])(t: Tile, p: Player)(implicit NXT: Next[S, M]): \/[String, Board[NXT.NewS, NXT.NewM]] =
    move(b, Empty(t), p)

  def nextMoveFor[S <: Status, M <: Moves](b: \/[String, Board[S, M]])(t: Tile, p: Player)(implicit NXT: Next[S, M]): \/[String, Board[NXT.NewS, NXT.NewM]] =
    b flatMap (move(_, Empty(t), p))

  def move[S <: Status, M <: Moves](b: Board[S, M], e: Empty[Tile], p: Player)(implicit NXT: Next[S, M]): \/[String, Board[NXT.NewS, NXT.NewM]] =
    Board.tryMoveAt(b)(e, p)

  def whoWon[S <: Status, M <: Moves](b: Board[S, M])(implicit W: WhoWon[S]): W.R =
    W.whoWon(b)

  def playerAt[S <: Status, M <: Moves](b: Board[S, M], t: Tile): Option[Player] =
    b.playerAt(t)

  def takeBack[S <: Status, M <: Moves](b: Board[S, M])(implicit PRV: Previous[S, M]): Board[PRV.NewS, PRV.NewM] =
    Board.takeBack(b)

  def isDraw(b: Board[Finished, Full]): Boolean = true

  implicit class TicTacToeApiOps[S, M](b: \/[String, Board[S, M]]) {
    def asString: String = b.fold(identity, _.toString)
  }
}
