import Algebra._
import scalaz.{\/, -\/, \/-}
import Previous._
import scala.language.higherKinds

object TicTacToeApi {

  def startGame: Board[NotStarted, NoMoves] =
    Board.empty

  def nextMove[S[_], S1[_], M, M1](b: Board[S, M])(t: Tile, p: Player)(implicit M: Move[S, S1, M, M1]): \/[String, Board[S1, M1]] =
    move(b, Empty(t), p)

  def nextMove[S[_], S1[_], M, M1](b: \/[String, Board[S, M]])(t: Tile, p: Player)(implicit M: Move[S, S1, M, M1]): \/[String, Board[S1, M1]] =
    b flatMap (move(_, Empty(t), p))

  def move[S[_], S1[_], M, M1](b: Board[S, M], e: Empty[Tile], p: Player)(implicit M: Move[S, S1, M, M1]): \/[String, Board[S1, M1]] =
    M.move(b)(e)(p)

  def whoWon[S[_], R](b: Board[S, _])(implicit W: WhoWon[S, R]): R =
    W.whoWon(b)

  def playerAt[S[_], M](b: Board[S, M], t: Tile): Option[Player] =
    b.playerAt(t)

  def takeBack[S[_], S1[_], M, M1](b: Board[S, M])(implicit P: Previous[S, S1, M, M1]): Board[S1, M1] =
    TakeBack[S, S1, M, M1].takeBack(b)

  def isDraw(b: Board[Finished, Full]): Boolean = true

  def display[S[_], M](b: \/[String, Board[S, M]]): String =
    b.fold(identity, _.print)
}
