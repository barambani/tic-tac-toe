import Algebra._
import scalaz.{\/, -\/, \/-}
import Previous._

object Api {

  object TicTacToe {

    def move[S, S1](b: Board[S], e: Empty[Tile], p: Player)(implicit M: Move[S, S1]): \/[String, Board[S1]] =
      M.move(b)(e)(p)

    def whoWon[S](b: Board[S])(implicit W: WhoWon[S]): Result[Player] =
      W.whoWon(b)

    def playerAt[S, T](b: Board[S], t: T)(implicit PA: PlayerAt[S]): Option[Player] =
      PA.playerAt(b)(t)

    def takeBack[S, S1](b: Board[S])(implicit P: Previous[S, S1]): Board[S1] =
      TakeBack[S, S1].takeBack(b)

    def isDraw[S](b: Board[S])(implicit ID: IsDraw[S]): Boolean =
      ID.isDraw(b)
  }
}
