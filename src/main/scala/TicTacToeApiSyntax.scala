import Algebra._
import TicTacToeApi._
import scalaz.{\/, \/-}

object TicTacToeApiSyntax {

  implicit final class TicTacToeApiSyntax1[S <: Status, M <: Move](b: Board[S, M]) {
    
    def afterMove[S1 <: Status, M1 <: Move](t: Tile, p: Player)(
      implicit 
        NXT: Next.Aux[S, M, S1, M1],
        SS: Show[S1],
        SM: Show[M1]): \/[String, Board[S1, M1]] = 
      TicTacToeApi.nextMoveFor(b)(t, p)
    
    def move[S1 <: Status, M1 <: Move](e: Empty[Tile], p: Player)(
      implicit 
        NXT: Next.Aux[S, M, S1, M1],
        SS: Show[S1],
        SM: Show[M1]): \/[String, Board[S1, M1]] = 
      TicTacToeApi.move(b, e, p)
    
    def playerAt(t: Tile): Option[Player] = 
      TicTacToeApi.playerAt(b, t)
  }

  implicit final class TicTacToeApiSyntax2[S <: Status, M <: Move](b: \/[String, Board[S, M]]) {
    
    def asString: String = 
      b.fold(identity, _.toString)
    
    def afterMove[S1 <: Status, M1 <: Move](t: Tile, p: Player)(
      implicit 
        NXT: Next.Aux[S, M, S1, M1],
        SS: Show[S1],
        SM: Show[M1]): \/[String, Board[S1, M1]] = 
      TicTacToeApi.nextMoveFor(b)(t, p)

    def takeBack[S1 <: Status, M1 <: Move](
      implicit
        PRV : Previous.Aux[S, M, S1, M1],
        SS  : Show[S1],
        SM  : Show[M1]): \/[String, Board[S1, M1]] =
      b map { eb => TicTacToeApi.takeBack(eb) }

    def whoWon(implicit W: WhoWon[S]): \/[String, W.R] =
      b flatMap { r => \/-(TicTacToeApi.whoWon(r)) }
  }

  implicit final class TicTacToeApiSyntax3(fullBoard: Board[Finished, Full]) {
    def isDraw: Boolean = 
      TicTacToeApi.isDraw(fullBoard)
  }
}
