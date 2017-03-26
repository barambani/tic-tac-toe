import Algebra._
import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

sealed trait WhoWon[S[_], R] {
  def whoWon: Board[S, _] => R
}

object WhoWon {
  
  def apply[S[_], R](implicit INST: WhoWon[S, R]): WhoWon[S, R] = INST

  implicit lazy val whoWon5: WhoWon[MayBeFinished, \/[PlayAgain, Winner[Player]]] = new WhoWon[MayBeFinished, \/[PlayAgain, Winner[Player]]] {
    
    def whoWon: Board[MayBeFinished, _] => \/[PlayAgain, Winner[Player]] =
      b => 
        if(playerWins(X(), b.h))      \/-(Winner(X()))
        else if(playerWins(O(), b.h)) \/-(Winner(O()))
        else                          -\/(PlayAgain())

    private def playerWins(p: Player, h: List[Taken[Tile, Player]]): Boolean =
      hasWinningSequence(h filter (_.p == p) map (_.t))

    private def hasWinningSequence(h: List[Tile]): Boolean =
      ((h contains Tile11) && (h contains Tile12) && (h contains Tile13)) ||
      ((h contains Tile21) && (h contains Tile22) && (h contains Tile23)) ||
      ((h contains Tile31) && (h contains Tile32) && (h contains Tile33)) ||
      ((h contains Tile11) && (h contains Tile21) && (h contains Tile31)) ||
      ((h contains Tile12) && (h contains Tile22) && (h contains Tile32)) ||
      ((h contains Tile13) && (h contains Tile23) && (h contains Tile33)) ||
      ((h contains Tile11) && (h contains Tile22) && (h contains Tile33)) ||
      ((h contains Tile13) && (h contains Tile22) && (h contains Tile31))
  }
  
  implicit lazy val whoWon9: WhoWon[Finished, Draw] = new WhoWon[Finished, Draw] {
    def whoWon: Board[Finished, _] => Draw = 
      _ => Draw()
  }
}
