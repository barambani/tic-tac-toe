import Algebra._
import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

sealed trait WhoWon[S <: Status] {
  type R
  def whoWon: Board[S, _] => R
}

object WhoWon {

  type Aux[S <: Status, Res] = WhoWon[S] { type R = Res } 
  
  def apply[S <: Status](implicit INST: WhoWon[S]): Aux [S, INST.R] = INST

  implicit lazy val whoWon5: Aux[MayBeFinished, \/[PlayAgain, Winner[Player]]] = 
    new WhoWon[MayBeFinished] {
      type R = \/[PlayAgain, Winner[Player]]
      
      def whoWon: Board[MayBeFinished, _] => R =
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
  
  implicit lazy val whoWon9: Aux[Finished, Draw] =
    new WhoWon[Finished] {
      type R = Draw
      def whoWon: Board[Finished, _] => R =
        _ => Draw()
    }
}
