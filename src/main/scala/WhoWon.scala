import Algebra._

sealed trait WhoWon[S] {
  def whoWon: Board[S] => Result[Player]
}
object WhoWon {
  def apply[S](implicit INST: WhoWon[S]): WhoWon[S] = INST

  implicit lazy val whoWon1: WhoWon[FiveMoves] = new WhoWon[FiveMoves] {
    def whoWon: Board[FiveMoves] => Result[Player] = ???
  }
  
  implicit lazy val whoWon2: WhoWon[Full] = new WhoWon[Full] {
    def whoWon: Board[Full] => Result[Player] = ???
  }
}
