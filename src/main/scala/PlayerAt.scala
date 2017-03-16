import Algebra._

sealed trait PlayerAt[S] {
  def playerAt[T]: Board[S] => T => Option[Player]
}

object PlayerAt{
  def apply[S](implicit INST: PlayerAt[S]): PlayerAt[S] = INST

  implicit lazy val playerAt1: PlayerAt[NoMoves] = new PlayerAt[NoMoves] {
    def playerAt[T]: Board[NoMoves] => T => Option[Player] = ???
  }
}
