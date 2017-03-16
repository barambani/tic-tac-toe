import Algebra._

sealed trait TakeBack[S, S1] {
  def takeBack: Board[S] => Board[S1]
}

object TakeBack {

  def apply[S, S1](implicit INST: TakeBack[S, S1]): TakeBack[S, S1] = INST

  implicit lazy val takeBack1: TakeBack[OneMove, NoMoves] = new TakeBack[OneMove, NoMoves] {
    def takeBack: Board[OneMove] => Board[NoMoves] = ???
  }
}
