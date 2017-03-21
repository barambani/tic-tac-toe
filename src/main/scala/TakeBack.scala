import Algebra._

sealed trait TakeBack[S, S1] {
  def takeBack(b: Board[S])(implicit P: Previous[S, S1]): Board[S1] =
    Board.createPrev(b)
}

object TakeBack {
  def apply[S, S1]: TakeBack[S, S1] = new TakeBack[S, S1] {}
}
