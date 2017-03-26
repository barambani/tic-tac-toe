import Algebra._
import scala.language.higherKinds

sealed trait TakeBack[S[_], S1[_], M, M1] {
  def takeBack(b: Board[S, M])(implicit P: Previous[S, S1, M, M1]): Board[S1, M1] =
    Board.createPrev(b)
}

object TakeBack {
  def apply[S[_], S1[_], M, M1]: TakeBack[S, S1, M, M1] = new TakeBack[S, S1, M, M1] {}
}
