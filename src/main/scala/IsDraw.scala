import Algebra._

sealed trait IsDraw[S] {
  def isDraw: Board[S] => Boolean
}

object IsDraw {
  def apply[S](implicit INST: IsDraw[S]): IsDraw[S] = INST
  
  implicit lazy val isDraw1: IsDraw[Full] = new IsDraw[Full] {
    def isDraw: Board[Full] => Boolean = ???
  }

}
