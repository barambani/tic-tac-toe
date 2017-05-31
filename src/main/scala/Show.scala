import Algebra._

sealed trait Show[A] {
  def show: String
}

object Show {

  def apply[A](implicit INST: Show[A]): Show[A] = INST

  implicit object noMovesShow extends Show[NoMoves] {
    def show: String = "no moves"
  }

  implicit object oneMoveShow extends Show[OneMove] {
    def show: String = "one move"
  }

  implicit object twoMovesShow extends Show[TwoMoves] {
    def show: String = "two moves"
  }

  implicit object threeMovesShow extends Show[ThreeMoves] {
    def show: String = "three moves"
  }

  implicit object fourMovesShow extends Show[FourMoves] {
    def show: String = "four moves"
  }

  implicit object fiveMovesShow extends Show[FiveMoves] {
    def show: String = "five moves"
  }

  implicit object sixMovesShow extends Show[SixMoves] {
    def show: String = "six moves"
  }

  implicit object sevenMovesShow extends Show[SevenMoves] {
    def show: String = "seven moves"
  }

  implicit object eightMovesShow extends Show[EightMoves] {
    def show: String = "eight moves"
  }

  implicit object fullShow extends Show[Full] {
    def show: String = "nine moves"
  }

  implicit object notStartedShow extends Show[NotStarted] {
    def show: String = "not started" 
  }

  implicit object inPlayShow extends Show[InPlay] {
    def show: String = "in play"
  }

  implicit object mayBeFinishedShow extends Show[MayBeFinished] {
    def show: String = "may be finished"
  }

  implicit object finishedShow extends Show[Finished] {
    def show: String = "finished"
  }
}
