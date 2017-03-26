import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

object Algebra {

  sealed trait Moves extends Product with Serializable { val taken: Int }
  final case class NoMoves()    extends Moves { val taken: Int = 0 }
  final case class OneMove()    extends Moves { val taken: Int = 1 }
  final case class TwoMoves()   extends Moves { val taken: Int = 2 }
  final case class ThreeMoves() extends Moves { val taken: Int = 3 }
  final case class FourMoves()  extends Moves { val taken: Int = 4 }
  final case class FiveMoves()  extends Moves { val taken: Int = 5 }
  final case class SixMoves()   extends Moves { val taken: Int = 6 }
  final case class SevenMoves() extends Moves { val taken: Int = 7 }
  final case class EightMoves() extends Moves { val taken: Int = 8 }
  final case class Full()       extends Moves { val taken: Int = 9 }

  
  sealed trait Player extends Product with Serializable
  final case class X() extends Player
  final case class O() extends Player

  
  sealed trait Tile extends Product with Serializable
  final case object Tile11 extends Tile
  final case object Tile12 extends Tile
  final case object Tile13 extends Tile
  final case object Tile21 extends Tile
  final case object Tile22 extends Tile
  final case object Tile23 extends Tile
  final case object Tile31 extends Tile
  final case object Tile32 extends Tile
  final case object Tile33 extends Tile

  
  sealed trait Position[A] extends Product with Serializable
  final case class Empty[A](t: A) extends Position[A]
  final case class Taken[A, P](t: A, p: P) extends Position[A]

  
  final case class Winner[A](p: A)
  final case class PlayAgain()
  final case class Draw()

  
  sealed trait Status[M] extends Product with Serializable { 
    val m: M   
    def moves: Int = this.m match {
      case m: Moves => m.taken
      case _        => 10
    }
  }
  final case class NotStarted[M](m: M)     extends Status[M]
  final case class InPlay[M](m: M)         extends Status[M]
  final case class MayBeFinished[M](m: M)  extends Status[M]
  final case class Finished[M](m: M)       extends Status[M]


  sealed trait Board[S[_], M] {
    val s: S[M]
    val es: Set[Empty[Tile]]
    val h: List[Taken[Tile, Player]]

    def playerAt(t: Tile): Option[Player] =
      h find { _.t == t } map { _.p }

    def print: String =
      s"""
        Status $s
        Empty $es
        Taken $h
      """
  }
  
  object Board {

    lazy val empty: Board[NotStarted, NoMoves] = new Board[NotStarted, NoMoves]{
      val s   = NotStarted(NoMoves())
      val es  = Set(Tile11, Tile12, Tile13, Tile21, Tile22, Tile23, Tile31, Tile32, Tile33) map (Empty(_))
      val h   = Nil
    }

    def createNew[S[_] <: Status[_], M <: Moves](nS: S[M])(nEs: Set[Empty[Tile]], nH: List[Taken[Tile, Player]]): \/[String, Board[S, M]] = 
      nH.size == nS.moves match { // TODO: Check also the Status
        case true => \/-(new Board[S, M] {
            val s   = nS
            val es  = nEs
            val h   = nH
          })
        case false => -\/(s"Inconsistent state: $nS doesn't match with the taken positions $nH")
    }

    def createPrev[S[_], S1[_], M, M1](b: Board[S, M])(implicit P: Previous[S, S1, M, M1]): Board[S1, M1] = new Board[S1, M1] {
      val s = P.getPrevious
      val es = b.es + Empty(b.h.head.t)
      val h = b.h.tail
    }
  }
}
