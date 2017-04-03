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

  
  sealed trait Player extends Product with Serializable { def print: String }
  final case class X() extends Player { def print: String = "X" }
  final case class O() extends Player { def print: String = "O" }

  
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

  
  sealed trait Status extends Product with Serializable
  final case class NotStarted()     extends Status
  final case class InPlay()         extends Status
  final case class MayBeFinished()  extends Status
  final case class Finished ()      extends Status


  sealed trait Board[S, M] {
    val s: S
    val m: M
    val es: Set[Empty[Tile]]
    val h: List[Taken[Tile, Player]]

    def playerAt(t: Tile): Option[Player] =
      h find { _.t == t } map { _.p }

    override def toString: String = s"""
      ${ printPlayerAt(Tile11) }  ${ printPlayerAt(Tile12) }  ${ printPlayerAt(Tile13) }
      ${ printPlayerAt(Tile21) }  ${ printPlayerAt(Tile22) }  ${ printPlayerAt(Tile23) }
      ${ printPlayerAt(Tile31) }  ${ printPlayerAt(Tile32) }  ${ printPlayerAt(Tile33) }

      Status: $s with $m"""

    private def printPlayerAt(t: Tile): String =
      playerAt(t).fold("_"){ _.print }
  }
  
  object Board {

    lazy val empty: Board[NotStarted, NoMoves] = new Board[NotStarted, NoMoves]{
      val s   = NotStarted()
      val m   = NoMoves()
      val es  = Set(Tile11, Tile12, Tile13, Tile21, Tile22, Tile23, Tile31, Tile32, Tile33) map Empty.apply
      val h   = Nil
    }

    def createNew[S <: Status, M <: Moves](nS: S, nM: M)(nEs: Set[Empty[Tile]], nH: List[Taken[Tile, Player]]): \/[String, Board[S, M]] = 
      nH.size == nM.taken match { // TODO: Check also the Status
        case true => \/-(new Board[S, M] {
            val s   = nS
            val m   = nM
            val es  = nEs
            val h   = nH
          })
        case false => -\/(s"Inconsistent state: $nM doesn't match with the taken positions $nH")
      }

    def createPrev[S <: Status, M <: Moves](b: Board[S, M])(implicit PRV: Previous[S, M]): Board[PRV.NewS, PRV.NewM] = new Board[PRV.NewS, PRV.NewM] {
      val s = PRV.s
      val m = PRV.m
      val es = b.es + Empty(b.h.head.t)
      val h = b.h.tail
    }
  }
}
