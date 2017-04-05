import scalaz.{\/, -\/, \/-}
import scala.language.higherKinds

object Algebra {

  sealed trait Moves extends Product with Serializable
  sealed trait NoMoves    extends Moves
  sealed trait OneMove    extends Moves
  sealed trait TwoMoves   extends Moves
  sealed trait ThreeMoves extends Moves
  sealed trait FourMoves  extends Moves
  sealed trait FiveMoves  extends Moves
  sealed trait SixMoves   extends Moves
  sealed trait SevenMoves extends Moves
  sealed trait EightMoves extends Moves
  sealed trait Full       extends Moves
  final case object NoMoves    extends NoMoves
  final case object OneMove    extends OneMove
  final case object TwoMoves   extends TwoMoves
  final case object ThreeMoves extends ThreeMoves
  final case object FourMoves  extends FourMoves
  final case object FiveMoves  extends FiveMoves
  final case object SixMoves   extends SixMoves
  final case object SevenMoves extends SevenMoves
  final case object EightMoves extends EightMoves
  final case object Full       extends Full

  
  sealed trait Player extends Product with Serializable
  sealed trait X extends Player
  sealed trait O extends Player
  final case object X extends X { override def toString: String = "X" }
  final case object O extends O { override def toString: String = "O" }

  
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
  
  sealed trait PlayAgain extends Product with Serializable
  final case object PlayAgain extends PlayAgain

  sealed trait Draw extends Product with Serializable
  final case object Draw extends Draw

  
  sealed trait Status extends Product with Serializable
  sealed trait NotStarted extends Status
  sealed trait InPlay extends Status
  sealed trait MayBeFinished extends Status
  sealed trait Finished extends Status
  final case object NotStarted    extends NotStarted
  final case object InPlay        extends InPlay
  final case object MayBeFinished extends MayBeFinished
  final case object Finished      extends Finished


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
      playerAt(t).fold("_"){ _.toString }
  }
  
  object Board {

    lazy val empty: Board[NotStarted, NoMoves] = new Board[NotStarted, NoMoves]{
      val s   = NotStarted
      val m   = NoMoves
      val es  = Set(Tile11, Tile12, Tile13, Tile21, Tile22, Tile23, Tile31, Tile32, Tile33) map Empty.apply
      val h   = Nil
    }

    def tryMoveAt[S <: Status, M <: Moves](b: Board[S, M])(e: Empty[Tile], p: Player)(implicit M: Move[S, M]): \/[String, Board[M.NewS, M.NewM]] = 
      takeIfAvailable(b.es, e) map {
        newEmpty => new Board[M.NewS, M.NewM] {
            val s   = M.s
            val m   = M.m
            val es  = newEmpty
            val h   = Taken(e.t, p) :: b.h
        }
      }

    private def takeIfAvailable[A](xs: Set[Empty[A]], a: Empty[A]): \/[String, Set[Empty[A]]] =
      xs contains a match {
        case true   => \/-(xs filterNot (_ == a))
        case false  => -\/(s"${ a.t } already taken")
      }

    def createPrev[S <: Status, M <: Moves](b: Board[S, M])(implicit PRV: Previous[S, M]): Board[PRV.NewS, PRV.NewM] = new Board[PRV.NewS, PRV.NewM] {
      val s = PRV.s
      val m = PRV.m
      val es = b.es + Empty(b.h.head.t)
      val h = b.h.tail
    }
  }
}
