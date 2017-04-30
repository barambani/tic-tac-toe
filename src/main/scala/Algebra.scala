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
  final case object X extends X 
  final case object O extends O 

  
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

    lazy val empty: Board[NotStarted, NoMoves] =
      new Board[NotStarted, NoMoves]{
        val s   = NotStarted
        val m   = NoMoves
        val h   = Nil
      }

    def tryMoveAt[S <: Status, M <: Moves](b: Board[S, M])(e: Empty[Tile], p: Player)(implicit NXT: Next[S, M]): \/[String, Board[NXT.NewS, NXT.NewM]] = 
      takeIfAvailable(b.h, Taken(e.t, p)) map {
        taken => new Board[NXT.NewS, NXT.NewM] {
            val s   = NXT.s
            val m   = NXT.m
            val h   = taken :: b.h
        }
      }

    private def takeIfAvailable(hs: List[Taken[Tile, Player]], t: => Taken[Tile, Player]): \/[String, Taken[Tile, Player]] =
      hs exists (_.t == t.t) match {
        case false  => \/-(t)
        case true   => -\/(s"${ t.t } already taken")
      }

    def takeBack[S <: Status, M <: Moves](b: Board[S, M])(implicit PRV: Previous[S, M]): Board[PRV.NewS, PRV.NewM] = 
      new Board[PRV.NewS, PRV.NewM] {
        val s = PRV.s
        val m = PRV.m
        val h = b.h.tail
      }
  }
}
