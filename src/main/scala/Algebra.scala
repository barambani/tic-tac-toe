import scalaz.{\/, -\/, \/-}

object Algebra {

  sealed trait Move extends Product with Serializable
  sealed trait NoMoves extends Move
  final case object NoMoves extends NoMoves
  final case class Succ[M <: Move](m: M) extends Move

  type OneMove    = Succ[NoMoves]
  type TwoMoves   = Succ[OneMove]
  type ThreeMoves = Succ[TwoMoves]
  type FourMoves  = Succ[ThreeMoves]
  type FiveMoves  = Succ[FourMoves]
  type SixMoves   = Succ[FiveMoves]
  type SevenMoves = Succ[SixMoves]
  type EightMoves = Succ[SevenMoves]
  type Full       = Succ[EightMoves]

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


  sealed abstract class Board[S, M](implicit SS: Show[S], SM: Show[M]) {

    val h: List[Taken[Tile, Player]]

    def playerAt(t: Tile): Option[Player] =
      h find { _.t == t } map { _.p }

    override def toString: String = s"""
      ${ printPlayerAt(Tile11) }  ${ printPlayerAt(Tile12) }  ${ printPlayerAt(Tile13) }
      ${ printPlayerAt(Tile21) }  ${ printPlayerAt(Tile22) }  ${ printPlayerAt(Tile23) }
      ${ printPlayerAt(Tile31) }  ${ printPlayerAt(Tile32) }  ${ printPlayerAt(Tile33) }

      Status: ${SS.show} with ${SM.show}"""

    private def printPlayerAt(t: Tile): String =
      playerAt(t).fold("_"){ _.toString }
  }
  
  object Board {

    lazy val empty: Board[NotStarted, NoMoves] =
      new Board[NotStarted, NoMoves]()(Show[NotStarted], Show[NoMoves]) { val h = Nil }

    def tryMoveAt[S <: Status, M <: Move, S1 <: Status, M1 <: Move](b: Board[S, M])(e: Empty[Tile], p: Player)(
      implicit 
        NXT: Next.Aux[S, M, S1, M1], 
        SS: Show[S1],
        SM: Show[M1]): \/[String, Board[S1, M1]] = 
      takeIfAvailable(b.h, Taken(e.t, p)) map {
        taken => new Board[S1, M1] { val h = taken :: b.h }
      }

    private def takeIfAvailable(hs: List[Taken[Tile, Player]], t: => Taken[Tile, Player]): \/[String, Taken[Tile, Player]] =
      hs exists (_.t == t.t) match {
        case false  => \/-(t)
        case true   => -\/(s"${ t.t } already taken")
      }

    def takeBack[S <: Status, M <: Move, S1 <: Status, M1 <: Move](b: Board[S, M])(
      implicit 
        PRV: Previous.Aux[S, M, S1, M1],
        SS: Show[S1],
        SM: Show[M1]): Board[S1, M1] = 
      new Board[S1, M1] { val h = b.h.tail }
  }
}
