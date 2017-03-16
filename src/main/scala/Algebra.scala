object Algebra {

  sealed trait Status extends Product with Serializable
  final case class NoMoves()    extends Status
  final case class OneMove()    extends Status
  final case class TwoMoves()   extends Status
  final case class ThreeMoves() extends Status
  final case class FourMoves()  extends Status
  final case class FiveMoves()  extends Status
  final case class SixMoves()   extends Status
  final case class SevenMoves() extends Status
  final case class EightMoves() extends Status
  final case class Full()       extends Status

  sealed trait Player extends Product with Serializable
  final case object X extends Player
  final case object O extends Player

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

  sealed trait Result[A] extends Product with Serializable
  final case class Winner[A](p: A) extends Result[A]
  final case object Draw extends Result[Nothing]

  final case class Board[+S] private (s: S, ts: Set[Empty[Tile]])

  object Board {
    lazy val empty = Board(
      NoMoves(), Set(Tile11, Tile12, Tile13, Tile21, Tile22, Tile23, Tile31, Tile32, Tile33) map (Empty(_))
    )
  }
}
