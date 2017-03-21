import scalaz.{\/, -\/, \/-}

object Algebra {

  sealed trait Status extends Product with Serializable { val taken: Int }
  final case class NoMoves()    extends Status { val taken: Int = 0 }
  final case class OneMove()    extends Status { val taken: Int = 1 }
  final case class TwoMoves()   extends Status { val taken: Int = 2 }
  final case class ThreeMoves() extends Status { val taken: Int = 3 }
  final case class FourMoves()  extends Status { val taken: Int = 4 }
  final case class FiveMoves()  extends Status { val taken: Int = 5 }
  final case class SixMoves()   extends Status { val taken: Int = 6 }
  final case class SevenMoves() extends Status { val taken: Int = 7 }
  final case class EightMoves() extends Status { val taken: Int = 8 }
  final case class Full()       extends Status { val taken: Int = 9 }

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

  sealed trait Board[+S] {
    val s: S
    val ts: Set[Taken[Tile, Player]]
    val es: Set[Empty[Tile]]
    val h: List[Tile]
  }

  object Board {

    lazy val empty: Board[NoMoves] = new Board[NoMoves]{
      val s   = NoMoves()
      val ts  = Set.empty 
      val es  = Set(Tile11, Tile12, Tile13, Tile21, Tile22, Tile23, Tile31, Tile32, Tile33) map (Empty(_))
      val h   = Nil
    }

    def createNew[S <: Status](nS: S, nTs: Set[Taken[Tile, Player]], nEs: Set[Empty[Tile]], nH: List[Tile]): \/[String, Board[S]] = 
      nTs.size == nS.taken match {
        case true => \/-(new Board[S] {
            val s   = nS
            val ts  = nTs
            val es  = nEs
            val h   = nH
          })
        case false => -\/(s"Inconsistent state: $nS doesn't match with the taken positions $nTs")
    }

    def createPrev[S, S1](b: Board[S])(implicit P: Previous[S, S1]): Board[S1] = new Board[S1] {
      val s = P.getPrevious
      val ts = b.ts filterNot (_.t == b.h.head)
      val es = b.es + Empty(b.h.head)
      val h = b.h.tail
    }
  }
}
