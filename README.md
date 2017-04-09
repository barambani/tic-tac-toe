# Tic Tac Toe


Possible solution to the Tic Tac Toe challenge proposed in the Tony Morris' fpcourse

https://github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown

&nbsp;

start a new game
```
scala> val init = startGame
init: Algebra.Board[Algebra.NotStarted,Algebra.NoMoves] =

      _  _  _
      _  _  _
      _  _  _

      Status: NotStarted with NoMoves
 ```

&nbsp;

first move:
```
scala> val m1 = nextMoveFor(init)(Tile22, X)
m1: scalaz.\/[String,Algebra.Board[Next.move0.NewS,Next.move0.NewM]] =
\/-(
      _  _  _
      _  X  _
      _  _  _

      Status: InPlay with OneMove)
```

&nbsp;

second move:
```
scala> val m2 = nextMoveFor(m1)(Tile23, O)
m2: scalaz.\/[String,Algebra.Board[Next.move1.NewS,Next.move1.NewM]] =
\/-(
      _  _  _
      _  X  O
      _  _  _

      Status: InPlay with TwoMoves)
```

&nbsp;

whoWon for impossible scenario:
```
scala> m2 flatMap (whoWon(_))
<console>:19: error: could not find implicit value for parameter W: WhoWon[Next.move1.NewS]
       m2 flatMap (whoWon(_))
                         ^
```

&nbsp;

winning move:
```
scala> val m7 = nextMoveFor(m6)(Tile12, X)
m7: scalaz.\/[String,Algebra.Board[Next.move6.NewS,Next.move6.NewM]] =
\/-(
      O  X  _
      _  X  O
      O  X  X

      Status: MayBeFinished with SevenMoves)
```

&nbsp;

whoWon on possible case with winning player
```
scala> m7 flatMap (whoWon(_))
res6: scalaz.\/[java.io.Serializable,Algebra.Winner[Algebra.Player]] = \/-(Winner(X))
```

&nbsp;

take back on the board with 7 moves
```
scala> m7 map (takeBack(_))
res11: scalaz.\/[String,Algebra.Board[Previous.previous7.NewS,Previous.previous7.NewM]] =
\/-(
      O  _  _
      _  X  O
      O  X  X

      Status: MayBeFinished with SixMoves)
```


