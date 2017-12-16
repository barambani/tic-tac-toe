# Tic Tac Toe


Possible Scala solution of the Tic Tac Toe challenge proposed in the [Tony Morris' fpcourse](https://github.com/data61/fp-course/blob/master/projects/TicTacToe/TicTacToe.markdown)

### Api Example
start a new game
```
scala> val board_0 = startGame
board_0: Algebra.Board[Algebra.NotStarted,Algebra.NoMoves] =

      _  _  _
      _  _  _
      _  _  _

      Status: not started with no moves
 ```

&nbsp;

first move:
```
scala> val board_1 = board_0.afterMove(Tile22, X)
board_1: scalaz.\/[String,Algebra.Board[Next.noMoves.NewS,Next.noMoves.NewM]] =
\/-(
      _  _  _
      _  X  _
      _  _  _

      Status: in play with one move)
```

&nbsp;

second move:
```
scala> val board_2 = board_1.afterMove(Tile23, O)
board_2: scalaz.\/[String,Algebra.Board[Next.oneMove.NewS,Next.oneMove.NewM]] =
\/-(
      _  _  _
      _  X  O
      _  _  _

      Status: in play with two moves)
```

&nbsp;

whoWon for impossible scenario:
```
scala> board_2.whoWon
<console>:22: error: could not find implicit value for parameter W: WhoWon[Next.oneMove.NewS]
       board_2.whoWon
               ^
```

&nbsp;

winning move:
```
scala> val board_7 = board_6.afterMove(Tile32, X)
m7: scalaz.\/[String,Algebra.Board[Next.sixMoves.NewS,Next.sixMoves.NewM]] =
\/-(
      O  X  _
      _  X  O
      O  X  X

      Status: may be finished with seven moves)
```

&nbsp;

whoWon on possible scenario with winning player
```
scala> board_7.whoWon
res1: scalaz.\/[String,WhoWon.whoWon5.R] = \/-(\/-(Winner(X)))
```

&nbsp;

take back on the board with 7 moves
```
scala> board_7.takeBack
res2: scalaz.\/[String,Algebra.Board[Previous.previous7.NewS,Previous.previous7.NewM]] =
\/-(
      O  X  _
      _  X  O
      O  _  O

      Status: may be finished with six moves)
```


