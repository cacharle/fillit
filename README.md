# fillit

Fit Tetris pieces (tetrimino) in the smallest square possible.

Based on a 42 school project, you can find the project's definition in [subject.pdf](./subject.pdf).

The implementation uses [backtracking][1] on increasingly larger squares and stops when it finds one in which all the pieces can fit.

I first used a list of 2D positions to represent a tetrimino but switched to a bit wise representation in the end for speed.
That way you can check very quickly if a tetrimino is on top of an other with a bitwise AND or move a tetrimino around with bitwise shift.

### Ruff Benchmark:

| # of tetrimino    | List of positions | Bitwise representation |
|-------------------|-------------------|------------------------|
| 9                 | 9s                | 0.6s                   |
| 10                | 32s               | 1s                     |
| 11                | too long          | 54s                    |

As you can see the bitwise representation brings significant improvement.

## Usage

```
$ cat examples/subject1.fillit
....
##..
.#..
.#..

....
####
....
....

#...
###.
....
....

....
##..
.##.
....

$ stack run < examples/subject1.fillit
DDDD
CCBB
ACCB
AAAB

# where each letter is a tetrimino
```

## TODO

- [ ] Tetrimino ID according to the order they are in the file
- [ ] Benchmarking (http://www.serpentine.com/criterion/tutorial.html)
- [ ] Print the algorithm's steps in real time
- [x] Optimized with bitwise operation
- [x] haskell stylish formatter
- [ ] neovim haskell language server
- [ ] Unit tests
- [ ] Property tests (with quickcheck)

[1]: https://en.wikipedia.org/wiki/Backtracking
