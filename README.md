## scm-sudoku
Solves Sudoku puzzles

Based on [this write-up by Peter Norvig](http://norvig.com/sudoku.html).
This is a pretty straight-forward translation of Peter's Python code into
Scheme. I tried to keep variable and function names as similar as possible
to the Python names. I wanted to use symbols for the hash keys but digits
by themselves are not valid symbols so in the interest of keeping the code
somewhat similar to the original, I decided to use strings for the keys.

## Example

```lisp
(display-grid (solve grid2))
```