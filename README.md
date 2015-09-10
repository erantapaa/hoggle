A Boggle solver written in Haskell.

Usage from the command line:

    hoggle-exe "./dicts/ods6.txt" "SERS P A TG LIN E SE RS"

Either 16 or 25 letters may be specified for a 4x4 or 5x5 grid.
Only alpha characters are used.

From ghci:

    stack ghci
    ghci> :l Run
    Run> showWords "./dicts/ods6.txt" best4x4

Notes:

- The dictionary files may be either all upper case
or all lower case. The board will be converted to
match the case of the dictionary. The found words will
also match the case of the dictionary.

- See http://ai.stanford.edu/~chuongdo/boggle/ for info
on dense Boggle boards. The 4x4, 5x5 and 6x6 boards are
available in the `Run` module as `best4x4`, `best5x5` and `best6x6`.

