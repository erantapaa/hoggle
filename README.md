A Boggle solver written in Haskell.

Usage from the command line:

    hoggle-exe bs "./dicts/ODS6.txt" "SERS P A TG LIN E SE RS"

Either 16, 25 or 36 letters may be specified for a 4x4, 5x5  or 6x6
grid respectively.

Non-alpha characters in the baord are ignored.

Instead of letters, one may also use the values "best4", "best5"
and "best6" to specify the densest 4x4, 5x5 and 6x6 boards found
by Chuong Do:

    hoogle-exe bs "./dicts/ODS6.txt" best4

Other trie options:

    hoogle-exe trie "./dicts/ODS6.txt" ... - use Data.Trie

From ghci:

    stack ghci
    ghci> :l Run
    Run> showWords "./dicts/ods6.txt" best4x4

Notes:

- The dictionary files must have the words in
sorted order in order for the "bs" trie implementation
to work.

- The dictionary files may be either all upper case
or all lower case. The board will be converted to
match the case of the dictionary. The found words will
also match the case of the dictionary.

- See http://ai.stanford.edu/~chuongdo/boggle/ for info
on dense Boggle boards. The 4x4, 5x5 and 6x6 boards are
available in the `Run` module as `best4x4`, `best5x5` and `best6x6`.

