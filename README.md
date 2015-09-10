A Boggle solver.

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
or all lower case. The found words will match the case of
the dictionary.

