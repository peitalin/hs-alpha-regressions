

### Fama-French 3-factor regressions in Haskell

Needs a more flexible csv parsing implementation.
Currently parses .csv file by into record data types, using label names as
record fields.

Run:
cabal install
cabal run

$ ghci regression.hs
ghci> main

#### Cabal Dependencies
- hmatrix
- cassava
- statistics
- hscolour
- pretty-show


