

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

Example to try in GHCI
```
let v = asColumn $ fromList [1..12]
let u = asColumn $ fromList [21..32]
qwer <- readRawCsv fileP
let y  = getKey qwer lmvtx
let x1 = getKey qwer market
let x2 = getKey qwer smb
let x3 = getKey qwer hml
let x0 = intercept qwer
let x = x0 |+| x1 |+| x2 |+| x3
let betas = liftA2 estimateBetas x y
let e = getResiduals <$> y <*> (predictYhat <$> x <*> betas)
let stdErrs      = fmap (asColumn . sqrt . takeDiag) $ liftA2 (varCovMatrix) x e
let stdErrs      = (asColumn . sqrt . takeDiag) <$> (varCovMatrix <$> x <*> e)
let whiteStdErrs = (asColumn . sqrt . takeDiag) <$> (robustVCV <$> x <*> e)
let tstats = liftA2 (/) betas whiteStdErrs
let df = regOut <$> betas <*> whiteStdErrs <*> tstats
df ! "Alpha"
df ! "HML"
let matrixToList = fmap (ZipList . toList . head . toColumns)
map matrixToList [betas, whiteStdErrs, tstats]
```
