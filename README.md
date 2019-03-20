

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
```haskell
-- Read in data from ff3.csv
data <- readRawCsv fileP
let y  = getKey data lmvtx
let x1 = getKey data market
let x2 = getKey data smb
let x3 = getKey data hml
let x0 = intercept data

-- Append columns into a matrix with |+| operator
let x = x0 |+| x1 |+| x2 |+| x3

-- Estimate OLS with pseudo inverse
let betas = liftA2 estimateBetas x y
-- Calculate residuals and error corrections
let e = getResiduals <$> y <*> (predictYhat <$> x <*> betas)
let stdErrs      = fmap (asColumn . sqrt . takeDiag) $ liftA2 (varCovMatrix) x e
let stdErrs      = (asColumn . sqrt . takeDiag) <$> (varCovMatrix <$> x <*> e)
let whiteStdErrs = (asColumn . sqrt . takeDiag) <$> (robustVCV <$> x <*> e)
let tstats = liftA2 (/) betas whiteStdErrs

-- Display regression results
let df = regOut <$> betas <*> whiteStdErrs <*> tstats
-- Extract display data out with ! operator
df ! "Alpha"
df ! "HML"
let matrixToList = fmap (ZipList . toList . head . toColumns)
map matrixToList [betas, whiteStdErrs, tstats]

```
