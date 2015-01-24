
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}

import           Colour.ColourGHCI
import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector                as V
import qualified Data.Map
import           Data.Csv
import           Data.List
import           Data.String                -- IsString type
import           Foreign.Storable           -- Storable type
import           GHC.Generics
import           Numeric.LinearAlgebra
import           Statistics.Distribution    as SD
import           Statistics.Distribution.Normal as SDN
-- WARNING: Methods from Data.Vector and Numeric.LinearAlgebra.Vector are different!
-- E.g: V.toList and NLA.toList


main :: IO ()
main = do
    csvdata <- readRawCsv fileP
    -- Right csvdata <- readRawCsv fileP -- Unwraps Either layer for entire block
    let y  = getKey csvdata lmvtx
        x1 = getKey csvdata market
        x2 = getKey csvdata smb
        x3 = getKey csvdata hml
        x0 = intercept csvdata
        x = x0 |+| x1 |+| x2 |+| x3 -- append (Either matrix) columns

        -- OLS
        betas     = estimateBetas <$> x <*> y
        yhats     = predictYhat   <$> x <*> betas
        errs      = getResiduals  <$> y <*> yhats
        stdErrs   = getStdErrs varCovMatrix <$> x <*> errs
        whiteErrs = getStdErrs robustVCV    <$> x <*> errs
        tstats    = liftA2 (/) betas whiteErrs
        pvals     = getPvalues <$> tstats

    putStrLn "\n\tLMVTX Excess Returns & Fama French Factors (view: 5)"
    readRawCsv fileP >>= pprint . (sliceData 1 5)
    putStrLn "\n\tGLS Regression output"
    let output = regOut <$> betas <*> whiteErrs <*> tstats <*> pvals
    pprint output
    putStrLn "\n\tEstimated Alphas on LMVTX"
    pprint $ output ! "Alpha"




-- CSV Parsing Functions
fileP :: FilePath
fileP = "ff3.csv"

data Returns = Returns
	{ date 		:: String
	, lmvtx	 	:: Double
	, market 	:: Double
	, smb 		:: Double
	, hml 		:: Double
	} deriving (Generic, Show)

instance FromNamedRecord Returns
instance ToNamedRecord Returns

type Scalar a = Numeric.LinearAlgebra.Field a
type ErrorMsg = String

readRawCsv :: FilePath -> IO (Either ErrorMsg [Returns])
readRawCsv fileName = fmap parseCSV $ BL.readFile fileName
	where parseCSV = fmap (V.toList . snd) . decodeByName

getKey :: (Storable a, Functor f) => f [a1] -> (a1 -> a) -> f (Matrix a)
getKey csvData key = (asColumn . fromList . map key) <$> csvData

intercept :: Functor f => f [a] -> f (Matrix Double)
intercept covar = (asColumn . constant 1) <$> length <$> covar

sliceData :: Functor f => Int -> Int -> f [a] -> f (V.Vector a)
sliceData startRow rowsDown = fmap ((V.slice startRow rowsDown) . V.fromList)



-- OLS Functions
estimateBetas :: (Mul Matrix a b, Scalar c) => Matrix c -> a c -> b c
estimateBetas x y = pinv(x) <> y
-- estimateBetas x y = inv (x' <> x) <> (x' <> y) -- OLS Normal Equations


predictYhat :: (Mul a b c, Product t) => a t -> b t -> c t
predictYhat x betas = x <> betas


getResiduals :: Num a => a -> a -> a
getResiduals y yhat = y - yhat


varCovMatrix :: (Num (Vector a), Scalar a) => Matrix a -> Matrix a -> Matrix a
varCovMatrix x e = sigma * inv (trans x <> x)
    where sigma  = trans e <> e / degreesFree (toRows e) (toColumns x) -- e'e / (n-k)
          degreesFree errors predictors = (genericLength errors) - (genericLength predictors)


robustVCV :: (Num (Vector a), Scalar a) => Matrix a -> Matrix a -> Matrix a
robustVCV x e = (inv(x'<>x))  <>  x'<>omega<>x  <>  (inv(x'<>x))  -- Sandwich Estimator
    where omega = diag $ takeDiag (e <> e')
          x'    = trans x
          e'    = trans e


getStdErrs :: (Element t, Floating (Vector t)) => (a -> b -> Matrix t) -> a -> b -> Matrix t
getStdErrs varCovFn x e = (asColumn . sqrt . takeDiag) (varCovFn x e)


getPvalues :: Matrix Double -> Matrix Double
getPvalues tstats = mapMatrix getPval tstats
    where getPval = \t -> 2*(1 - cumulative (normalDistr 0 1) (abs t))


-- Regression Output
type HashMap = Data.Map.Map
type ZipOutput a d = HashMap ErrorMsg ((a,d), (a,d), (a,d), (a,d))

regOut :: (Element d, IsString a) => Matrix d -> Matrix d -> Matrix d -> Matrix d -> ZipOutput a d
regOut betas stdErr tstats pvals = do
    Data.Map.fromList $ zip (words "Alpha Market SMB HML") $ regOutput
    where matrixToList = (toList . head . toColumns)
          numParams = length (matrixToList betas)
          regOutput = zip4 (zip (replicate numParams "Coefficient") (matrixToList betas))
                           (zip (replicate numParams "Std. Error") (matrixToList stdErr))
                           (zip (replicate numParams "t-Statistic") (matrixToList tstats))
                           (zip (replicate numParams "p-value") (matrixToList pvals))
-- OUTPUT: Ideally find a way to format and print in table


-- Define infix operators for Monadic Matrix Appending
infixl 3 |&|
a |&| b = fromBlocks [[a, b]] :: Matrix Double

infixl 3 |+|
c |+| d = liftA2 (|&|) c d

infixl 3 !
hashmap ! key = (Data.Map.lookup key) <$> hashmap






{-
-- GHCI only

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

-}
