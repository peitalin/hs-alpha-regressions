
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveGeneric             #-}

import           Colour.ColourGHCI
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector 				as V
import           Data.Csv
import           Data.List
import  		 Data.String 				-- IsString type
import 			 Foreign.Storable			-- Storable type
import 			 GHC.Generics
import 			 Control.Applicative
import           Numeric.LinearAlgebra
-- import           Control.Lens hiding ((<.>))
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
        x = x0 |+| x1 |+| x2 |+| x3 -- append Either matrix columns (design matrix)

        -- OLS
        betas = liftA2 estimateBetas x y
        e     = residuals x y betas
        stdErrs      = fmap (sqrt . takeDiag) $ liftA2 (varCovMatrix) x e
        whiteStdErrs = robustVCV <$> x <*> e >>= return . sqrt . takeDiag
        -- whiteStdErrs'  = (sqrt . takeDiag) <$> (robustVCV <$> x <*> e)

    pprint "Returns & Fama French Factors"
    readRawCsv fileP >>= pprint . (sliceData 5 2)
    pprint "GLS Regression Output"
    pprint $ regOut <$> betas <*> whiteStdErrs
    pprint "Original standard errors:"
    pprint stdErrs




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
-- type CsvData = V.Vector (V.Vector BL.ByteString)

readRawCsv :: FilePath -> IO (Either ErrorMsg [Returns])
readRawCsv fileName = fmap parseCSV $ BL.readFile fileName
	where parseCSV = fmap (V.toList . snd) . decodeByName

getKey :: (Storable a, Functor f) => f [a1] -> (a1 -> a) -> f (Matrix a)
getKey csvData key = (asColumn . fromList . map key) <$> csvData

sliceData :: Functor f => Int -> Int -> f [a] -> f (V.Vector a)
sliceData startRow rowsDown = fmap ((V.slice startRow rowsDown) . V.fromList)



-- OLS Functions
intercept :: Functor f => f [a] -> f (Matrix Double)
intercept covar = (asColumn . constant 1) <$> length <$> covar


estimateBetas :: (Mul Matrix a b, Scalar c) => Matrix c -> a c -> b c
estimateBetas x y = pinv(x) <> y
-- estimateBetas x y = inv (x' <> x) <> (x' <> y) -- OLS Normal Equations


residuals :: (Mul a b c, Product t, Applicative f, Num (c t)) => f (a t) -> f (c t) -> f (b t) -> f (c t)
residuals x y betas = (-) <$> y <*> (yhat)
	where yhat = (<>) <$> x <*> betas


degreesFree :: Num a => [b] -> [b] -> a
degreesFree residuals predictors = (genericLength residuals) - (genericLength predictors)


varCovMatrix :: (Num (Vector a), Scalar a) => Matrix a -> Matrix a -> Matrix a
varCovMatrix x e = sigma * inv (trans x <> x)
    where sigma  = trans e <> e / degreesFree (toRows e) (toColumns x)
				-- e'e / (n-k)


robustVCV :: (Num (Vector a), Scalar a) => Matrix a -> Matrix a -> Matrix a
robustVCV x e = (inv(x'<>x))  <>  x'<>omega<>x  <>  (inv(x'<>x))  -- Sandwich Estimator
    where omega = diag $ takeDiag (e <> e')
          x'    = trans x
          e'    = trans e


-- regOut :: (Storable d, Storable b, IsString c, IsString a) => Matrix b -> Vector d -> [(String, (a, b, c, d))]
regOut betas stdErrs = do
    zip p $ zip4 c (toList b') s (toList stdErrs)
    where c = (replicate 4 "Coefficient")
          s = (replicate 4 "Std. Error")
          p = words "Alpha Market SMB HML"
          b' = ((toColumns betas)!!0) -- ((!!) b 0)



-- Define infix operators for Monadic Matrix Appending
infixl 3 |&|
a |&| b = fromBlocks [[a, b]] :: Matrix Double

infixl 3 |+|
c |+| d = liftA2 (|&|) c d







{-
--GHCI only

let v = asColumn $ fromList [1..12]
let u = asColumn $ fromList [21..32]

qwer <- readRawCsv fileP
let y  = getKey qwer lmvtx
let x1 = getKey qwer market
let x2 = getKey qwer smb
let x3 = getKey qwer hml
let x0 = intercept qwer
-- let x = liftA3 (\x y z -> fromColumns [x, y, z]) x1 x2 x3
let x = x0 |+| x1 |+| x2 |+| x3
let betas = liftA2 estimateBetas x y
let e = residuals x y betas

let stdErrs      = fmap (sqrt . takeDiag) $ liftA2 (varCovMatrix) x e
let stdErrs      = (sqrt . takeDiag) <$> (varCovMatrix <$> x <*> e)
let whiteStdErrs = robustVCV <$> x <*> e >>= return . sqrt . takeDiag
regOut <$> betas <*> whiteStdErrs

-- Using lens
import Control.Lens
let yy1 = fmap (fmap lmvtx) qwer
let xx1 = fmap (fmap market) qwer
let xx2 = fmap (fmap smb) qwer
let xx3 = fmap (fmap hml) qwer
let csvdata = liftA3 (\a b c -> zip3 a b c) xx1 xx2 xx3

let y  = (asColumn . fromList) <$> yy1
let x1 = fmap (asColumn . fromList . fmap (^._1)) csvdata
let x2 = fmap (asColumn . fromList . fmap (^._2)) csvdata
let x3 = fmap (asColumn . fromList . fmap (^._3)) csvdata
let x0 = Right $ asColumn (constant 1 51 :: Vector Double)

let x  = x0 |+| x1 |+| x2 |+| x3
let x  = foldl (|+|) x0 [x1, x2, x3]
let betas = liftA2 (\x y -> pinv x <> y) x y


let yhat = liftA2 (<>) x betas
let e = residuals x y betas -- y - yhat
let stdErrs       = (sqrt . takeDiag) <$> (varCovMatrix <$> x <*> e)
let whiteStdErrs  = robustVCV <$> x <*> e >>= return . sqrt . takeDiag
regOut <$> betas <*> whiteStdErrs

-}
