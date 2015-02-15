
-- Local
{-# LANGUAGE NoMonomorphismRestriction #-}

module Colour.ColourGHCI where


import qualified Text.Show.Pretty as Pr
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour

myColourPrefs :: HsColour.ColourPrefs
myColourPrefs = HsColour.defaultColourPrefs { HsColour.conid = [HsColour.Foreground HsColour.Yellow, HsColour.Bold], HsColour.conop = [HsColour.Foreground HsColour.Yellow], HsColour.string = [HsColour.Foreground HsColour.Green], HsColour.char = [HsColour.Foreground HsColour.Cyan], HsColour.number = [HsColour.Foreground HsColour.Red, HsColour.Bold], HsColour.layout = [HsColour.Foreground HsColour.White], HsColour.keyglyph = [HsColour.Foreground HsColour.White] }


pprint :: Show a => a -> IO ()
pprint = putStrLn . HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) myColourPrefs False False "" False . Pr.ppShow

cprint :: Show a => a -> IO ()
cprint = putStrLn . HsColour.hscolour (HsColour.TTYg HsColour.XTerm256Compatible) myColourPrefs False False "" False . show