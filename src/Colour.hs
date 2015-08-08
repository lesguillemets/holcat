{-# LANGUAGE OverloadedStrings #-}
module Colour where
import qualified Data.ByteString.Char8 as BC

class Colour a where
    toCode :: a -> BC.ByteString
    setFg :: a -> IO ()
    setBg :: a -> IO ()

data TermRGB = TermRGB {_r :: Int, _g :: Int, _b :: Int}

instance Colour TermRGB where
    toCode (TermRGB r g b) = (BC.intercalate ";" . map (BC.pack . show)) [r,g,b]
    setFg c = BC.putStr $ BC.concat ["\027[38;2;", toCode c, "m"]
    setBg c = BC.putStr $ BC.concat ["\027[48;2;", toCode c, "m"]

data Term256 = Term256 {_t :: Int}
instance Colour Term256 where
    toCode  = BC.pack . show . _t
    setFg c = BC.putStr $ BC.concat ["\027[38;5;", toCode c, "m"]
    setBg c = BC.putStr $ BC.concat ["\027[48;5;", toCode c, "m"]

data Term8 = Term8 Int
instance Colour Term8 where
    toCode = const ""
    setFg (Term8 i) = BC.putStr $ BC.concat [
            "\027[", (BC.pack . show) (30+i), "m"]
    setBg (Term8 i) = BC.putStr $ BC.concat [
            "\027[", (BC.pack . show) (40+i), "m"]


data Hi = Hi256 { _fg :: TermRGB, _bg :: TermRGB }
        | None

setHighlight :: Hi -> IO ()
setHighlight None = BC.putStr "\027[0m"
setHighlight (Hi256 fg bg) = do
    setFg fg; setBg bg
