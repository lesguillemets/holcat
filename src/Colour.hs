{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


class Colour a where
    toCode :: a -> BC.ByteString
    setFg :: a -> IO ()
    setBg :: a -> IO ()

data RGB = RGB {_r :: Int, _g :: Int, _b :: Int}

instance Colour RGB where
    toCode (RGB r g b) =  (BC.intercalate ";" . map (BC.pack . show)) [r,g,b]
    setFg c = BC.putStr $ BC.concat ["\027[38;2;", toCode c, "m"]
    setBg c = BC.putStr $ BC.concat ["\027[48;2;", toCode c, "m"]

data Hi = Hi256 { _fg :: RGB, _bg :: RGB }
        | None

setHighlight :: Hi -> IO ()
setHighlight None = BC.putStr "\027[0m"
setHighlight (Hi256 fg bg) = do
    setFg fg; setBg bg

main = do
    setHighlight $ Hi256 (RGB 0 243 234) (RGB 34 123 0)
    TIO.putStr "こんにちは!"
    setHighlight None
    TIO.putStrLn "こんにちは!"
