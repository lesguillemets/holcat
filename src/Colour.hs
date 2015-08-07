{-# LANGUAGE OverloadedStrings #-}
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Colour = RGB {_r :: Int, _g :: Int, _b :: Int}

data Hi = Hi256 { _fg :: Colour, _bg :: Colour }
        | None

toSeq :: Hi -> T.Text
toSeq None = T.pack "\027[0m"
toSeq (Hi256 (RGB fr fg fb) (RGB br bg bb)) = T.pack $
    "\027[38;2;" ++ (intercalate ";" . map show) [fr,fg,fb] ++ "m" ++
    "\027[48;2;" ++ (intercalate ";" . map show) [br,bg,bb] ++ "m"

main = do
    TIO.putStr . toSeq $ Hi256 (RGB 0 243 234) (RGB 34 123 0)
    TIO.putStr "こんにちは!"
    TIO.putStrLn . toSeq $ None
    TIO.putStrLn "こんにちは!"
