{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Colour

main = do
    setHighlight $ Hi256 (RGB 0 243 234) (RGB 34 123 0)
    TIO.putStr "こんにちは!"
    setHighlight None
    TIO.putStrLn "こんにちは!"
