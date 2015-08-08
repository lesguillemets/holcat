{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Highlight

main = do
    setHighlight $ Hi256 (TermRGB 0 243 234) (TermRGB 34 123 0)
    TIO.putStr "こんにちは!"
    setHighlight None
    TIO.putStrLn "こんにちは!"
    mapM_ (\c -> do
          setBg (Term256 c)
          putChar ' ') [0..255]
    setHighlight None
    putStrLn ""
    mapM_ (\c -> do
          setBg (Term8 c)
          putChar ' ') [0..7]
    setHighlight None
    putStrLn ""
