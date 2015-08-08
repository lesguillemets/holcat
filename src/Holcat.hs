{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Data.Colour
import Highlight
import Config

holcat :: Config -> T.Text -> IO ()
holcat (Config dx dy s v Normal) txt = do
    forM_ (zip [0,dy..] $ T.lines txt) $
        \(y,l) -> do
            forM_ (zip [0,dx..] (T.unpack l)) $ \(x,c) -> do
                let (r,g,b) = toTrueColour . fromHSV $ (x+y,s,v)
                setFg (TermRGB r g b)
                putChar c
            clearHl
            putStrLn ""
    clearHl

main = do
    args <- getArgs
    case args of
        [f] -> TIO.readFile f >>= holcat defaultConfig
        _ -> putStrLn "not implemented yet"
