{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Data.Colour
import Highlight
import Config

holcat :: Config -> T.Text -> IO ()
holcat (Config dx dy s v style) txt = do
    forM_ (zip [0,dy..] $ T.lines txt) $
        \(y,l) -> do
            forM_ (zip [0,dx..] (T.unpack l)) $ \(x,c) -> do
                let (r,g,b) = toTrueColour . fromHSV $
                        (fromIntegral $ floor (x+y) `mod` 360,s,v)
                case style of
                    Normal -> setFg (TermRGB r g b)
                    (Background (rb,gb,bb)) -> do
                        setFg (TermRGB rb gb bb)
                        setBg (TermRGB r g b)
                putChar c
            clearHl
            putStrLn ""

main = do
    args <- getArgs
    case args of
        [f] -> TIO.readFile f >>= holcat defaultBGConfig
        _ -> putStrLn "not implemented yet"
