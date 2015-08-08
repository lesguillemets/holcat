{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Options.Declarative
import Control.Monad.IO.Class (liftIO)

import Data.Colour
import Highlight
import Config

holcat :: Config -> T.Text -> IO ()
holcat (Config dx dy s v style) txt =
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

holcatMain :: Flag "r" '["rainbow"] "bg/fg" "bg/fg" (Def "fg" String)
           -> Arg "filename" String
           -> Cmd "holcat!" ()
holcatMain bf fName = liftIO $
    TIO.readFile (get fName) >>= holcat config
    where
    config = case get bf of
        "bg" -> defaultBGConfig
        _ -> defaultConfig

main = run_ holcatMain
