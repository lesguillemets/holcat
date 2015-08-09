{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Config where
import Data.Colour
data Config = Config {
            dhdx :: Double,
            dhdy :: Double,
            saturation :: Double,
            value :: Double,
            style :: Style
        }

data Style = Normal
           | Background {fgConfig :: TrueColour}
           | BackgroundFull {_fgbf :: TrueColour}

defaultConfig :: Config
defaultConfig = Config 5 5 1 1 Normal

defaultBGConfig :: Config
defaultBGConfig = Config 5 5 1 1 (Background (0,0,0))

defaultBGFConfig :: Config
defaultBGFConfig = Config 5 5 1 1 (BackgroundFull (0,0,0))

-- TODO : nicer type-level handling
sanitise :: Config -> Config
sanitise = mkConfig . UnsafeConfig

newtype UnsafeConfig = UnsafeConfig Config


mkConfig :: UnsafeConfig -> Config
mkConfig (UnsafeConfig c@(Config {..})) =
    c { saturation = fitRange 0 1 saturation,
        value = fitRange 0 1 value,
        style = sanitiseStyle style
      }

sanitiseStyle :: Style -> Style
sanitiseStyle Normal = Normal
sanitiseStyle (Background (r,g,b)) =
    Background (f r, f g, f b) where f = fitRange 0 255
sanitiseStyle (BackgroundFull (r,g,b)) =
    BackgroundFull (f r, f g, f b) where f = fitRange 0 255

fitRange :: Ord a => a -> a -> a -> a
fitRange lBound uBound n
    | n < lBound = lBound
    | uBound < n = uBound
    | otherwise = n
