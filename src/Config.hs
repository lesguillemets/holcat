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

defaultConfig :: Config
defaultConfig = Config 5 5 1 1 Normal

defaultBGConfig :: Config
defaultBGConfig = Config 5 5 1 1 (Background (0,0,0))
