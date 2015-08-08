module Config where
data Config = Config {
            dhdx :: Double,
            dhdy :: Double,
            saturation :: Double,
            value :: Double,
            style :: Style
        }

data Style = Normal
           | Background

defaultConfig :: Config
defaultConfig = Config 5 5 1 1 Normal
