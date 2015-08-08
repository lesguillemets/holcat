module Config where
data Config = Config {
            dhdx :: Double,
            dhdy :: Double,
            style :: Style
        }

data Style = Normal
           | Background
