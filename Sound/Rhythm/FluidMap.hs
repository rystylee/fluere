module Sound.Rhythm.FluidMap where


data FluidMap = FluidMap { waveLength :: Double, waveAmp :: Double, waveCurve :: WaveCurve }
                deriving (Show)

data WaveCurve = Sin | Lin | Exp | Step
                  deriving (Show)
