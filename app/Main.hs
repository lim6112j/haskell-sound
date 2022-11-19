module Main where
import Lib
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as BB
import Data.Foldable as F
import System.Process
import Text.Printf
import Data.List
type Seconds = Float
type Samples = Float
type Hz = Float
type Pulse = Float
type Semitones = Float
type Beats = Float
outputFilePath :: FilePath
outputFilePath = "output.bin"
sampleRate :: Float
sampleRate = 48000.0
volume :: Float
volume = 0.5
pitchStandard :: Float
pitchStandard = 440.0
bpm :: Beats
bpm = 120.0
beatDuration :: Seconds
beatDuration = 60.0 / bpm
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
  where step = (hz * 2 * pi) / sampleRate
        attack :: [Pulse]
        attack = map (min 1.0) [ 0.0, 0.001 ..]
        release :: [Pulse]
        release = reverse $ take (length output) attack
        output = map sin $ map (* step) [0.0 .. sampleRate * duration]

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

wave :: [Pulse]
wave = concat $ [ note 0 0.5 | i <- [1 .. 10] ]
  where duration = 1.0
save :: IO ()
save = B.writeFile outputFilePath $ BB.toLazyByteString $ F.fold $ map BB.floatLE wave

main :: IO ()
main = do
  save
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()
