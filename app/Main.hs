module Main where
import Lib
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as BB
import Data.Foldable as F
import System.Process
import Text.Printf
outputFilePath :: FilePath
outputFilePath = "output.bin"
sampleRate :: Float
sampleRate = 48000.0
volume :: Float
volume = 0.5
wave :: [Float]
wave = map (* volume) $ map sin $ map (* step) [0.0 .. sampleRate * duration]
  where step = 0.05
        duration = 4
save :: IO ()
save = B.writeFile outputFilePath $ BB.toLazyByteString $ F.fold $ map BB.floatLE wave

main :: IO ()
main = do
  save
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()
