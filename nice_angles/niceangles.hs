import System.Environment
import Data.List.Split
import Text.Printf

readInputs :: String -> IO [String]
readInputs filePath = do
  inputs <- readFile filePath
  return $ lines inputs

niceAngle :: String -> String
niceAngle s = intPart ++ "." ++ angleFractionalPart
  where [intPart,decPart] = splitOn "." s
        angleFractionalPart  = toAngle fullFractional
        fullFractional       = "0." ++ decPart

toAngle :: String -> String
toAngle s = minutesPart ++ "'" ++ secondsPart ++ "''"
  where minutesPart       = intPart minAngleStr
        secondsPart       = intPart scndAngleStr
        minAngleStr       = toAngleDecStr s
        scndAngleStr      = toAngleDecStr fractionalScndStr
        toAngleDecStr x   = printf "%023.20f" $ toDouble x * 60
        fractionalScndStr = "0." ++ fractionalPart minAngleStr
        intPart x         = head $ parts x
        fractionalPart x  = last $ parts x
        parts             = splitOn "."
        toDouble x        = read x :: Double

main = do
  args   <- getArgs
  inputs <- readInputs $ head args
  mapM (putStrLn . niceAngle) inputs
