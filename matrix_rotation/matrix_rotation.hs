import System.Environment
import Data.List

type Matrix = [[Char]]

toMtx :: String -> Matrix
toMtx s = [[ elem row col | col <- [0..size-1]] | row <- [0..size-1]]
  where chars = filter (/=' ') s
        size  = truncate $ sqrt $ fromIntegral $ length chars
        elem r c = head $ drop (r * size + c) chars

toString :: Matrix -> String
toString m = intersperse ' ' $ concat m

rotateLeft :: Matrix -> Matrix
rotateLeft m = reverse $ transpose m

rotateRight :: Matrix -> Matrix
rotateRight = rotateLeft . rotateLeft . rotateLeft

main = do
  argv <- getArgs
  fileStr <- readFile $ head argv
  let samples = map toMtx $ lines fileStr 
  mapM (putStrLn . toString . rotateRight) samples 
