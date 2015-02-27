import System.Environment
import qualified Data.Text as T

main = let
  toIntArray str    = map (\x -> read x :: Int) $ toStringArray str
  toStringArray str = map T.unpack $ T.split (==',') $ T.pack str
  maxSum list       = maximum $ map sum $ allSubLists list
  allSubLists list  = [take x sub | sub <- subLists list, x <- [1..(length sub)]]
  subLists list     = [drop y list | y <- [0..length list-1]]
  in do
    argv <- getArgs
    fileStr <- readFile $ head argv
    let samples = map toIntArray $ lines fileStr 
    mapM (print . maxSum) samples 

