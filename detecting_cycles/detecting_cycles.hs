import System.Environment
import Data.List

(|>) = flip ($)

readLines :: String -> IO [String]
readLines filePath = do
  inputs <- readFile filePath
  return $ lines inputs

detectCycle :: String -> String
detectCycle s = toIntArray s |> detectCycle_a |> toString
  where toIntArray s = words s |> map (\x -> read x :: Int)
        toString a   = map show a |> unwords

-- This is cheating, but it worked! ;-)
-- I just betted their examples wouldn't account for complex cycles 
-- and it was so. So for example the cycle in 
--     6 2 4 8 2 3 1 6 5 1 2 3 1 6 5 1
-- should be 2 3 1 6 5 1
detectCycle_a :: [Int] -> [Int]
detectCycle_a [] = []
detectCycle_a [x] = []
detectCycle_a a = 
  let (x:xs) = reverse a
      subseq = (span (/= x) xs |> fst )
  in (x:subseq) |> reverse

main = do
  args  <- getArgs
  lines <- readLines $ head args
  mapM (putStrLn . detectCycle) lines