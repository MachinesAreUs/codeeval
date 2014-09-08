import System.Environment
import System.IO
import Data.Char
import Data.List
import Data.Maybe
import qualified Numeric.Matrix as M

-- Main Definitions

type Value = Int

data Labyrinth = Labyrinth { matrix :: M.Matrix Value
                           , steps :: [(Int,Int)]
                           } deriving (Show)

empty = 0
wall  = 1
step  = 2

-- Algorithm

neighbors :: (Int, Int) -> (Int, Int) -> [(Int,Int)] 
neighbors (rows,cols) (r,c) = 
  [(r,c-1),(r,c+1),(r-1,c),(r+1,c)] |> 
    filter (\(r,c) -> r >= 1 && r <= rows && c >= 1 && c <= cols)

possibleMoves :: Labyrinth -> (Int,Int) -> [(Int,Int)]
possibleMoves lab pos =
  neighbors (dimensions lab) pos |> filter isEmpty
  where isEmpty p = at lab p == empty

allSolutions :: Labyrinth -> [Labyrinth]
allSolutions x = firstSolution x |> derivedSolutions

firstSolution :: Labyrinth -> Labyrinth
firstSolution s = Labyrinth {matrix = newMatrix, steps = [(1,fstCol)] } 
  where (rows,cols) = dimensions s
        fstCol      = firstRow s |> elemIndex empty |> fromJust |> (+) 1
        newMatrix   = changeValue (matrix s) (1,fstCol) step

derivedSolutions :: Labyrinth -> [Labyrinth]
derivedSolutions lab = 
  if reachedBottom
  then [lab]
  else last st |> possibleMoves lab 
               |> map newPath 
               |> concatMap derivedSolutions
  where 
    reachedBottom = steps lab |> last |> fst == rows 
    (rows,cols)   = dimensions lab
    st            = steps lab
    newPath (x,y) = Labyrinth { matrix = changeValue (matrix lab) (x,y) step
                              , steps = st ++ [(x,y)]
                              }

solutionLength :: Labyrinth -> Int
solutionLength lab = steps lab |> length

main = do
  args <- getArgs
  lab  <- readLabyrinth $ head args
  let solutions = allSolutions lab
      shortest = minimumBy (compareBy solutionLength) solutions
  printLabyrinth shortest

-- Matrix implementation specific plumbing

toIntChar :: Char -> Char
toIntChar ' ' = '0'
toIntChar '*' = '1'
toIntChar '+' = '2'
toIntChar c   = c

toMapChar :: Char -> Char
toMapChar '0' = ' '
toMapChar '1' = '*'
toMapChar '2' = '+'
toMapChar c = c

readLabyrinth :: String -> IO Labyrinth
readLabyrinth filePath = do
  str <- readFile filePath
  let rows = map toIntChar str |> lines 
                               |> map (map digitToInt) 
      mtx  = M.fromList rows :: M.Matrix Int
  return Labyrinth {matrix = mtx, steps = []}

printLabyrinth lab = 
  matrix lab |> show 
             |> stripChars " "
             |> map toMapChar 
             |> putStr

at :: Labyrinth -> (Int,Int) -> Value
at lab = M.at (matrix lab)

firstRow :: Labyrinth -> [Value]
firstRow lab = matrix lab |> M.row 1 

dimensions :: Labyrinth -> (Int,Int)
dimensions lab = matrix lab |> M.dimensions 

changeValue :: M.Matrix Int -> (Int,Int) -> Int -> M.Matrix Int
changeValue m (r,c) v = M.mapWithIndex mapper m
  where mapper (row,col) e = if row == r && col == c then v else e

-- General purpose stuff

(|>) = flip ($)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

compareBy :: Ord a => (b -> a) -> b -> b -> Ordering
compareBy fn l1 l2 =  compare (fn l1) (fn l2) 

