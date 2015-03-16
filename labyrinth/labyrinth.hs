import System.Environment
import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- Definitions

type Value = Char
type Matrix a = [[a]]

data Labyrinth = Labyrinth { matrix :: Matrix Value
                           , steps :: [(Int,Int)]
                           } deriving (Show)

empty = ' '
wall  = '*'
step  = '+'

-- Algorithm

neighbors :: (Int, Int) -> (Int, Int) -> [(Int,Int)] 
neighbors (rows,cols) (r,c) = 
  [(r,c-1),(r,c+1),(r-1,c),(r+1,c)] |> 
    filter (\(r,c) -> r >= 0 && r < rows && c >= 0 && c < cols)

possibleMoves :: Labyrinth -> (Int,Int) -> [(Int,Int)]
possibleMoves lab pos =
  neighbors (dimensions lab) pos |> filter isEmpty
  where isEmpty p = at lab p == empty

allSolutions :: Labyrinth -> [Labyrinth]
allSolutions x = firstSolution x |> derivedSolutions

firstSolution :: Labyrinth -> Labyrinth
firstSolution s = Labyrinth {matrix = newMatrix, steps = [(0,fstCol)] } 
  where (rows,cols) = dimensions s
        fstCol      = firstRow s |> elemIndex empty |> fromJust
        newMatrix   = changeValue (matrix s) (0,fstCol) step

derivedSolutions :: Labyrinth -> [Labyrinth]
derivedSolutions lab = 
  if reachedBottom
  then [lab]
  else last st |> possibleMoves lab 
               |> map newPath 
               |> concatMap derivedSolutions
  where 
    reachedBottom = steps lab |> last |> fst == rows - 1 
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

readLabyrinth :: String -> IO Labyrinth
readLabyrinth filePath = do
  str <- readFile filePath
  let rows = lines str
  return Labyrinth {matrix = rows, steps = []}

printLabyrinth lab = 
  matrix lab |> unlines |> putStr

at :: Labyrinth -> (Int,Int) -> Value
at lab (r,c) = (matrix lab !! r) !! c 

firstRow :: Labyrinth -> [Value]
firstRow lab = matrix lab |> head

dimensions :: Labyrinth -> (Int,Int)
dimensions lab = 
  let mtx = matrix lab
  in (length mtx, length (head mtx))

changeValue :: Matrix a -> (Int,Int) -> a -> Matrix a
changeValue mat (r,c) v = 
  let (upperRows, thisRow : lowerRows ) = splitAt r mat
      (leftCells, thisCell : rightCells) = splitAt c thisRow
  in upperRows ++ (leftCells ++ v : rightCells) : lowerRows

-- General purpose stuff

(|>) = flip ($)

compareBy :: Ord a => (b -> a) -> b -> b -> Ordering
compareBy fn l1 l2 = compare (fn l1) (fn l2) 


