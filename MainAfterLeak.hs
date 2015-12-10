{-#LANGUAGE FlexibleContexts#-}
import           Control.Arrow ((&&&))
import           Data.Graph.Inductive
import           Data.Graph.Inductive.PatriciaTree as GP
import           Data.Function (on)
import           Data.List (sort,transpose,sortOn)
import qualified Data.Vector.Storable as S
import           GraphBuilder
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel
import           NearestNeighbor
import           System.Environment
import           System.Random
import           Utils
import           Control.Monad (join)
import           Debug.Trace

main :: IO ()
main = do
  -- args <- getArgs
  -- file <- checkFile args
  -- let nbds = checkArgs args
  -- I've just hardcoded these values for easier testing
  let file = "swissroll.dat"
      nbds = (300,100)

  matrix' <- getReqMatrix file

  let dataset = convertDataset matrix'
      rowsize = rowSize matrix'
      f       = selectKAfterLeak dataset rowsize
      arr     = map (f (fst nbds)) [0..100::Int]
      g v'    = map snd . sortOn fst . tail . join . map (take 1 . unLPath) . flip spTree v'
      v       = foldr (&) (empty :: GP.Gr () Double) arr
      -- v       = buildGraph arr (f (fst nbds))

  print . checkSymRowsCols . createAdjacencyList (konst 0 (rowsize,rowsize) :: Matrix Double) 101 $ g v

checkSymRowsCols :: (Container Vector a, Eq a, S.Storable a) => Matrix a -> Bool
checkSymRowsCols = uncurry (==) . (map sumElements . toColumns &&& map sumElements . toRows)

createAdjacencyList
  :: Matrix Double
  -> Int
  -> (Int -> [Double])
  -> Matrix Double
createAdjacencyList mat size' g' = fst $ mutable ( \ (_,_) x ->
                                                  mapM_ ((\ (n',l) -> do
                                                          setMatrix x n' (n'+1) (row l)
                                                          setMatrix x (n'+1) n' (col l)) . (\x' -> (x',g' x')))
                                                        [0..size'-1]) mat
