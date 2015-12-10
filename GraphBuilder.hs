{-#LANGUAGE BangPatterns#-}
module GraphBuilder
       (buildGraph)
       where

import           Data.Graph.Inductive
import qualified Data.Graph.Inductive.PatriciaTree as GP
-- import qualified Data.Vector.Unboxed as U
import Debug.Trace
import Data.List (foldl')
-- import Data.IntMap (IntMap)
-- import           Data.IntMap.Strict as IntMap hiding (empty)
-- import           Data.List (foldl')

buildGraph
  :: [Int]
  -> (Int -> [(Int,Int,Double)])
  -> GP.Gr () Double
buildGraph nodes' getEdges = let nodes'' = insNodes (zip nodes' $ repeat ()) empty
                             in foldl' (\g v -> let e = getEdges v
                                                in e `insEdges` g) nodes'' nodes'

-- buildGraph
--   :: (Int -> (IntMap [Double],Int,IntMap [Double]))
--   -> [Int]
--   -> GP.Gr () Double
-- buildGraph nearest = foldl' (\g x -> let val = nearest x
--                                          in (&) val g) empty
-- buildGraph nearest = Prelude.foldr (\x g -> deepseq g $ (&) (nearest x) g) empty
-- buildGraph nearest = foldr ((&) . nearest) empty

-- capTrees :: GP.Gr () Double -> Int -> [Int] -> [Double]
-- capTrees graph pt = Prelude.map (\x -> flip3 G.spLength graph x pt)
--   where -- The last argument is nearest
--    flip3 f z y x = f x y z
