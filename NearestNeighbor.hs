module NearestNeighbor
       (selectKBeforeLeak,selectKAfterLeak) where

import           Control.Arrow (second)
import           Control.Monad (join)
import           Control.Monad.ST (runST)
import           Data.IntMap (IntMap,Key)
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA (sortBy)
import qualified Data.Vector.Generic as G (unsafeThaw,unsafeFreeze)
import qualified Data.Vector.Unboxed as U

selectKBeforeLeak
  :: V.Vector (U.Vector Double)
  -> Int -> Int -> Int
  -> [(Int,Int,Double)]
selectKBeforeLeak vec len numK pt = if numK > 1
                          then neighbors
                          else error "Not enough neighbors"
  where
    neighbors = mkContexts . U.take numK . vsort $ U.map combLazy indices
    combLazy = metricSpaceDistBuild vec euclidDist pt
    indices = (U.++) (U.enumFromN 0 (pt-1)) (U.enumFromN (pt+1) (len-1-pt))
    mkContexts = U.toList . U.map (\(d,i) ->  seq i  seq pt seq d (pt,i,d))

selectKAfterLeak
  :: V.Vector (U.Vector Double)
  -> Int -> Int -> Int
  -> ([(Double,Int)],Int,(),[(Double,Int)])
selectKAfterLeak vec len numK pt = if numK > 1
                          then neighbors
                          else error "Not enough neighbors"
  where
    neighbors = mkContexts . U.take numK . vsort $ U.map combLazy indices
    combLazy = metricSpaceDistBuild vec euclidDist pt
    indices = (U.++) (U.enumFromN 0 (pt-1)) (U.enumFromN (pt+1) (len-1-pt))
    mkContexts = (\a -> (a,pt,(),a)) . U.toList

vsort :: U.Vector (Double,Int) -> U.Vector (Double,Int)
vsort v = runST $ do m <- G.unsafeThaw v
                     VA.sortBy (\x y -> if fst x == fst y then EQ else if fst x > fst y then GT else LT) m
                     G.unsafeFreeze m

metricSpaceDistBuild
  :: V.Vector (U.Vector Double)
  -> (U.Vector Double -> U.Vector Double -> Double)
  -> Int
  -> Int
  -> (Double,Int)
metricSpaceDistBuild vec symFunc idx1 idx2 = (dist' idx1 idx2,idx2)
  where
    dist' ind1 ind2 = if ind1 < ind2
                      then symFunc
                           (V.unsafeIndex vec ind1) (V.unsafeIndex vec ind2)
                      else symFunc
                           (V.unsafeIndex vec ind2) (V.unsafeIndex vec ind1)

euclidDist ::  U.Vector Double -> U.Vector Double -> Double
euclidDist xs ys = norm' xs + norm' ys - U.sum (dot' xs ys)
  where
    norm' = U.sum . join dot'
    dot' = U.zipWith (*)
