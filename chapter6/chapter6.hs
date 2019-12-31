{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.List
import qualified Data.Map as M

class Vector v where
  distance :: v -> v -> Double

instance Vector (Double, Double) where
  distance (a, b) (c, d) = sqrt $ (c - a) * (c - a) + (b - d) * (b - d)

class Vector v => Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

-- kMeans :: (Vector v, Vectorizable e v)
--        => (Int -> [e] -> [v]) -- number of centroids
--        -> [e] -- the information
--        -> [v] -- centroids after convergence

clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v)
                       => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
    in foldr (\p m -> let chosenC = minimumBy (compareDistance p) centroids
                       in M.adjust (p:) chosenC m)
            initialMap points
    where compareDistance p x y = compare (distance x $ toVector p)
                                          (distance y $ toVector p)




