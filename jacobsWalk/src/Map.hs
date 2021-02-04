{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Map where

import qualified Data.Map as M
import Control.Monad       (liftM, ap,mapM)
import qualified Data.Foldable as Foldable
import qualified AsMonad as AM 

newtype Dist a k = Dist{unDist:: M.Map k a}

empty :: Dist a k 
empty = Dist $ M.empty
mapEmpty = empty

singleton :: k -> a -> Dist k a 
singleton a k = Dist $ M.singleton k a
mapSingleton = singleton 1 'a'

fromList :: Ord k => [(k, a)] -> Dist a k 
fromList x = Dist $ M.fromList x 

assocs :: Dist a k -> [(k, a)] 
assocs x =  M.assocs (unDist x)

mMap :: (a -> b) -> Dist a k -> Dist b k 
mMap f x = Dist $ M.map f x' where
  x' = unDist x

multiply :: Num a => a -> Dist a b -> Dist a b
multiply v m = Dist $ M.map (v*) m' where
  m' = unDist m

instance (Show k,Show a) => Show(Dist k a) where
  show(Dist x) = show(x)

instance (Num k) => AM.OrdMonad (Dist k) where
   ordReturn x = singleton 1 x
   m `ordBind`  f = undefined--mMap (\(x,v) -> multiply v (f x)) (assocs m)

