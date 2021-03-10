{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module MapTwo where

import qualified Data.Map as M
import Control.Monad       (liftM, ap,mapM)
import qualified Data.Foldable as Foldable
import qualified AsMonad as AM 
import Data.Ratio
import qualified MapMon as MM

newtype Dist a k = Dist{unDist:: M.Map k a}

empty :: Dist a k 
empty = Dist $ M.empty
mapEmpty = empty

singleton :: a -> k -> Dist a k 
singleton a k = Dist $ M.singleton k a
mapSingleton = singleton 1 'a'

insert :: Ord k => k -> a -> Dist a k -> Dist a k 
insert a k m = Dist $ M.insert a k m' where
  m' = unDist m

fromList :: Ord k => [(k, a)] -> Dist a k 
fromList x = Dist $ M.fromList x 

assocs :: Dist a k -> [(k, a)] 
assocs x =  M.assocs (unDist x)

mMap :: (a -> b) -> Dist a k -> Dist b k 
mMap f x = Dist $ M.map f x' where
  x' = unDist x

multiply :: Num a => a -> Dist a k -> Dist a k
multiply v m = Dist $ M.map (v*) m' where
  m' = unDist m

-- Nao entendi muito bem esta funcao.
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = z `seq` go (f z x) xs

unionWith :: Ord k => (a -> a -> a) -> Dist a k-> Dist a k-> Dist a k
unionWith f m1 m2 = Dist $ M.unionWith f m1' m2' where
  m1' = unDist m1
  m2' = unDist m2 

--TODO: Map do unDist.
unionsWith :: Ord k => (a->a->a) -> [Dist a k] -> Dist a k
unionsWith f ts = foldlStrict (unionWith f) empty ts

instance (Show k,Show a) => Show(Dist a k) where
  show(Dist x) = show(x)

instance (Num k) => MM.OrdRest (Dist k) where
   OrdReturn x = singleton 1 x
   x `OrdBind` f = unionsWith (+) $ map (\(x,v) -> multiply v (f x)) (assocs m)

distExample = MM.OrdReturn 0 :: Dist Rational Int 
distExample2 = insert 1 1 distExample

func :: Int -> Dist Rational Int
func a = fromList $ [(a+1,1%2),(a-1,1%2)]

-- TODO: Fazer uma funcao de medicao. Amplitudes para probablidades.
