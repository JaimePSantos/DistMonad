{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Map where

import qualified Data.Map as M
import Control.Monad       (liftM, ap,mapM)
import qualified Data.Foldable as Foldable
import qualified AsMonad as AM 
import Data.Ratio
import Data.Complex 
import qualified Data.Semiring as SM

newtype Dist a k = Dist{unDist:: M.Map k a}
type AsMonDist a = AM.AsMonad(Dist a ) 

instance (Show a, Show k,Ord k) => Show(AsMonDist a k) where
   show(x) =  show(AM.unEmbed(x)) 

toListAM :: Ord k => AsMonDist a k -> [(k,a)] 
toListAM x = M.toList . unDist . AM.unEmbed $ x


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

fromListAM ::(Num a, Ord k) => [(k, a)] -> AsMonDist a k 
fromListAM x = AM.Embed $ Dist $ M.fromList x 

assocs :: Dist a k -> [(k, a)] 
assocs x =  M.assocs (unDist x)

mMap :: (a -> b) -> Dist a k -> Dist b k 
mMap f x = Dist $ M.map f x' where
  x' = unDist x

mMapAM ::(Num b, Num a, Ord k) => (a -> b) -> AsMonDist a k -> AsMonDist b k
mMapAM f x = AM.Embed . mMap f . AM.unEmbed $ x 

multiply :: Num a => a -> Dist a k -> Dist a k
multiply v m = Dist $ M.map (v*) m' where
  m' = unDist m

realFunc x =(realPart(abs(x)^2))

getProbs (Dist a) = mMap (realFunc) (Dist a) 

getProbsAM (a) = mMapAM (realFunc) (a) 

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

instance (Num k) => AM.OrdMonad (Dist k) where
   ordReturn x = singleton 1 x
   m `ordBind`  f = unionsWith (+) $ map (\(x,v) -> multiply v (f x)) (assocs m)

--instance SM.Semiring(Dist k) where
--   zero = undefined
--   one = undefined
--   plus = undefined
--   times = undefined
--   fromNatural = undefined 

distExample = AM.ordReturn 0 :: Dist Rational Int 
distExample2 = insert 1 1 distExample

func :: Int -> Dist Rational Int
func a = fromList $ [(a+1,1%2),(a-1,1%2)]

-- TODO: Fazer uma funcao de medicao. Amplitudes para probablidades.
--
--
--newtype Complex2 a = C2{unC2::Complex a} deriving ( Eq, Floating,Fractional )
--
--instance  (RealFloat a) => Num (Complex2 a)  where
--    {-# SPECIALISE instance Num (Complex2 Float) #-}
--    {-# SPECIALISE instance Num (Complex2 Double) #-}
--    C2(x:+y) + C2(x':+y')   =  C2 $ sqrt((x:+y)^2 + (x':+y')^2)
--    C2(x:+y) * C2(x':+y')   =  C2 $ (x*x'-y*y') :+ (x*y'+y*x')
--    fromInteger n  = C2 $ fromInteger n :+ 0
--
--instance Show(a) => Show(Complex2 a) where
--  show(C2 x) = show(x)
--
--ble :: Complex2 Float
--ble = C2 $ 1 :+ 1 
--ble2 = C2 $ 1 :+ 1
--ble3 = ble + ble2
