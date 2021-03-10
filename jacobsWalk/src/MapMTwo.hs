{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module MapMTwo where

import Control.Monad(join,ap,liftM)
import System.IO
import Data.Tuple
import Control.Applicative
import Data.Functor.Classes
--import qualified Numeric.Probability.Distribution as Dist
--import Numeric.Probability.Distribution ((??), (?=<<), )
import Vector
import Map
import MapTwo
import qualified AsMonad as AM
 
 --Helper structures
data Twice a  = In1 a | In2 a deriving (Show,Eq ,Ord)     
data Square a = Pair(a,a) deriving Show
data Square' a =Pair' {pi1 :: a, pi2 :: a} 
 
data MapM2 t a  = MapM2 {unMapM2 :: Square  (t (Twice a)) }
data MapM2' t a = MapM2'{unMapM2':: Square' (t (Twice a)) }

instance (Show a, Show b) => Show(MapM2 (Vec a) b) where
     show(MapM2 x) = "MapM2 ( " ++ show(x) ++ ")"

instance (Show a, Show b) => Show(MapM2 (Dist a ) b) where
     show(MapM2 x) = "MapM2 ( " ++ show(x) ++ ")"

--Twice
instance Functor Twice where
     fmap f (In1 a) = In1( f a)
     fmap f (In2 a) = In2( f a)

unTwice (In1 a) = a
unTwice (In2 a) = a

eitherT :: (a->c) -> (a->c) ->(Twice a -> c)
eitherT f _ (In1 a) = f a
eitherT _ g (In2 a) = g a

twice2Either :: Twice a -> Either a a
twice2Either( In1 a )= Left a
twice2Either( In2 a) = Right a

either2Twice :: Either a a -> Twice a
either2Twice (Left a) = In1 a
either2Twice (Right b) = In2 b

--Square
instance Functor Square where
     fmap f (Pair(a,b) )= Pair(f a, f b)

instance Applicative Square where
     pure  = return
     Pair(f,g) <*> Pair(a,b) = Pair(f a, g b)

instance Monad Square where
     return = pure
     (>>=) = undefined

splitS :: (c->a) -> (c->a) -> (c->Square a)
splitS f g a = Pair(f a, g a)

square2Pair ::Square a-> (a,a)
square2Pair( Pair(a,b)) = (a,b)

join1 ::(Monad m) => m(m a) -> m a
join1 x = x >>= id

first1 :: Square a -> a
first1 (Pair(a,b)) = a

second1 :: Square a -> a
second1 (Pair(a,b)) = b

--joinMap ::(AM.OrdMonad m) => m(m a) -> m a
--joinMap x = x >>= id
--
--sharpMap ::(AM.OrdMonad t)=> (a->MapM2 t b)->(MapM2 t a -> MapM2 t b)
--sharpMap f = MapM2. fmap(joinMap.(fmap)(eitherT(first1.f') (second1.f'))).unMapM2 where
--     f' = unMapM2.f
--
--instance (Functor t) => Functor(AM.OrdMonad t) where
--  fmap = liftM
--
--instance (AM.OrdMonad t) => AM.OrdMonad (MapM2 t) where
--     ordReturn = MapM2. splitS (fmap(In1).(AM.ordReturn)) (fmap(In2).(AM.ordReturn))
--     x `ordBind` f  = sharpMap f x

--TODO: Reescrever com OrdMonad?
--MapM2
--instance (Functor t) => Functor(MapM2 t) where
--     fmap :: (a -> b) -> (MapM2 t a -> MapM2 t b)
--     fmap f (MapM2 t) = MapM2 $ (fmap.fmap.fmap) f t
--
--instance (Monad t) => Applicative (MapM2 t) where
--     pure :: a -> MapM2 t a
--     pure = return
--     (<*>) = ap
--
--sharp ::(Monad t)=> (a->MapM2 t b)->(MapM2 t a -> MapM2 t b)
--sharp f = MapM2. fmap(join1.fmap(eitherT(first1.f') (second1.f'))).unMapM2 where
--     f' = unMapM2.f
--

--instance (Monad t) => Monad (MapM2 t) where
--     return :: a -> MapM2 t a
--     return = MapM2. splitS (fmap(In1).return) (fmap(In2).return)
--     (>>=) x f  = sharp f x
--
--Examples
--examplefuncMapM2 a= MapM2(Pair ([In1 (a-1),In2 (a+1)],[In1(a-1) ,In2 (a+1)]))
--exampleWalkMapM2 = return 0 :: MapM2 [] Int
--
--vector1 :: Vec Double Int
--vector1 = return 0 :: Vec Double Int
--vector2 = return 1 :: Vec Double Int


