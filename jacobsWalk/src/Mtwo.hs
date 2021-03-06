{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Mtwo where

import Control.Monad(join,ap)
import Control.Applicative
import System.IO
import Data.Tuple
import Control.Applicative
import Data.Functor.Classes
--import qualified Numeric.Probability.Distribution as Dist
--import Numeric.Probability.Distribution ((??), (?=<<), )
import Vector
import Map 
import qualified AsMonad as AM
--import MapTwo

--Helper structures
data Twice a  = In1 a | In2 a deriving (Show,Eq ,Ord)      -- this is the functor T(A) = A + A with injections In1 and In2
data Square a = Pair(a,a) deriving Show 
data Square' a =Pair' {pi1 :: a, pi2 :: a}  --this is the functor S(A) = A x A with projections pi1 and pi2

data M2 t a  = M2 {unM2 :: Square  (t (Twice a)) }
data M2' t a = M2'{unM2':: Square' (t (Twice a)) }

instance (Show a, Show b) => Show(M2 (Vec a) b) where
     show(M2 x) = "M2 ( " ++ show(x) ++ ")" 

instance (Show a, Show b) => Show(M2 (Dist a) b) where
     show(M2 x) = "M2 ( " ++ show(x) ++ ")" 

instance (Show a, Show b, Ord b) => Show(M2 (AsMonDist a) b) where
     show(M2 x) = "M2 ( " ++ show(x) ++ ")" 


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

--M2
instance (Functor t) => Functor(M2 t) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     fmap f (M2 t) = M2 $ (fmap.fmap.fmap) f t

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure = return
     (<*>) = ap

sharp ::(Monad t)=> (a->M2 t b)->(M2 t a -> M2 t b)
sharp f = M2. fmap(join1.fmap(eitherT(first1.f') (second1.f'))).unM2 where
     f' = unM2.f

instance (Monad t) => Monad (M2 t) where
     return :: a -> M2 t a
     return = M2. splitS (fmap(In1).return) (fmap(In2).return)
     (>>=) x f  = sharp f x

--Examples
examplefunc a= M2(Pair ([In1 (a-1),In2 (a+1)],[In1(a-1) ,In2 (a+1)]))
exampleWalk = return 0 :: M2 [] Int

vector1 :: Vec Double Int
vector1 = return 0 :: Vec Double Int
vector2 = return 1 :: Vec Double Int

