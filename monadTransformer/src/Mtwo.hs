{-# LANGUAGE InstanceSigs #-}
module Mtwo where

import Control.Monad 
import Control.Applicative
import System.IO
import Data.Tuple


-- sim, estao certos agora o either e o Pair
-- nao vale a pena e acrescentar construtures so como wrappers, em especial o do Either.
-- podes fazer antes:
-- type Square a = Pair {pi1 :: a, pi2 :: a} 
-- data Twice a = First a | Second a deriving Show

-- e as definições depois ficam mais limpas.

-- O M2 é que ainda ainda nao bem. A ideia á usares o Twice (i.e. EitherAux) e o Square (i.e. PairAux) para definir o M2.


data Twice a = First a | Second a deriving Show
data Square a = Square(a,a) deriving Show
data SquareTwice a = SquareTwice(Square(Twice a)) deriving Show

data M2 t a = M2 ( SquareTwice( t a)) deriving Show

     
instance Functor Twice where
     fmap f (First a) = First( f a)
     fmap f (Second a) = Second( f a) 
     
instance Functor Square where
     fmap f (Square(a,b)) = Square $ (f a, f b)

instance Functor SquareTwice where
     fmap f (SquareTwice(Square(First a,First b))) = SquareTwice $ Square (First $ f a,  First $ f b)
     fmap f (SquareTwice(Square(First a,Second b))) =SquareTwice $ Square (First $ f a,  Second $ f b)
     fmap f (SquareTwice(Square(Second a,First b))) = SquareTwice $ Square (Second $ f a,  First $ f b)
     fmap f (SquareTwice(Square(Second a,Second b))) =SquareTwice $ Square (Second $ f a,  Second $ f b)

instance (Functor t) => Functor(M2 t ) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     fmap f (M2 t) = M2 $ (fmap.fmap) f t

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure  = return
     (<*>) = ap    

instance (Monad t) => Monad (M2 t) where
     (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     (>>=)  = undefined    
