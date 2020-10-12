{-# LANGUAGE InstanceSigs #-}
module Mtwo where

import Control.Monad 
import Control.Applicative
import System.IO
import Data.Tuple
import Control.Applicative

-- sim, estao certos agora o either e o Pair
-- nao vale a pena e acrescentar construtures so como wrappers, em especial o do Either.
-- podes fazer antes:
-- type Square a = Pair {pi1 :: a, pi2 :: a} 
-- data Twice a = First a | Second a deriving Show

-- e as definições depois ficam mais limpas.

-- O M2 é que ainda ainda nao bem. A ideia á usares o Twice (i.e. EitherAux) e o Square (i.e. PairAux) para definir o M2.

--Helper structures
data Twice a = First a | Second a deriving Show
data Square a = Pair(a,a) deriving Show
data Square' a =Pair'{pi1 :: a, pi2 :: a} deriving Show

-- data TTwice t a = TTwice(t(Twice a)) deriving

data M2' t a =  M2' (Square( t a)) deriving Show
data M2 t a = M2(Square (t ( Twice a))) --deriving Show


-- instance ((Show(t a))) => Show(M2 t a) where
     -- show (M2 x) = "(M2 " ++ show (x) ++ ")"
-- dificuldades a implementar o show
-- erro:  Could not deduce (Show (t (Twice a))) arising from a use of `show'

--Twice
instance Functor Twice where
     fmap f (First a) = First( f a)
     fmap f (Second a) = Second( f a) 

instance Applicative Twice where
     pure a = First a
     First f <*> First a = First(f a)
     Second f <*> Second a = Second(f a) 
     Second f <*> First a = undefined
     First f <*> Second a = undefined
     --duvida em como emparelhar elementos diferentes do tipo
     -- Second f <*> First a = First(f a)
     -- First f <*> Second a = Second(f a)

instance Monad Twice where
     return = pure
     (>>=) = undefined

--Square

instance Functor Square where
     fmap f (Pair(a,b) )= Pair(f a, f b)

instance Applicative Square where
     pure  = return
     Pair(f,g) <*> Pair(a,b) = Pair(f a, g b)

instance Monad Square where
     return = pure
     (>>=) = undefined

--M2


-- \\ -- \\ -- \\ -- \\ --
--Examples

-- a = M2 $ Pair()

-- (f <<= ) = f#
-- x >>= f = f# x