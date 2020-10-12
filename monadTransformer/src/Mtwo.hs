{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

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

data M2' t a =  M2' (Square( t a)) --deriving Show

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
data M2 t a = M2(Square (t ( Twice a)))

instance (Show a) => Show (M2 [] a) where
     show(M2 x) = "(M2 " ++ show (x) ++ ")"

instance (Show a) => Show (M2 Maybe a) where
     show(M2 x) = "(M2 " ++ show (x) ++ ")"

instance (Functor t) => Functor(M2 t) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     -- fmap = undefined
     fmap f (M2 t) = M2 $ (fmap.fmap.fmap) f t

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure = return
     -- M2 f <*> M2 t = M2 $ (<*>) <$> f <*> t
     (<*>) = ap
     
instance (Monad t) => Monad (M2 t) where
     return :: a -> M2 t a
     return = pure
     (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     (>>=)= undefined
     -- M2 x >>= f =(f =<< M2 x) 
     -- M2 x >>= M2 f a = M2 $ (fmap.fmap) f x 

-- \\ -- \\ -- \\ -- \\ --
--Examples

a = M2 $ Pair(([First 1], [Second 2]))
aux = M2 $ Pair(([First (+2)], [Second (+2)]))
plusTwoAp = aux <*> a
-- (f <<= ) = f#
-- x >>= f = f# x