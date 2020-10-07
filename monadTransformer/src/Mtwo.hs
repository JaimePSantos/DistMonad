{-# LANGUAGE InstanceSigs #-}
module Mtwo where

import Control.Monad 
import Control.Applicative
import System.IO


newtype M2Aux t a = M2Aux {runM2Aux :: (t a, t a)} deriving Show

newtype EitherAux a = EitherAux (Either a a) deriving Show
newtype PairAux a = PairAux ((a,a)) deriving Show



instance Functor EitherAux where
     fmap f (EitherAux(Right a)) = EitherAux(Right( f a))
     fmap f (EitherAux(Left a)) = EitherAux(Left( f a)) 
     
instance Functor PairAux where
     fmap f (PairAux (a,b)) =  PairAux(f a, f b)

instance (Functor t) => Functor (M2Aux t) where
     fmap f (M2Aux t) = M2Aux $ (fmap) f' t where
          f' a = (f a, f a)
-- \ \ ---

newtype M2 t a = M2 {runM2 :: (t (Either a a), t (Either a a)) }

instance (Functor t) => Functor (M2 t) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     -- fmap f (M2 t) = M2 $ (fmap.fmap) f t

     fmap = undefined

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure  = return
     (<*>) = ap    

instance (Monad t) => Monad (M2 t) where
     (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     (>>=)  = undefined    

{-instance Show (M2 t a) where
    show t a = "("++show(t)++"Et"-}

test:: Either a b -> Either a b
test = id


bla :: Int -> M2 t a
bla = undefined