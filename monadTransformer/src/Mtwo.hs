{-# LANGUAGE InstanceSigs #-}
module Mtwo where

import Control.Monad 
import Control.Applicative
import System.IO



-- data M2 t a = M2 (t (Either a a), t (Either a a))
newtype M2 t a = M2 {runM2 :: (t (Either a a), t (Either a a)) }
instance (Functor m) => Functor (M2 m) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     fmap f (M2 m) = M2 $ (fmap.fmap) f' m where
         f'(a,b) = (f a, f b)

    --  fmap = undefined
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