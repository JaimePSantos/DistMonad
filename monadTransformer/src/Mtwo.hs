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

-- data Square a = Square(a,a) deriving Show
-- data SquareTwice a = SquareTwice(Square(Twice a)) deriving Show

-- instance Functor Square where
--      fmap f (Square(a,b)) = Square $ (f a, f b)

-- instance Functor SquareTwice where
--      fmap f (SquareTwice(Square(First a,First b))) = SquareTwice $ Square (First $ f a,  First $ f b)
--      fmap f (SquareTwice(Square(First a,Second b))) =SquareTwice $ Square (First $ f a,  Second $ f b)
--      fmap f (SquareTwice(Square(Second a,First b))) = SquareTwice $ Square (Second $ f a,  First $ f b)
--      fmap f (SquareTwice(Square(Second a,Second b))) =SquareTwice $ Square (Second $ f a,  Second $ f b)

--Helper structures
data Twice a = First a | Second a deriving Show

data SquareTwice a = SquareTwice((Twice a, Twice a)) deriving Show

data M2 t a = M2 ( SquareTwice( t a)) deriving Show


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

--SquareTwice   
instance Functor SquareTwice where
     fmap f (SquareTwice(First a,First b)) = SquareTwice (First $ f a,  First $ f b)
     fmap f (SquareTwice(First a,Second b)) =SquareTwice  (First $ f a,  Second $ f b)
     fmap f (SquareTwice(Second a,First b)) = SquareTwice (Second $ f a,  First $ f b)
     fmap f (SquareTwice(Second a,Second b)) =SquareTwice (Second $ f a,  Second $ f b)

instance Applicative SquareTwice where
     pure = return
     (SquareTwice(First f,First g)) <*> (SquareTwice(First a,First b)) = SquareTwice (First $ f a,  First $ g b)
     (SquareTwice(First f,Second g)) <*> (SquareTwice(First a,Second b)) = SquareTwice (First $ f a,  Second $ g b)
     (SquareTwice(Second f,First g)) <*> (SquareTwice(Second a,First b)) = SquareTwice (Second $ f a,  First $ g b)
     (SquareTwice(Second f,Second g)) <*> (SquareTwice(Second a,Second b)) = SquareTwice (Second $ f a,  Second $ g b)

     (SquareTwice(First f,First g)) <*> _ = undefined
     (SquareTwice(First f,Second g)) <*> _ = undefined
     (SquareTwice(Second f,First g)) <*> _ = undefined
     (SquareTwice(Second f,Second g)) <*> _ = undefined

instance Monad SquareTwice where
     return = pure
     (>>=) = undefined


-- \\ -- \\ -- \\ -- \\ --
--M2
instance (Functor t) => Functor(M2 t ) where
     fmap :: (a -> b) -> (M2 t a -> M2 t b)
     fmap f (M2 t) = M2 $ (fmap.fmap) f t

instance (Monad t) => Applicative (M2 t) where
     pure :: a -> M2 t a
     pure = M2 . pure .pure
     M2 f <*> M2 t = M2 $ (<*>) <$> f <*> t
     -- (<*>) = ap

instance (Monad t) => Monad (M2 t) where
     return :: a -> M2 t a
     return = pure
     (>>=) :: M2 t a -> (a -> M2 t b) -> M2 t b
     (>>=)= undefined


-- \\ -- \\ -- \\ -- \\ --
--Examples

a = M2 $ SquareTwice (First [1],Second [2])
b = M2 $ SquareTwice (First (Just 1),Second Nothing)
c = M2 $ SquareTwice (First [1],Second [2])

plusTwo = (+2) 
plusTwo' = (+2) 

aux = M2 $(SquareTwice(First[(+2),(+2)],First[(+2)]))

plusTwo'' = aux <*> a 