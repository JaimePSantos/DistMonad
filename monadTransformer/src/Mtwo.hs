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

-- newtype EitherAux a = EitherAux (Either a a) deriving Show
-- newtype PairAux a = PairAux ((a,a)) deriving Show

data Twice a = First a | Second a deriving Show
data Square a = Square(a,a) deriving Show

data SquareTwice a = SquareTwice(Square(Twice a)) deriving Show

data M2Aux t a = M2Aux(SquareTwice(t a) ) deriving Show

-- data M2Aux t a = M2Aux(t (Twice a), t (Twice a))
-- data M2Aux t a = M2Aux(Square(Twice( t a)))

     
instance Functor Twice where
     fmap f (First a) = First( f a)
     fmap f (Second a) = Second( f a) 
     
-- instance Functor Square where
     -- fmap f (Pair(a,b)) = Pair $ (f a, f b)

instance Functor SquareTwice where
     fmap f (SquareTwice(Square(First a,First b))) = SquareTwice $ Square (First $ f a,  First $ f b)
     fmap f (SquareTwice(Square(First a,Second b))) =SquareTwice $ Square (First $ f a,  Second $ f b)
     fmap f (SquareTwice(Square(Second a,First b))) = SquareTwice $ Square (Second $ f a,  First $ f b)
     fmap f (SquareTwice(Square(Second a,Second b))) =SquareTwice $ Square (Second $ f a,  Second $ f b)

-- instance (Functor t) => Functor(M2Aux t a) where
     -- fmap f M2Aux(SquareTwice(Square(t(First a),t(First b)))) = undefined

     -- fmap f ((First a,Second b)) = Pair (First $ f a,  Second $ f b)
     -- fmap f ((Second a,First b)) = Pair (Second $ f a,  First $ f b)
     -- fmap f ((Second a,Second b)) = Pair (Second $ f a,  Second $ f b)

-- nao estou a conseguir definir o funtor quando a monad esta dentro de um tuplo!
-- instance (Functor t) => Functor (M2Aux t) where
     -- fmap f (M2Aux t) = M2Aux $ (fmap.fmap) f t

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
